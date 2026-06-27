import logging
from contextlib import AsyncExitStack
from typing import Optional

from mcp import ClientSession, StdioServerParameters
from mcp.client.sse import sse_client
from mcp.client.stdio import stdio_client
from mcp.types import AudioContent, ImageContent, TextContent

logger = logging.getLogger(__name__)


class MCPClient:
    """
    A client for interacting with MCP servers.
    Establishes a session with the server and provides methods to get and run tools.
    Supports both stdio and sse communication methods.
    """

    def __init__(self, name: str, path: str, api_key: Optional[str] = None):
        self.name = name
        self.path = path
        self.api_key = api_key
        self.session: Optional[ClientSession] = None
        self.exit_stack = AsyncExitStack()

    async def connect(self):
        """
        Connect to the MCP server based on the specified path.
        Uses stdio for local scripts and SSE for remote servers.
        """
        if self.path.startswith("http://") or self.path.startswith("https://"):
            # Path is a URL, so use SSE to connect to the server
            await self._connect_to_sse_server()
        else:
            # Path is a script, so use stdio to connect to the server
            await self._connect_to_stdio_server()

    async def get_tools(self):
        """
        Retrieve the list of tools available on the MCP server.
        """
        # Ensure connection is established
        if not self.session:
            await self.connect()

        tools_response = await self.session.list_tools()
        return tools_response.tools

    async def run_tool(self, tool_name: str, input_data: dict):
        """
        Run a specified tool on the MCP server with the given input data.
        """
        # Ensure connection is established
        if not self.session:
            await self.connect()

        result = await self.session.call_tool(tool_name, input_data)

        # Process result content based on its type
        if len(result.content) > 0 and isinstance(result.content[0], TextContent):
            return [item.text for item in result.content]
        elif len(result.content) > 0 and isinstance(result.content[0], AudioContent):
            return [{"data": item.data, "format": item.mimeType} for item in result.content]
        elif len(result.content) > 0 and isinstance(result.content[0], ImageContent):
            return [{"data": item.data, "format": item.mimeType} for item in result.content]
        return result.content

    async def _connect_to_sse_server(self):
        """
        Connect to the MCP server using Server-Sent Events (SSE).
        """
        self._streams_context = sse_client(url=self.path)
        streams = await self._streams_context.__aenter__()

        self._session_context = ClientSession(*streams)
        self.session = await self._session_context.__aenter__()

        # Initialize
        await self.session.initialize()

    async def _connect_to_stdio_server(self):
        """
        Connect to the MCP server using stdio communication.
        """
        is_python = False
        is_javascript = False
        command = None
        args = [self.path]

        # Determine if the server is a file path or npm package
        if self.path.startswith("@") or "/" not in self.path:
            # Assume it's an npm package
            is_javascript = True
            command = "npx"
        else:
            # It's a file path
            is_python = self.path.endswith(".py")
            is_javascript = self.path.endswith(".js")
            if not (is_python or is_javascript):
                raise ValueError("Server script must be a .py, .js file or npm package.")

            command = "python" if is_python else "node"

        server_params = StdioServerParameters(command=command, args=args, env=None)

        logger.debug(f"Connecting to stdio MCP server with command: {command} and args: {args}")

        # Start the server
        stdio_transport = await self.exit_stack.enter_async_context(stdio_client(server_params))
        self.stdio, self.writer = stdio_transport
        self.session = await self.exit_stack.enter_async_context(ClientSession(self.stdio, self.writer))

        await self.session.initialize()

    async def close(self):
        """
        Close the MCP client session and clean up resources.
        """
        await self.exit_stack.aclose()
        if hasattr(self, "_session_context") and self._session_context:
            await self._session_context.__aexit__(None, None, None)
        if hasattr(self, "_streams_context") and self._streams_context:
            await self._streams_context.__aexit__(None, None, None)
