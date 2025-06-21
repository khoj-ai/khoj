import asyncio
import logging
import re
from contextlib import AsyncExitStack
from typing import List, Optional

from mcp import ClientSession, StdioServerParameters, Tool
from mcp.client.sse import sse_client
from mcp.client.stdio import stdio_client

# Set up logger
logger = logging.getLogger(__name__)


class MCPClient:
    def __init__(self, base_uri: str, api_key: Optional[str] = None):
        self.base_uri = base_uri.rstrip("/")
        self.api_key = api_key
        self.session = None
        self.is_connected = asyncio.Event()
        self.exit_stack = AsyncExitStack()
        self.tools: Optional[List[Tool]] = None

    async def connect_to_sse_server(self):
        """Connect to an SSE MCP server."""
        logger.debug(f"Connecting to SSE MCP server at {self.base_uri}")

        self._streams_context = sse_client(url=self.base_uri)
        streams = await self._streams_context.__aenter__()

        self._session_context = ClientSession(*streams)
        self.session = await self._session_context.__aenter__()

        # Initialize
        await self.session.initialize()
        self.is_connected.set()

        # List available tools
        response = await self.session.list_tools()
        self.tools = response.tools
        logger.info(
            f"Connected to SSE MCP Server at {self.base_uri}. Available tools: {[tool.name for tool in self.tools]}"
        )

    async def connect_to_stdio_server(self):
        """Connect to a stdio MCP server."""
        command = self.base_uri.split(" ", 1)[0]  # Get the script/command from the URI
        args = self.base_uri.split(" ")[1:]  # Get the rest of the arguments, if any

        server_params = StdioServerParameters(command=command, args=args, env=None)
        logger.debug(f"Connecting to stdio MCP server with command: {command} and args: {args}")

        # Start the server
        stdio_transport = await self.exit_stack.enter_async_context(stdio_client(server_params))
        self.stdio, self.writer = stdio_transport
        self.session = await self.exit_stack.enter_async_context(ClientSession(self.stdio, self.writer))

        await self.session.initialize()
        self.is_connected.set()

        # List available tools
        response = await self.session.list_tools()
        self.tools = response.tools
        logger.info(f"Connected to stdio MCP Server. Available tools: {[tool.name for tool in self.tools]}")

    async def connect(self):
        """Connect to an MCP server (either stdio or SSE)."""
        # Check if the input is a URL (for SSE server)
        url_pattern = re.compile(r"^https?://")

        if url_pattern.match(self.base_uri):
            # It's a URL, connect to SSE server
            await self.connect_to_sse_server()
        else:
            # It's a script path, connect to stdio server
            await self.connect_to_stdio_server()

    async def list_tools(self) -> List[Tool]:
        """Get the list of available tools from the MCP server."""
        if not self.is_connected.is_set():
            raise RuntimeError("Not connected to MCP server. Call connect() first.")
        if self.tools is None:
            response = await self.session.list_tools()
            self.tools = response.tools
        return self.tools

    async def call_tool(self, name: str, arguments: dict):
        """Call a tool by name with the provided arguments."""
        # Run validation checks
        if not self.is_connected.is_set():
            raise RuntimeError("Not connected to MCP server. Call connect() first.")
        if not self.tools:
            await self.get_tools()
        # find the tool by name
        tool = next((t for t in self.tools if t.name == name), None)
        if not tool:
            raise ValueError(f"Tool '{name}' not found on the server.")

        # Call the tool
        raw_response = await self.session.call_tool(name, arguments)

        # Return formatted results
        tool_results = [result.text for result in raw_response.content]
        return tool_results

    async def cleanup(self):
        """Close connection and clean up resources."""
        try:
            # Close exit stack for stdio connections
            await self.exit_stack.aclose()

            # Close SSE connections - ignore cancel scope errors
            if hasattr(self, "_session_context") and self._session_context:
                try:
                    await self._session_context.__aexit__(None, None, None)
                except (RuntimeError, asyncio.CancelledError) as e:
                    if "cancel scope" in str(e) or isinstance(e, asyncio.CancelledError):
                        logger.debug(f"Ignoring cancel scope error during session cleanup: {e}")
                    else:
                        raise

            if hasattr(self, "_streams_context") and self._streams_context:
                try:
                    await self._streams_context.__aexit__(None, None, None)
                except (RuntimeError, asyncio.CancelledError) as e:
                    if "cancel scope" in str(e) or isinstance(e, asyncio.CancelledError):
                        logger.debug(f"Ignoring cancel scope error during streams cleanup: {e}")
                    else:
                        raise

        except Exception as e:
            logger.debug(f"Error during cleanup: {e}")
        finally:
            # Clear the connection event and references
            self.is_connected.clear()
            if hasattr(self, "_session_context"):
                self._session_context = None
            if hasattr(self, "_streams_context"):
                self._streams_context = None
