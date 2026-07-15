import asyncio
import base64
import datetime
import logging
import mimetypes
import os
import re
import shlex
import uuid
from pathlib import Path
from typing import Any, Callable, List, NamedTuple, Optional

import aiohttp
import httpx
from asgiref.sync import sync_to_async
from tenacity import (
    before_sleep_log,
    retry,
    retry_if_exception_type,
    stop_after_attempt,
    wait_random_exponential,
)

from khoj.database.adapters import AgentAdapters, FileObjectAdapters
from khoj.database.models import Agent, ChatMessageModel, FileObject, KhojUser, UserMemory
from khoj.processor.conversation import prompts
from khoj.processor.conversation.utils import (
    ChatEvent,
    clean_code_python,
    construct_chat_history,
)
from khoj.routers.helpers import send_message_to_model_wrapper
from khoj.utils.helpers import (
    is_e2b_code_sandbox_enabled,
    is_none_or_empty,
    is_tenki_code_sandbox_enabled,
    timer,
    truncate_code_context,
)
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)


SANDBOX_URL = os.getenv("KHOJ_TERRARIUM_URL")
DEFAULT_E2B_TEMPLATE = "pmt2o0ghpang8gbiys57"
HOME_DIR = "/home/user"


class GeneratedCode(NamedTuple):
    code: str
    input_files: List[FileObject]


async def run_code(
    instructions: str,
    conversation_history: List[ChatMessageModel],
    context: str,
    location_data: LocationData,
    user: KhojUser,
    send_status_func: Optional[Callable] = None,
    query_images: List[str] = None,
    query_files: str = None,
    relevant_memories: List[UserMemory] = None,
    agent: Agent = None,
    sandbox_url: str = SANDBOX_URL,
    tracer: dict = {},
):
    # Generate Code
    if send_status_func:
        async for event in send_status_func(f"**Generate code snippet** for {instructions}"):
            yield {ChatEvent.STATUS: event}
    try:
        with timer("Chat actor: Generate programs to execute", logger):
            generated_code = await generate_python_code(
                instructions,
                conversation_history,
                context,
                location_data,
                user,
                query_images,
                agent,
                tracer,
                query_files,
                relevant_memories,
            )
    except Exception as e:
        raise ValueError(f"Failed to generate code for {instructions} with error: {e}")

    # Prepare Input Data
    input_data = []
    for f in generated_code.input_files:
        input_data.append(
            {
                "filename": f.file_name,
                "b64_data": base64.b64encode(f.raw_text.encode("utf-8")).decode("utf-8"),
            }
        )

    # Run Code
    if send_status_func:
        async for event in send_status_func("**Running code snippet**"):
            yield {ChatEvent.STATUS: event}
    try:
        with timer("Chat actor: Execute generated program", logger, log_level=logging.INFO):
            result = await execute_sandboxed_python(generated_code.code, input_data, sandbox_url)
            code = result.pop("code")
            cleaned_result = truncate_code_context({"cleaned": {"results": result}})["cleaned"]["results"]
            logger.info(f"Executed Code\n----\n{code}\n----\nResult\n----\n{cleaned_result}\n----")
            yield {instructions: {"code": code, "results": result}}
    except asyncio.TimeoutError as e:
        # Call the sandbox_url/stop GET API endpoint to stop the code sandbox
        error = f"Failed to run code for {instructions} with Timeout error: {e}"
        try:
            await aiohttp.ClientSession().get(f"{sandbox_url}/stop", timeout=5)
        except Exception as e:
            error += f"\n\nFailed to stop code sandbox with error: {e}"
        raise ValueError(error)
    except Exception as e:
        raise ValueError(f"Failed to run code for {instructions} with error: {e}")


async def generate_python_code(
    instructions: str,
    chat_history: List[ChatMessageModel],
    context: str,
    location_data: LocationData,
    user: KhojUser,
    query_images: list[str] = None,
    agent: Agent = None,
    tracer: dict = {},
    query_files: str = None,
    relevant_memories: List[UserMemory] = None,
) -> GeneratedCode:
    location = f"{location_data}" if location_data else "Unknown"
    username = prompts.user_name.format(name=user.get_full_name()) if user.get_full_name() else ""
    chat_history_str = construct_chat_history(chat_history)

    utc_date = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%d")
    personality_context = (
        prompts.personality_context.format(personality=agent.personality) if agent and agent.personality else ""
    )

    # add sandbox specific context like available packages
    has_sandbox_network = is_e2b_code_sandbox_enabled() or is_tenki_code_sandbox_enabled()
    sandbox_context = prompts.e2b_sandbox_context if has_sandbox_network else prompts.terrarium_sandbox_context
    personality_context = f"{sandbox_context}\n{personality_context}"
    network_access_context = "" if has_sandbox_network else "**NO** "

    code_generation_prompt = prompts.python_code_generation_prompt.format(
        instructions=instructions,
        chat_history=chat_history_str,
        context=context,
        has_network_access=network_access_context,
        home_dir=HOME_DIR,
        current_date=utc_date,
        location=location,
        username=username,
        personality_context=personality_context,
    )

    agent_chat_model = AgentAdapters.get_agent_chat_model(agent, user) if agent else None

    response = await send_message_to_model_wrapper(
        code_generation_prompt,
        query_files=query_files,
        query_images=query_images,
        relevant_memories=relevant_memories,
        fast_model=False,
        agent_chat_model=agent_chat_model,
        user=user,
        tracer=tracer,
    )

    # Extract python code wrapped in markdown code blocks from the response
    code_blocks = re.findall(r"```(?:python)?\n(.*?)```", response.text, re.DOTALL)

    if not code_blocks:
        raise ValueError("No Python code blocks found in response")

    # Join multiple code blocks with newlines and strip any leading/trailing whitespace
    code = "\n".join(code_blocks).strip()

    if not isinstance(code, str) or is_none_or_empty(code):
        raise ValueError

    # Infer user files required in sandbox based on user file paths mentioned in code
    input_files: List[FileObject] = []
    user_files = await sync_to_async(set)(FileObjectAdapters.get_all_file_objects(user))
    for user_file in user_files:
        if user_file.file_name in code:
            # Replace references to full file path used in code with just the file basename to ease reference in sandbox
            file_basename = os.path.basename(user_file.file_name)
            code = code.replace(user_file.file_name, file_basename)
            user_file.file_name = file_basename
            input_files.append(user_file)

    return GeneratedCode(code, input_files)


@retry(
    retry=(
        retry_if_exception_type(aiohttp.ClientError)
        | retry_if_exception_type(aiohttp.ClientTimeout)
        | retry_if_exception_type(asyncio.TimeoutError)
        | retry_if_exception_type(ConnectionError)
        | retry_if_exception_type(httpx.RemoteProtocolError)
        | retry_if_exception_type(httpx.NetworkError)
        | retry_if_exception_type(httpx.TimeoutException)
    ),
    wait=wait_random_exponential(min=1, max=5),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
async def execute_sandboxed_python(code: str, input_data: list[dict], sandbox_url: str = SANDBOX_URL) -> dict[str, Any]:
    """
    Takes code to run as a string and calls the terrarium API to execute it.
    Returns the result of the code execution as a dictionary.

    Reference data i/o format based on Terrarium example client code at:
    https://github.com/cohere-ai/cohere-terrarium/blob/main/example-clients/python/terrarium_client.py
    """
    cleaned_code = clean_code_python(code)
    if is_e2b_code_sandbox_enabled():
        try:
            return await execute_e2b(cleaned_code, input_data)
        except ImportError:
            pass
    if is_tenki_code_sandbox_enabled():
        try:
            return await execute_tenki(cleaned_code, input_data)
        except ImportError:
            pass
    return await execute_terrarium(cleaned_code, input_data, sandbox_url)


async def execute_e2b(code: str, input_files: list[dict]) -> dict[str, Any]:
    """Execute code and handle file I/O in e2b sandbox"""
    from e2b_code_interpreter import AsyncSandbox

    sandbox = await AsyncSandbox.create(
        api_key=os.getenv("E2B_API_KEY"),
        template=os.getenv("E2B_TEMPLATE", DEFAULT_E2B_TEMPLATE),
        timeout=120,
        request_timeout=30,
    )

    try:
        # Upload input files in parallel
        upload_tasks = [
            sandbox.files.write(file["filename"], base64.b64decode(file["b64_data"])) for file in input_files
        ]
        await asyncio.gather(*upload_tasks)

        # Note stored files before execution to identify new files created during execution
        E2bFile = NamedTuple("E2bFile", [("name", str), ("path", str)])
        original_files = {E2bFile(f.name, f.path) for f in await sandbox.files.list(HOME_DIR, depth=1)}

        # Execute code from main.py file
        execution = await sandbox.run_code(code=code, timeout=60)

        # Collect output files
        output_files = []
        image_file_ext = {".png", ".jpeg", ".jpg", ".svg"}

        # Identify new files created during execution
        new_files = set(E2bFile(f.name, f.path) for f in await sandbox.files.list(HOME_DIR, depth=1)) - original_files

        # Read newly created files in parallel
        def read_format(f):
            return "bytes" if Path(f.name).suffix in image_file_ext else "text"

        download_tasks = [sandbox.files.read(f.path, format=read_format(f), request_timeout=30) for f in new_files]
        downloaded_files = await asyncio.gather(*download_tasks)
        for f, content in zip(new_files, downloaded_files):
            if isinstance(content, bytes):
                # Binary files like PNG - encode as base64
                b64_data = base64.b64encode(content).decode("utf-8")
            elif Path(f.name).suffix in image_file_ext:
                # Ignore image files as they are extracted from execution results below for inline display
                b64_data = base64.b64encode(content).decode("utf-8")
            else:
                # Text files - encode utf-8 string as base64
                b64_data = content
            output_files.append({"filename": f.name, "b64_data": b64_data})

        # Collect output files from execution results
        # Respect ordering of output result types to disregard text output associated with images
        downloaded_datatypes = {f["filename"].split(".")[-1] for f in output_files}
        output_result_types = ["png", "jpeg", "svg", "text", "markdown", "json"]
        for result in execution.results:
            if getattr(result, "chart", None):
                continue
            for result_type in output_result_types:
                if b64_data := getattr(result, result_type, None):
                    if result_type in downloaded_datatypes:
                        break
                    filename = f"{HOME_DIR}/{uuid.uuid4()}.{result_type}"
                    output_files.append({"filename": filename, "b64_data": b64_data})
                    break

        # collect logs
        success = not execution.error and not execution.logs.stderr
        stdout = "\n".join(execution.logs.stdout)
        errors = "\n".join(execution.logs.stderr)
        if execution.error:
            errors = f"{execution.error}\n{errors}"

        return {
            "code": code,
            "success": success,
            "std_out": stdout,
            "std_err": errors,
            "output_files": output_files,
        }
    except Exception as e:
        return {
            "code": code,
            "success": False,
            "std_err": f"Sandbox failed to execute code: {str(e)}",
            "output_files": [],
        }


async def execute_tenki(code: str, input_files: list[dict]) -> dict[str, Any]:
    """Execute code and handle file I/O in a Tenki Cloud sandbox.

    Uses only stable Tenki features (ephemeral exec + file I/O). The default
    Tenki image ships python3 but not the data packages, so a common set is
    installed on start. Set KHOJ_TENKI_IMAGE to point at a prebaked image to
    skip that install.
    """
    from tenki_sandbox import AsyncClient

    workdir = HOME_DIR
    image_file_ext = {".png", ".jpeg", ".jpg", ".svg"}
    client = AsyncClient()  # reads TENKI_API_KEY from the environment
    sandbox = None
    try:
        # The SDK has no "current project" default, so resolve the first
        # workspace/project on the account (override via env if needed).
        identity = await client.who_am_i()
        workspace = next(
            (w for w in identity.workspaces if w.id == os.getenv("KHOJ_TENKI_WORKSPACE_ID")),
            identity.workspaces[0],
        )
        project = next(
            (p for p in workspace.projects if p.id == os.getenv("KHOJ_TENKI_PROJECT_ID")),
            workspace.projects[0],
        )

        sandbox = await client.create(
            name=f"khoj-code-{uuid.uuid4().hex[:8]}",
            workspace_id=workspace.id,
            project_id=project.id,
            image=os.getenv("KHOJ_TENKI_IMAGE") or None,
            allow_outbound=True,
            max_duration=300,
        )

        # Prepare the working dir and install common data packages on the stock image
        setup = (
            f'sudo mkdir -p {shlex.quote(workdir)} && sudo chown -R "$(whoami)" {shlex.quote(workdir)} && '
            "if ! command -v pip3 >/dev/null 2>&1; then sudo apt-get update -y && sudo apt-get install -y python3-pip; fi && "
            "pip3 install --quiet --break-system-packages matplotlib pandas numpy scipy || true"
        )
        await sandbox.exec("bash", "-lc", setup, timeout=300)

        # Upload input files (base64 over stdin to avoid arg length limits)
        for input_file in input_files:
            in_path = shlex.quote(f"{workdir}/{input_file['filename']}")
            await sandbox.exec("bash", "-lc", f"base64 -d > {in_path}", input=input_file["b64_data"], timeout=60)

        # Note existing files so we can detect ones created during execution
        list_cmd = f"cd {shlex.quote(workdir)} && ls -1p 2>/dev/null | grep -v '/$' || true"
        before = await sandbox.exec("bash", "-lc", list_cmd, timeout=30)
        original_files = {n for n in (before.stdout or b"").decode(errors="replace").splitlines() if n}

        # Write and run the generated code
        main_path = shlex.quote(f"{workdir}/main.py")
        await sandbox.exec(
            "bash",
            "-lc",
            f"base64 -d > {main_path}",
            input=base64.b64encode(code.encode("utf-8")).decode("utf-8"),
            timeout=60,
        )
        execution = await sandbox.exec("python3", "main.py", cwd=workdir, timeout=120)
        stdout = (execution.stdout or b"").decode(errors="replace")
        stderr = (execution.stderr or b"").decode(errors="replace")
        success = execution.exit_code == 0 and not stderr

        # Collect newly created output files
        after = await sandbox.exec("bash", "-lc", list_cmd, timeout=30)
        current_files = {n for n in (after.stdout or b"").decode(errors="replace").splitlines() if n}
        output_files = []
        for name in sorted(current_files - original_files):
            if name == "main.py":
                continue
            out_path = shlex.quote(f"{workdir}/{name}")
            read = await sandbox.exec("bash", "-lc", f"base64 -w0 {out_path}", timeout=60)
            b64_data = (read.stdout or b"").decode(errors="replace").strip()
            if not b64_data:
                continue
            if Path(name).suffix in image_file_ext:
                # Binary/image files: keep as base64
                output_files.append({"filename": name, "b64_data": b64_data})
            else:
                # Text files: store decoded utf-8, matching the e2b/terrarium contract
                try:
                    output_files.append({"filename": name, "b64_data": base64.b64decode(b64_data).decode("utf-8")})
                except (UnicodeDecodeError, ValueError):
                    output_files.append({"filename": name, "b64_data": b64_data})

        return {
            "code": code,
            "success": success,
            "std_out": stdout,
            "std_err": stderr,
            "output_files": output_files,
        }
    except Exception as e:
        return {
            "code": code,
            "success": False,
            "std_err": f"Sandbox failed to execute code: {str(e)}",
            "output_files": [],
        }
    finally:
        if sandbox is not None:
            try:
                await sandbox.terminate()
            except Exception:
                pass
        try:
            await client.close()
        except Exception:
            pass


async def execute_terrarium(
    code: str,
    input_data: list[dict],
    sandbox_url: str,
) -> dict[str, Any]:
    """Execute code using Terrarium sandbox"""
    headers = {"Content-Type": "application/json"}
    data = {"code": code, "files": input_data}
    async with aiohttp.ClientSession() as session:
        async with session.post(sandbox_url, json=data, headers=headers, timeout=30) as response:
            if response.status == 200:
                result: dict[str, Any] = await response.json()
                result["code"] = code
                # Store decoded output files
                result["output_files"] = result.get("output_files", [])
                for output_file in result["output_files"]:
                    # Decode text files as UTF-8
                    if mimetypes.guess_type(output_file["filename"])[0].startswith("text/") or Path(
                        output_file["filename"]
                    ).suffix in [".org", ".md", ".json"]:
                        output_file["b64_data"] = base64.b64decode(output_file["b64_data"]).decode("utf-8")
                return result
            else:
                return {
                    "code": code,
                    "success": False,
                    "std_err": f"Failed to execute code with {response.status}",
                    "output_files": [],
                }
