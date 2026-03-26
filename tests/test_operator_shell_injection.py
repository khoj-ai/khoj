"""
Tests for CWE-78: OS Command Injection in ComputerEnvironment.

Verifies that:
1. _execute_shell_command does NOT use shell=True (prevents double-shell interpretation)
2. Text editor actions use Python file I/O instead of shell commands
3. docker_execute uses list-based subprocess (no shell=True)
4. File operations with malicious paths/content cannot inject commands
"""

import asyncio
import importlib
import importlib.util
import os
import sys
import tempfile
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

# --- Direct module loading to avoid heavy __init__.py imports ---

WORKTREE = os.environ.get(
    "WORKTREE",
    os.path.join(
        os.path.dirname(__file__),
        "..",
        "projects",
        "audits",
        "khoj-ai-khoj-worktrees",
        "cwe78-operator-environment-agent-642e",
    ),
)
SRC = os.path.join(WORKTREE, "src") if os.path.isdir(os.path.join(WORKTREE, "src")) else os.path.join(os.path.dirname(__file__), "..", "src")


def _load_module(name, filepath):
    """Load a single Python module by file path, without triggering __init__."""
    spec = importlib.util.spec_from_file_location(name, filepath)
    mod = importlib.util.module_from_spec(spec)
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod


def _ensure_stubs():
    """Create minimal stub modules so the target file can be imported."""
    for pkg in [
        "khoj",
        "khoj.processor",
        "khoj.processor.operator",
        "khoj.utils",
    ]:
        if pkg not in sys.modules:
            mod = type(sys)("types")
            mod.__name__ = pkg
            mod.__package__ = pkg
            mod.__path__ = []
            sys.modules[pkg] = mod

    actions_path = os.path.join(SRC, "khoj", "processor", "operator", "operator_actions.py")
    if os.path.exists(actions_path):
        _load_module("khoj.processor.operator.operator_actions", actions_path)

    base_path = os.path.join(SRC, "khoj", "processor", "operator", "operator_environment_base.py")
    if os.path.exists(base_path):
        _load_module("khoj.processor.operator.operator_environment_base", base_path)

    helpers_stub = type(sys)("types")
    helpers_stub.__name__ = "khoj.utils.helpers"
    helpers_stub.convert_image_to_webp = lambda x: x
    sys.modules["khoj.utils.helpers"] = helpers_stub


_ensure_stubs()

_target_path = os.path.join(SRC, "khoj", "processor", "operator", "operator_environment_computer.py")
_target = _load_module("khoj.processor.operator.operator_environment_computer", _target_path)

ComputerEnvironment = _target.ComputerEnvironment

from khoj.processor.operator.operator_actions import (
    TerminalAction,
    TextEditorCreateAction,
    TextEditorStrReplaceAction,
    TextEditorInsertAction,
    TextEditorViewAction,
)
from khoj.processor.operator.operator_environment_base import EnvState


@pytest.fixture
def env():
    e = ComputerEnvironment(provider="local")
    e.width = 1920
    e.height = 1080
    return e


class TestExecuteShellCommand:
    """Verify _execute_shell_command uses shell=False."""

    @pytest.mark.asyncio
    async def test_local_no_shell_true(self, env):
        """
        _execute_shell_command for local provider must NOT pass shell=True
        to subprocess.run. Instead it should use ['bash', '-c', command].
        """
        with patch("asyncio.to_thread") as mock_to_thread:
            mock_process = MagicMock()
            mock_process.returncode = 0
            mock_process.stdout = "safe"
            mock_process.stderr = ""
            mock_to_thread.return_value = mock_process

            await env._execute_shell_command("echo hello")

            assert mock_to_thread.called
            call_args, call_kwargs = mock_to_thread.call_args
            # shell must not be True
            assert call_kwargs.get("shell", False) is not True, (
                "VULNERABILITY: _execute_shell_command uses shell=True (CWE-78)"
            )
            # Command should be passed as a list with bash -c
            cmd_arg = call_args[1]  # second positional arg to asyncio.to_thread
            assert isinstance(cmd_arg, list), (
                f"Command should be a list, got {type(cmd_arg)}"
            )
            assert cmd_arg[0] == "bash" and cmd_arg[1] == "-c", (
                f"Expected ['bash', '-c', ...], got {cmd_arg[:2]}"
            )


class TestDockerExecuteNoShellTrue:
    """Verify docker_execute uses list-based subprocess, not shell=True."""

    @pytest.mark.asyncio
    async def test_docker_execute_no_shell(self):
        """docker_execute should use a list of args, not shell=True."""
        docker_env = ComputerEnvironment(provider="docker")

        with patch("asyncio.to_thread") as mock_to_thread:
            mock_process = MagicMock()
            mock_process.returncode = 0
            mock_process.stdout = "output"
            mock_process.stderr = ""
            mock_to_thread.return_value = mock_process

            await docker_env.docker_execute("print('hello')")

            assert mock_to_thread.called
            call_args, call_kwargs = mock_to_thread.call_args
            # shell must not be True
            assert call_kwargs.get("shell", False) is not True, (
                "VULNERABILITY: docker_execute uses shell=True (CWE-78)"
            )
            # Command should be a list
            cmd_arg = call_args[1]
            assert isinstance(cmd_arg, list), (
                f"Docker command should be a list, got {type(cmd_arg)}"
            )


class TestTextEditorPythonIO:
    """Verify text editor actions use Python file I/O, not shell commands."""

    @pytest.mark.asyncio
    async def test_text_editor_create_uses_python_io(self, env):
        """
        text_editor_create should use Python file I/O, not shell echo.
        Verify content with shell metacharacters is written literally.
        """
        with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as tmp:
            safe_path = tmp.name

        marker = f"/tmp/pwned_cwe78_create_{os.getpid()}"
        try:
            malicious_content = f"$(touch {marker})"
            action = TextEditorCreateAction(
                path=safe_path,
                file_text=malicious_content,
            )
            with patch.object(env, "get_state", new_callable=AsyncMock) as mock_state:
                mock_state.return_value = EnvState(screenshot=None, height=1080, width=1920)
                await env.step(action)

            assert not os.path.exists(marker), (
                "VULNERABILITY: command substitution in text_editor_create was executed!"
            )

            # The file should contain the literal string
            with open(safe_path) as f:
                content = f.read()
            assert malicious_content in content, (
                f"File content should contain the literal string, got: {content!r}"
            )
        finally:
            for f in [marker, safe_path]:
                if os.path.exists(f):
                    os.unlink(f)

    @pytest.mark.asyncio
    async def test_text_editor_create_path_injection(self, env):
        """Malicious path in text_editor_create should not inject commands."""
        marker = f"/tmp/pwned_create_path_{os.getpid()}"
        safe_file = f"/tmp/safe_create_{os.getpid()}"
        try:
            action = TextEditorCreateAction(
                path=f"{safe_file}'; touch {marker}; echo '",
                file_text="benign content",
            )
            with patch.object(env, "get_state", new_callable=AsyncMock) as mock_state:
                mock_state.return_value = EnvState(screenshot=None, height=1080, width=1920)
                await env.step(action)
            assert not os.path.exists(marker), (
                "VULNERABILITY: path injection in text_editor_create!"
            )
        finally:
            for f in [marker, safe_file]:
                if os.path.exists(f):
                    os.unlink(f)
            # Clean up the file with the weird name if it was created
            weird_path = f"{safe_file}'; touch {marker}; echo '"
            if os.path.exists(weird_path):
                os.unlink(weird_path)

    @pytest.mark.asyncio
    async def test_text_editor_view_path_injection(self, env):
        """Path injection in text_editor_view should not execute commands."""
        marker = f"/tmp/pwned_cwe78_view_{os.getpid()}"

        with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as tmp:
            tmp.write("safe content")
            safe_path = tmp.name

        try:
            action = TextEditorViewAction(
                path=f"{safe_path}'; touch {marker}; echo '",
            )
            with patch.object(env, "get_state", new_callable=AsyncMock) as mock_state:
                mock_state.return_value = EnvState(screenshot=None, height=1080, width=1920)
                await env.step(action)

            assert not os.path.exists(marker), (
                "VULNERABILITY: path injection in text_editor_view!"
            )
        finally:
            for f in [marker, safe_path]:
                if os.path.exists(f):
                    os.unlink(f)

    @pytest.mark.asyncio
    async def test_text_editor_view_reads_file(self, env):
        """text_editor_view should correctly read file contents."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as tmp:
            tmp.write("line1\nline2\nline3\n")
            safe_path = tmp.name

        try:
            action = TextEditorViewAction(path=safe_path)
            with patch.object(env, "get_state", new_callable=AsyncMock) as mock_state:
                mock_state.return_value = EnvState(screenshot=None, height=1080, width=1920)
                result = await env.step(action)

            assert result.error is None
            assert "line1" in result.output
            assert "line2" in result.output
        finally:
            if os.path.exists(safe_path):
                os.unlink(safe_path)

    @pytest.mark.asyncio
    async def test_text_editor_view_range(self, env):
        """text_editor_view with view_range should return only specified lines."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as tmp:
            tmp.write("line1\nline2\nline3\nline4\nline5\n")
            safe_path = tmp.name

        try:
            action = TextEditorViewAction(path=safe_path, view_range=[2, 4])
            with patch.object(env, "get_state", new_callable=AsyncMock) as mock_state:
                mock_state.return_value = EnvState(screenshot=None, height=1080, width=1920)
                result = await env.step(action)

            assert result.error is None
            assert "line2" in result.output
            assert "line4" in result.output
            # Should not include line1 or line5
            assert "line1" not in result.output
            assert "line5" not in result.output
        finally:
            if os.path.exists(safe_path):
                os.unlink(safe_path)

    @pytest.mark.asyncio
    async def test_text_editor_str_replace(self, env):
        """text_editor_str_replace should work correctly via Python I/O."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as tmp:
            tmp.write("hello world\n")
            safe_path = tmp.name

        try:
            action = TextEditorStrReplaceAction(
                path=safe_path,
                old_str="hello",
                new_str="goodbye",
            )
            with patch.object(env, "get_state", new_callable=AsyncMock) as mock_state:
                mock_state.return_value = EnvState(screenshot=None, height=1080, width=1920)
                result = await env.step(action)

            assert result.error is None
            with open(safe_path) as f:
                content = f.read()
            assert "goodbye world" in content
            assert "hello" not in content
        finally:
            if os.path.exists(safe_path):
                os.unlink(safe_path)

    @pytest.mark.asyncio
    async def test_text_editor_insert(self, env):
        """text_editor_insert should work correctly via Python I/O."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as tmp:
            tmp.write("line1\nline2\nline3\n")
            safe_path = tmp.name

        try:
            action = TextEditorInsertAction(
                path=safe_path,
                insert_line=2,
                new_str="inserted line",
            )
            with patch.object(env, "get_state", new_callable=AsyncMock) as mock_state:
                mock_state.return_value = EnvState(screenshot=None, height=1080, width=1920)
                result = await env.step(action)

            assert result.error is None
            with open(safe_path) as f:
                lines = f.readlines()
            # "inserted line" should be at index 2 (after line 2)
            assert "inserted line" in lines[2]
        finally:
            if os.path.exists(safe_path):
                os.unlink(safe_path)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
