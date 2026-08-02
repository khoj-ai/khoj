import pytest

from khoj.processor.operator.operator_environment_computer import ComputerEnvironment


def _stub_executor(record):
    async def _execute(command, new=True):
        record["command"] = command
        return {"success": True, "output": "ran", "error": None}

    return _execute


@pytest.mark.asyncio
async def test_local_terminal_refused_by_default(monkeypatch):
    monkeypatch.delenv("KHOJ_OPERATOR_ALLOW_LOCAL_SHELL", raising=False)
    env = ComputerEnvironment(provider="local")
    record = {}
    monkeypatch.setattr(env, "_execute_shell_command", _stub_executor(record))

    result = await env._run_terminal_command("touch /tmp/pwned")

    assert result["success"] is False
    assert "disabled" in result["error"].lower()
    assert "command" not in record


@pytest.mark.asyncio
async def test_local_terminal_runs_when_allowed(monkeypatch):
    monkeypatch.setenv("KHOJ_OPERATOR_ALLOW_LOCAL_SHELL", "true")
    env = ComputerEnvironment(provider="local")
    record = {}
    monkeypatch.setattr(env, "_execute_shell_command", _stub_executor(record))

    result = await env._run_terminal_command("echo hi")

    assert result["success"] is True
    assert record["command"] == "echo hi"


@pytest.mark.asyncio
async def test_docker_terminal_not_gated(monkeypatch):
    monkeypatch.delenv("KHOJ_OPERATOR_ALLOW_LOCAL_SHELL", raising=False)
    env = ComputerEnvironment(provider="docker")
    record = {}
    monkeypatch.setattr(env, "_execute_shell_command", _stub_executor(record))

    result = await env._run_terminal_command("echo hi")

    assert result["success"] is True
    assert record["command"] == "echo hi"
