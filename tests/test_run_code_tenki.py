import base64
import os

import pytest

from khoj.processor.tools.run_code import execute_tenki


@pytest.mark.asyncio
@pytest.mark.skipif(not os.getenv("TENKI_API_KEY"), reason="Set TENKI_API_KEY to run the Tenki sandbox test")
async def test_execute_tenki_runs_code_with_file_io():
    """execute_tenki runs code in a Tenki sandbox and returns stdout + output files.

    Live test against Tenki Cloud; skipped unless TENKI_API_KEY is set.
    """
    csv = "name,value\na,10\nb,20\nc,30\n"
    code = (
        "import pandas as pd\n"
        'df = pd.read_csv("data.csv")\n'
        'total = int(df["value"].sum())\n'
        'print("TOTAL:", total)\n'
        'with open("summary.txt", "w") as f:\n'
        '    f.write(f"total={total}")\n'
    )
    input_files = [{"filename": "data.csv", "b64_data": base64.b64encode(csv.encode()).decode()}]

    result = await execute_tenki(code, input_files)

    assert result["success"], result.get("std_err")
    assert "TOTAL: 60" in result["std_out"]
    output_names = {f["filename"] for f in result["output_files"]}
    assert "summary.txt" in output_names
    summary = next(f for f in result["output_files"] if f["filename"] == "summary.txt")
    assert summary["b64_data"].strip() == "total=60"


@pytest.mark.asyncio
async def test_execute_sandboxed_python_dispatches_to_tenki(monkeypatch):
    """execute_sandboxed_python routes to Tenki when only Tenki is enabled (no live call)."""
    from unittest.mock import AsyncMock

    import khoj.processor.tools.run_code as run_code

    monkeypatch.setattr(run_code, "is_e2b_code_sandbox_enabled", lambda: False)
    monkeypatch.setattr(run_code, "is_tenki_code_sandbox_enabled", lambda: True)
    sentinel = {"code": "print(1)", "success": True, "std_out": "1\n", "std_err": "", "output_files": []}
    mock_tenki = AsyncMock(return_value=sentinel)
    monkeypatch.setattr(run_code, "execute_tenki", mock_tenki)

    result = await run_code.execute_sandboxed_python("print(1)", [])

    assert result is sentinel
    mock_tenki.assert_awaited_once()


@pytest.mark.asyncio
async def test_execute_sandboxed_python_prefers_e2b_over_tenki(monkeypatch):
    """E2B keeps priority over Tenki when both are enabled."""
    from unittest.mock import AsyncMock

    import khoj.processor.tools.run_code as run_code

    monkeypatch.setattr(run_code, "is_e2b_code_sandbox_enabled", lambda: True)
    monkeypatch.setattr(run_code, "is_tenki_code_sandbox_enabled", lambda: True)
    e2b_result = {"code": "print(1)", "success": True, "std_out": "", "std_err": "", "output_files": []}
    mock_e2b = AsyncMock(return_value=e2b_result)
    mock_tenki = AsyncMock()
    monkeypatch.setattr(run_code, "execute_e2b", mock_e2b)
    monkeypatch.setattr(run_code, "execute_tenki", mock_tenki)

    result = await run_code.execute_sandboxed_python("print(1)", [])

    assert result is e2b_result
    mock_e2b.assert_awaited_once()
    mock_tenki.assert_not_awaited()
