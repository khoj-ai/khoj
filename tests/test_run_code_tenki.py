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
