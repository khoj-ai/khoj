import argparse
import logging
import os
import sqlite3
from typing import Dict, List

import uvicorn
from dotenv import load_dotenv
from fastapi import FastAPI, HTTPException
from posthog import Posthog

# Initialize Global App Variables
app = FastAPI()
sqlfile = "data/khoj.sqlite"
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)
load_dotenv()
posthog = Posthog(project_api_key=os.getenv("POSTHOG_API_KEY"), host="https://app.posthog.com")


@app.post("/v1/telemetry")
def v1_telemetry(telemetry_data: List[Dict[str, str]]):
    # Throw exception if no telemetry data received in POST request body
    if len(telemetry_data) == 0:
        error_message = "Post body is empty. It should contain some telemetry data"
        logger.error(error_message)
        raise HTTPException(status_code=500, detail=error_message)

    # POST request to khoj posthog server
    try:
        for row in telemetry_data:
            posthog.capture(row["server_id"], "api_request", row)
    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail="Could not POST equest to new khoj telemetry server. Contact developer to get this fixed.",
        )

    # Insert recieved telemetry data into SQLite db
    logger.info(f"Insert row into telemetry table at {sqlfile}: {telemetry_data}")
    with sqlite3.connect(sqlfile) as conn:
        cur = conn.cursor()

        # Create a table if it doesn't exist
        cur.execute(
            """CREATE TABLE IF NOT EXISTS usage (id INTEGER PRIMARY KEY, time TIMESTAMP, type TEXT, server_id TEXT, os TEXT, api TEXT, client TEXT, server_version TEXT)"""
        )

        # Log telemetry data
        for item in telemetry_data:
            cur.execute(
                "INSERT INTO usage (time, type, server_id, os, api, client, server_version) VALUES (?, ?, ?, ?, ?, ?, ?)",
                (
                    item["timestamp"],
                    item["telemetry_type"],
                    item["server_id"],
                    item["os"],
                    item.get("api"),
                    item.get("client"),
                    item.get("server_version", None),
                ),
            )
        # Commit the changes
        conn.commit()

    return {"status": "ok", "message": "Logged usage telemetry"}


if __name__ == "__main__":
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Start Khoj Telemetry Server")
    parser.add_argument("--host", default="127.0.0.1", type=str, help="I.P of telemetry server")
    parser.add_argument("--port", "-p", default=80, type=int, help="Port of telemetry server")
    args = parser.parse_args()

    # Start Application Server
    uvicorn.run(app, host=args.host, port=args.port, log_level="debug")
