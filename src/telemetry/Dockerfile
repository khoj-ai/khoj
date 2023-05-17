# Get Base Image
FROM tiangolo/uvicorn-gunicorn:python3.11-slim
LABEL org.opencontainers.image.source https://github.com/debanjum/khoj

# Install Telemetry Server Dependencies
COPY requirements.txt /tmp/requirements.txt
RUN pip install --no-cache-dir -r /tmp/requirements.txt

# Copy Application
COPY telemetry.py /app/main.py
