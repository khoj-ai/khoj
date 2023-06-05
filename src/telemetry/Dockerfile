# Get Base Image
FROM tiangolo/uvicorn-gunicorn:python3.11-slim
LABEL org.opencontainers.image.source https://github.com/debanjum/khoj

# Install Telemetry Server Dependencies
COPY requirements.txt /tmp/requirements.txt
RUN pip install --no-cache-dir -r /tmp/requirements.txt

# Set Environment Variables
RUN --mount=type=secret,id=POSTHOG_API_KEY \
  export POSTHOG_API_KEY=$(cat /run/secrets/POSTHOG_API_KEY) && \
  echo "POSTHOG_API_KEY=$POSTHOG_API_KEY" > /app/.env

# Copy Application
COPY telemetry.py /app/main.py
