# syntax=docker/dockerfile:1
FROM ubuntu:jammy
LABEL org.opencontainers.image.source https://github.com/khoj-ai/khoj

# Install System Dependencies
RUN apt update -y && apt -y install python3-pip git

WORKDIR /app

# Install Application
COPY pyproject.toml .
COPY README.md .
RUN sed -i 's/dynamic = \["version"\]/version = "0.0.0"/' pyproject.toml && \
    pip install --no-cache-dir .

# Copy Source Code
COPY . .

RUN apt install vim -y

# Set the PYTHONPATH environment variable in order for it to find the Django app.
ENV PYTHONPATH=/app/src:$PYTHONPATH

# Run the Application
# There are more arguments required for the application to run,
# but these should be passed in through the docker-compose.yml file.
ARG PORT
EXPOSE ${PORT}
ENTRYPOINT ["gunicorn", "--bind", "0.0.0.0:42110", "src.khoj.main:app", "--workers", "3", "-k", "uvicorn.workers.UvicornWorker", "--timeout", "600", "--keep-alive", "120", "--log-level", "info"]
