# Use Nvidia's latest Ubuntu 22.04 image as the base image
FROM nvidia/cuda:12.2.0-devel-ubuntu22.04

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
ENTRYPOINT [ "gunicorn", "-c", "gunicorn-config.py", "src.khoj.main:app" ]
