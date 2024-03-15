# syntax=docker/dockerfile:1
FROM ubuntu:jammy
LABEL org.opencontainers.image.source https://github.com/khoj-ai/khoj

# Install System Dependencies
RUN apt update -y && apt -y install python3-pip swig

WORKDIR /app

# Install Application
COPY pyproject.toml .
COPY README.md .
ARG VERSION=0.0.0
RUN sed -i "s/dynamic = \\[\"version\"\\]/version = \"$VERSION\"/" pyproject.toml && \
    pip install --no-cache-dir .

# Copy Source Code
COPY . .

# Set the PYTHONPATH environment variable in order for it to find the Django app.
ENV PYTHONPATH=/app/src:$PYTHONPATH

# Run the Application
# There are more arguments required for the application to run,
# but these should be passed in through the docker-compose.yml file.
ARG PORT
EXPOSE ${PORT}
ENTRYPOINT ["python3", "src/khoj/main.py"]
