FROM ubuntu:jammy

LABEL org.opencontainers.image.source https://github.com/khoj-ai/khoj

# Install System Dependencies
RUN apt update -y && apt -y install python3-pip libsqlite3-0 ffmpeg libsm6 libxext6

WORKDIR /app

# Install Application
COPY pyproject.toml .
COPY README.md .
ARG VERSION=0.0.0
RUN sed -i "s/dynamic = \\[\"version\"\\]/version = \"$VERSION\"/" pyproject.toml && \
    TMPDIR=/home/cache/ pip install --cache-dir=/home/cache/ -e .[prod]

# Copy Source Code
COPY . .

# Set the PYTHONPATH environment variable in order for it to find the Django app.
ENV PYTHONPATH=/app/src:$PYTHONPATH

# Run the Application
# There are more arguments required for the application to run,
# but these should be passed in through the docker-compose.yml file.
ARG PORT
EXPOSE ${PORT}
ENTRYPOINT [ "gunicorn", "-c", "gunicorn-config.py", "src.khoj.main:app" ]
