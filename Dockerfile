# syntax=docker/dockerfile:1
FROM ubuntu:kinetic
LABEL org.opencontainers.image.source https://github.com/khoj-ai/khoj

# Install System Dependencies
RUN apt update -y && \
    apt -y install python3-pip python3-pyqt6 git

# Install Application
RUN pip install --no-cache-dir git+https://github.com/khoj-ai/khoj.git

# Run the Application
# There are more arguments required for the application to run,
# but these should be passed in through the docker-compose.yml file.
ARG PORT
EXPOSE ${PORT}
ENTRYPOINT ["khoj"]
