# syntax=docker/dockerfile:1
FROM ubuntu:kinetic
LABEL org.opencontainers.image.source https://github.com/khoj-ai/khoj

# Install System Dependencies
RUN apt update -y && \
    apt -y install python3-pip python3-pyqt6

# Install Python Dependencies
RUN pip install --upgrade pip && \
    pip install --upgrade --pre khoj-assistant

# Run the Application
# There are more arguments required for the application to run,
# but these should be passed in through the docker-compose.yml file.
ARG PORT
EXPOSE ${PORT}
ENTRYPOINT ["khoj"]
