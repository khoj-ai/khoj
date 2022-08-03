# syntax=docker/dockerfile:1
FROM python:3.10-slim-bullseye
LABEL org.opencontainers.image.source https://github.com/debanjum/khoj

# Install System Dependencies
RUN apt-get update -y && \
    apt-get -y install libimage-exiftool-perl

# Copy Application to Container
COPY . /app
WORKDIR /app

# Install Python Dependencies
RUN pip install --upgrade pip && \
    pip install --upgrade .

# Run the Application
# There are more arguments required for the application to run,
# but these should be passed in through the docker-compose.yml file.
ARG PORT
EXPOSE ${PORT}
ENTRYPOINT ["khoj"]
