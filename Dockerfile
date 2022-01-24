# syntax=docker/dockerfile:1
FROM ubuntu:18.04

# Install system dependencies and Python packages
RUN apt-get update -y && \
    apt-get -y install libimage-exiftool-perl

FROM continuumio/miniconda3:4.10.3p0-alpine

COPY . .

# Get the arguments from the docker-compose environment
ARG PORT
EXPOSE ${PORT}

# This allows us to use the arguments during runtime
RUN conda env create -f environment.yml

# Use the conda environment we created to run the application.
# The docker execution process run conda activate semantic-search, since the lifetime of the environment would only be for the single command.
# Instead, we'll use the conda run to run the application.
# Use 0.0.0.0 to explicitly set the host ip for the service on the container. https://pythonspeed.com/articles/docker-connection-refused/
# Use sh -c to start a shell in order to use environment variables in CMD.
ENTRYPOINT ["conda", "run", "--no-capture-output", "--name", "semantic-search", \
    "python3", "-m", "src.main"]

    # "python3", "-m", "src.main", "-c=${CONFIG_FILE}", "-vv" ,"--host=${HOST}, "--port=${PORT}"]

# CMD ["sh", "-c", "echo ${CONFIG_FILE}", "echo ${HOST}", "echo ${PORT}"]
