# syntax=docker/dockerfile:1
FROM continuumio/miniconda3:4.12.0

# Install system dependencies.
RUN apt-get update -y && \
    apt-get -y install libimage-exiftool-perl

# Add the local code to the /app directory and set it to be the working directory.
# Since we mount the /app directory as a volume in docker-compose.yml, this
# allows us to automatically update the code in the Docker image when it's changed.
ADD . /app
WORKDIR /app

# Get the arguments from the docker-compose environment.
ARG PORT
EXPOSE ${PORT}

# Create the conda environment.
RUN conda env create -f config/environment.yml

# Use the conda environment we created to run the application.
# To enable the conda env, we cannot simply RUN `conda activate khoj`,
# since each RUN command in a Dockerfile is a separate bash shell. 
# The environment would not carry forward.
# Instead, we'll use `conda run` to run the application.
# There are more arguments required for the script to run, 
# but these should be passed in through the docker-compose.yml file.
ENTRYPOINT ["conda", "run", "--no-capture-output", "--name", "khoj", \
    "python3", "-m", "src.main"]
