# syntax=docker/dockerfile:1
FROM ubuntu:18.04

# Install system dependencies and Python packages
RUN apt-get update -y && \
    apt-get -y install libimage-exiftool-perl

FROM continuumio/miniconda3

COPY . /src
WORKDIR /src

COPY environment.yml .
COPY config.yml .

RUN conda env create -f environment.yml

EXPOSE 5000
COPY . .
# CMD python3 -m main -c=config.yml -vv
CMD ["conda", "run", "--name", "semantic-search", "python3", "-m", "main", "-c=config.yml", "-vv"]
