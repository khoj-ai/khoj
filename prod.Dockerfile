FROM ubuntu:jammy

LABEL org.opencontainers.image.source="https://github.com/khoj-ai/khoj"

# Install System Dependencies
RUN apt update -y && apt -y install python3-pip libsqlite3-0 ffmpeg libsm6 libxext6 swig curl

# Install Node.js and Yarn using nvm in a single RUN instruction
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash - && \
    export NVM_DIR="$HOME/.nvm" && \
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" && \
    nvm install 20 && \
    npm install --global yarn

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

# Go to the directory src/interface/web and export the built Next.js assets
WORKDIR /app/src/interface/web
RUN bash -c "yarn cache clean && yarn install --verbose && yarn ciexport"
WORKDIR /app

# Run the Application
# There are more arguments required for the application to run,
# but these should be passed in through the docker-compose.yml file.
ARG PORT
EXPOSE ${PORT}
ENTRYPOINT [ "gunicorn", "-c", "gunicorn-config.py", "src.khoj.main:app" ]
