ARG PYTHON_VERSION=3.12
FROM mcr.microsoft.com/devcontainers/python:${PYTHON_VERSION}

# Install UV and Bun
RUN curl -fsSL https://bun.sh/install | bash && mv /root/.bun/bin/bun /usr/local/bin/bun
COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/
RUN uv python pin $PYTHON_VERSION
# create python virtual environment
RUN uv venv /opt/venv --python $PYTHON_VERSION --seed
# Add venv to PATH for subsequent RUN commands and for the container environment
ENV PATH="/opt/venv/bin:$PATH"
# Tell pip, uv to use this virtual environment
ENV VIRTUAL_ENV="/opt/venv"
ENV UV_PROJECT_ENVIRONMENT="/opt/venv"

# Setup working directory
WORKDIR /workspaces/khoj

# --- Python Server App Dependencies ---
# Copy files required for Python dependency installation.
COPY pyproject.toml README.md ./

# Setup python environment
    # Use the pre-built torch cpu wheel
ENV UV_INDEX="https://download.pytorch.org/whl/cpu" \
    UV_INDEX_STRATEGY="unsafe-best-match" \
    # Avoid downloading unused cuda specific python packages
    CUDA_VISIBLE_DEVICES="" \
    # Use static version to build app without git dependency
    VERSION=0.0.0 \
    # Use embedded db
    USE_EMBEDDED_DB="True" \
    PGSERVER_DATA_DIR="/opt/khoj_db"
# Install Python dependencies from pyproject.toml in editable mode
RUN sed -i "s/dynamic = \\[\"version\"\\]/version = \"$VERSION\"/" pyproject.toml && \
    uv sync --all-extras && \
    # Save the lock file generated with correct Linux platform wheels
    cp uv.lock /opt/uv.lock.linux && \
    chown -R vscode:vscode /opt/venv

# --- Web App Dependencies ---
# Copy web app manifest files
COPY src/interface/web/package.json src/interface/web/bun.lock /opt/khoj_web/

# Install web app dependencies
RUN cd /opt/khoj_web && bun install && chown -R vscode:vscode .

# The .venv and node_modules are now populated in the image.
# The rest of the source code will be mounted by VS Code from your local checkout,
# overlaying any files copied here if they are part of the workspace mount.
