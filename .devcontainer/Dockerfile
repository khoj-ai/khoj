ARG PYTHON_VERSION=3.12
FROM mcr.microsoft.com/devcontainers/python:${PYTHON_VERSION}

# Install UV and Bun
RUN curl -fsSL https://bun.sh/install | bash
COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/
RUN uv python pin $PYTHON_VERSION
# create python virtual environment
RUN uv venv /opt/venv --python $PYTHON_VERSION --seed
# Add venv, deno to PATH for subsequent RUN commands and for the container environment
ENV PATH="/opt/venv/bin:/root/.bun/bin:$PATH"

# Setup working directory
WORKDIR /workspace

# --- Python Server App Dependencies ---
# Copy files required for Python dependency installation.
COPY pyproject.toml README.md ./

# Setup python environment
    # Use the pre-built torch cpu wheel
ENV PIP_EXTRA_INDEX_URL="https://download.pytorch.org/whl/cpu" \
    # Avoid downloading unused cuda specific python packages
    CUDA_VISIBLE_DEVICES="" \
    # Use static version to build app without git dependency
    VERSION=0.0.0
# Install Python dependencies from pyproject.toml in editable mode
RUN sed -i "s/dynamic = \\[\"version\"\\]/version = \"$VERSION\"/" pyproject.toml && \
    uv sync --all-extras && \
    chown -R vscode:vscode /opt/venv

# --- Web App Dependencies ---
# Copy web app manifest files
COPY src/interface/web/package.json src/interface/web/bun.lock /tmp/web/

# Install web app dependencies
RUN cd /tmp/web && bun install

# The .venv and node_modules are now populated in the image.
# The rest of the source code will be mounted by VS Code from your local checkout,
# overlaying any files copied here if they are part of the workspace mount.
