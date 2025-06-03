ARG PYTHON_VERSION=3.10
FROM mcr.microsoft.com/devcontainers/python:${PYTHON_VERSION}

# Install Node.js and Yarn
RUN curl -fsSL https://deb.nodesource.com/setup_lts.x | bash - && \
    apt-get install -y nodejs

# Setup working directory
WORKDIR /workspace

# --- Python Server App Dependencies ---
# Create Python virtual environment
RUN python3 -m venv /opt/venv
# Add venv to PATH for subsequent RUN commands and for the container environment
ENV PATH="/opt/venv/bin:${PATH}"

# Copy files required for Python dependency installation.
COPY pyproject.toml README.md ./

# Setup python environment
    # Use the pre-built llama-cpp-python, torch cpu wheel
ENV PIP_EXTRA_INDEX_URL="https://download.pytorch.org/whl/cpu https://abetlen.github.io/llama-cpp-python/whl/cpu" \
    # Avoid downloading unused cuda specific python packages
    CUDA_VISIBLE_DEVICES="" \
    # Use static version to build app without git dependency
    VERSION=0.0.0
# Install Python dependencies from pyproject.toml in editable mode
RUN sed -i "s/dynamic = \\[\"version\"\\]/version = \"$VERSION\"/" pyproject.toml && \
    pip install --no-cache-dir ".[dev]"

# --- Web App Dependencies ---
# Copy web app manifest files
COPY src/interface/web/package.json src/interface/web/yarn.lock /tmp/web/

# Install web app dependencies
# note: yarn will be available from the "features" in devcontainer.json
RUN yarn install --cwd /tmp/web --cache-folder /opt/yarn-cache

# The .venv and node_modules are now populated in the image.
# The rest of the source code will be mounted by VS Code from your local checkout,
# overlaying any files copied here if they are part of the workspace mount.
