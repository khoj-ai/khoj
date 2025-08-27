FROM ubuntu:24.04
ENV DEBIAN_FRONTEND=noninteractive

# Install System Dependencies
RUN apt update \
    && apt install -y \
        ca-certificates \
        gnupg \
        xfce4 \
        xfce4-goodies \
        x11vnc \
        xvfb \
        xdotool \
        imagemagick \
        x11-apps \
        dbus-x11 \
        sudo \
        python3-pip \
        python3-tk \
        python3-dev \
        build-essential \
        scrot \
        gnome-screenshot \
        net-tools \
        libx11-dev \
        libxext-dev \
        libxtst-dev \
        libxinerama-dev \
        libxmu-dev \
        libxrandr-dev \
        libxfixes-dev \
        software-properties-common \
    && add-apt-repository ppa:mozillateam/ppa && apt update \
    && apt install -y --no-install-recommends \
        # Desktop apps
        firefox-esr \
        libreoffice \
        x11-apps \
        xpdf \
        gedit \
        xpaint \
        tint2 \
        galculator \
        pcmanfm \
        unzip \
        # Terminal apps like file editors, viewers, git, wget/curl etc.
        less \
        nano \
        neovim \
        vim \
        git \
        curl \
        wget \
        procps \
        # Python/pyenv dependencies
        libssl-dev  \
        zlib1g-dev \
        libbz2-dev \
        libreadline-dev \
        libsqlite3-dev \
        libncursesw5-dev \
        xz-utils \
        tk-dev \
        libxml2-dev \
        libxmlsec1-dev \
        libffi-dev \
        liblzma-dev \
    # set default browser
    && update-alternatives --set x-www-browser /usr/bin/firefox-esr \
    && apt-get clean && rm -rf /var/lib/apt/lists/* \
    # remove screen locks, power managers
    && apt remove -y light-locker xfce4-screensaver xfce4-power-manager || true

# Create Computer User
ENV USERNAME=khoj
ENV HOME=/home/$USERNAME
RUN groupadd $USERNAME && \
    useradd -m -s /bin/bash -d $HOME -g $USERNAME $USERNAME && \
    echo "${USERNAME} ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER $USERNAME
WORKDIR $HOME

# Install Python using uv and create a virtual environment
COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/
ENV PYTHON_VERSION=3.11.6
RUN uv python pin $PYTHON_VERSION
RUN uv venv $HOME/.venv --python $PYTHON_VERSION --seed
RUN echo 'export PATH="$HOME/.venv/bin:$PATH"' >> "$HOME/.bashrc"
ENV PATH="$HOME/.venv/bin:$PATH"

# Install Python Packages
RUN uv pip install --no-cache-dir \
    pyautogui \
    Pillow \
    pyperclip \
    pygetwindow

# Setup VNC
RUN x11vnc -storepasswd secret /home/khoj/.vncpass

ARG WIDTH=1024
ARG HEIGHT=768
ARG DISPLAY_NUM=99
ENV WIDTH=$WIDTH
ENV HEIGHT=$HEIGHT
ENV DISPLAY_NUM=$DISPLAY_NUM
ENV DISPLAY=":$DISPLAY_NUM"

# Expose VNC on port 5900
EXPOSE 5900

# Start Virtual Display (Xvfb), Desktop Manager (XFCE) and Remote Viewer (X11 VNC)
CMD ["/bin/sh", "-c", "    \
    # Create and permission XDG_RUNTIME_DIR with sudo \n\
    export XDG_RUNTIME_DIR=/run/user/$(id -u); \
    sudo mkdir -p $XDG_RUNTIME_DIR && \
    sudo chown $(id -u):$(id -g) $XDG_RUNTIME_DIR && \
    sudo chmod 0700 $XDG_RUNTIME_DIR; \
    \
    # Start Virtual Display \n\
    Xvfb $DISPLAY -screen 0 ${WIDTH}x${HEIGHT}x24 -dpi 96 -auth /home/$USERNAME/.Xauthority >/dev/null 2>&1 & \
    sleep 1; \
    xauth add $DISPLAY . $(mcookie); \
    \
    # Start VNC Server \n\
    x11vnc -display $DISPLAY -forever -rfbauth /home/$USERNAME/.vncpass -listen 0.0.0.0 -rfbport 5900 >/dev/null 2>&1 & \
    eval $(dbus-launch --sh-syntax) && \
    startxfce4 & \
    sleep 2 && echo 'Container running!' && \
    tail -f /dev/null "]
