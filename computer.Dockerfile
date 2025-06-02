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
ENV USERNAME=operator
ENV HOME=/home/$USERNAME
RUN useradd -m -s /bin/bash -d $HOME -g $USERNAME $USERNAME && echo "${USERNAME} ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER $USERNAME
WORKDIR $HOME

# Setup Python
RUN git clone https://github.com/pyenv/pyenv.git ~/.pyenv && \
    cd ~/.pyenv && src/configure && make -C src && cd .. && \
    echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.bashrc && \
    echo 'command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bashrc && \
    echo 'eval "$(pyenv init -)"' >> ~/.bashrc
ENV PYENV_ROOT="$HOME/.pyenv"
ENV PATH="$PYENV_ROOT/bin:$PATH"
ENV PYENV_VERSION_MAJOR=3
ENV PYENV_VERSION_MINOR=11
ENV PYENV_VERSION_PATCH=6
ENV PYENV_VERSION=$PYENV_VERSION_MAJOR.$PYENV_VERSION_MINOR.$PYENV_VERSION_PATCH
RUN eval "$(pyenv init -)" && \
    pyenv install $PYENV_VERSION && \
    pyenv global $PYENV_VERSION && \
    pyenv rehash
ENV PATH="$HOME/.pyenv/shims:$HOME/.pyenv/bin:$PATH"

# Install Python Packages
RUN python3 -m pip install --no-cache-dir \
    pyautogui \
    Pillow \
    pyperclip \
    pygetwindow

# Setup VNC
RUN x11vnc -storepasswd secret /home/operator/.vncpass

ARG WIDTH=1024
ARG HEIGHT=768
ARG DISPLAY_NUM=99
ENV WIDTH=$WIDTH
ENV HEIGHT=$HEIGHT
ENV DISPLAY_NUM=$DISPLAY_NUM
ENV DISPLAY=":$DISPLAY_NUM"

# Expose VNC on port 5900
# run Xvfb, x11vnc, Xfce (no login manager)
EXPOSE 5900
CMD ["/bin/sh", "-c", "    export XDG_RUNTIME_DIR=/run/user/$(id -u); \
    mkdir -p $XDG_RUNTIME_DIR && chown $USERNAME:$USERNAME $XDG_RUNTIME_DIR && chmod 0700 $XDG_RUNTIME_DIR; \
    Xvfb $DISPLAY -screen 0 ${WIDTH}x${HEIGHT}x24 -dpi 96 -auth /home/$USERNAME/.Xauthority >/dev/null 2>&1 & \
    sleep 1; \
    xauth add $DISPLAY . $(mcookie); \
    x11vnc -display $DISPLAY -forever -rfbauth /home/$USERNAME/.vncpass -listen 0.0.0.0 -rfbport 5900 >/dev/null 2>&1 & \
    eval $(dbus-launch --sh-syntax) && \
    startxfce4 & \
    sleep 2 && echo 'Container running!' && \
    tail -f /dev/null "]
