## Setup
These are the general setup instructions for Khoj.

- Make sure [python](https://realpython.com/installing-python/) and [pip](https://pip.pypa.io/en/stable/installation/) are installed on your machine
- Check the [Khoj.el Readme](https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs#Setup) to setup Khoj with Emacs<br />
  Its simpler as it can skip the server *install*, *run* and *configure* step below.
- Check the [Khoj Obsidian Readme](https://github.com/khoj-ai/khoj/tree/master/src/interface/obsidian#Setup) to setup Khoj with Obsidian<br />
  Its simpler as it can skip the *configure* step below.

### 1. Install
Run the following command in your terminal to install the Khoj backend.

- On Linux/MacOS
  ```shell
  python -m pip install khoj-assistant
  ```

- On Windows
  ```shell
  py -m pip install khoj-assistant
  ```
For more detailed Windows installation and troubleshooting, see [Windows Install](./windows_install.md).


### 2. Run

Run the following commmand from your terminal to start the Khoj backend and open Khoj in your browser.

```shell
khoj --gui
```

Note: To start Khoj automatically in the background use [Task scheduler](https://www.windowscentral.com/how-create-automated-task-using-task-scheduler-windows-10) on Windows or [Cron](https://en.wikipedia.org/wiki/Cron) on Mac, Linux (e.g with `@reboot khoj`)

### 3. Configure
1. Set `File`, `Folder` and hit `Save` in each Plugins you want to enable for Search on the Khoj config page
2. Add your OpenAI API key to Chat Feature settings if you want to use Chat
3. Click `Configure` and wait. The app will download ML models and index the content for search and (optionally) chat

### 4. Install Interface Plugins (Optional)
Khoj exposes a web interface to search, chat and configure by default.<br />
The optional steps below allow using Khoj from within an existing application like Obsidian or Emacs.

- **Khoj Obsidian**:<br />
[Install](https://github.com/khoj-ai/khoj/tree/master/src/interface/obsidian#2-Setup-Plugin) the Khoj Obsidian plugin

- **Khoj Emacs**:<br />
[Install](https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs#2-Install-Khojel) khoj.el


## Upgrade
### Upgrade Khoj Server
```shell
pip install --upgrade khoj-assistant
```

*Note: To upgrade to the latest pre-release version of the khoj server run below command*
```shell
# Maps to the latest commit on the master branch
pip install --upgrade --pre khoj-assistant
```

### Upgrade Khoj on Emacs
- Use your Emacs Package Manager to Upgrade
- See [khoj.el readme](https://github.com/khoj-ai/khoj/tree/master/src/interface/emacs#Upgrade) for details

### Upgrade Khoj on Obsidian
- Upgrade via the Community plugins tab on the settings pane in the Obsidian app
- See the [khoj plugin readme](https://github.com/khoj-ai/khoj/tree/master/src/interface/obsidian#2-Setup-Plugin) for details

## Uninstall
1. (Optional) Hit `Ctrl-C` in the terminal running the khoj server to stop it
2. Delete the khoj directory in your home folder (i.e `~/.khoj` on Linux, Mac or `C:\Users\<your-username>\.khoj` on Windows)
3. Uninstall the khoj server with `pip uninstall khoj-assistant`
4. (Optional) Uninstall khoj.el or the khoj obsidian plugin in the standard way on Emacs, Obsidian

## Troubleshoot

#### Install fails while building Tokenizer dependency
- **Details**: `pip install khoj-assistant` fails while building the `tokenizers` dependency. Complains about Rust.
- **Fix**: Install Rust to build the tokenizers package. For example on Mac run:
    ```shell
    brew install rustup
    rustup-init
    source ~/.cargo/env
    ```
- **Refer**: [Issue with Fix](https://github.com/khoj-ai/khoj/issues/82#issuecomment-1241890946) for more details

#### Search starts giving wonky results
- **Fix**: Open [/api/update?force=true](http://localhost:42110/api/update?force=true)[^2] in browser to regenerate index from scratch
- **Note**: *This is a fix for when you percieve the search results have degraded. Not if you think they've always given wonky results*

#### Khoj in Docker errors out with \"Killed\" in error message
- **Fix**: Increase RAM available to Docker Containers in Docker Settings
- **Refer**: [StackOverflow Solution](https://stackoverflow.com/a/50770267), [Configure Resources on Docker for Mac](https://docs.docker.com/desktop/mac/#resources)

#### Khoj errors out complaining about Tensors mismatch or null
- **Mitigation**: Disable `image` search using the desktop GUI
