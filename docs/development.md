# Development

Welcome to the development docs of Khoj! Thanks for you interesting in being a contributor ❤️. Open source contributors are a corner-store of the Khoj community. We welcome all contributions, big or small.

To get started with contributing, check out the official GitHub docs on [contributing to an open-source project](https://docs.github.com/en/get-started/exploring-projects-on-github/contributing-to-a-project).

Join the [Discord](https://discord.gg/WaxF3SkFPU) server and click the ✅ for the question "Are you interested in becoming a contributor?" in the `#welcome-and-rules` channel. This will give you access to the `#contributors` channel where you can ask questions and get help from other contributors.

If you're looking for a place to get started, check out the list of [Github Issues](https://github.com/khoj-ai/khoj/issues) with the tag `good first issue` to find issues that are good for first-time contributors.

## Local Server Installation
### Using Pip
#### 1. Install


<!-- tabs:start -->

#### **MacOS**

```shell
# Get Khoj Code
git clone https://github.com/khoj-ai/khoj && cd khoj

# Create, Activate Virtual Environment
python3 -m venv .venv && source .venv/bin/activate

# For MacOS or zsh users run this
pip install -e '.[dev]'
```

#### **Windows**

```shell
# Get Khoj Code
git clone https://github.com/khoj-ai/khoj && cd khoj

# Create, Activate Virtual Environment
python3 -m venv .venv && .venv\Scripts\activate

# Install Khoj for Development
pip install -e .[dev]

```

#### **Linux**

```shell
# Get Khoj Code
git clone https://github.com/khoj-ai/khoj && cd khoj

# Create, Activate Virtual Environment
python3 -m venv .venv && source .venv/bin/activate

# Install Khoj for Development
pip install -e .[dev]

```

<!-- tabs:end -->


#### 2. Run
1. Start Khoj
   ```shell
   khoj -vv
   ```
2. Configure Khoj
   - **Via the Desktop application**: Add files, directories to index using the settings page of your desktop application. Click "Save" to immediately trigger indexing.

  Note: Wait after configuration for khoj to Load ML model, generate embeddings and expose API to query notes, images, documents etc specified in config YAML

### Using Docker

Make sure you install the latest version of [Docker](https://docs.docker.com/get-docker/) and [Docker Compose](https://docs.docker.com/compose/install/).

#### 1. Clone

```shell
git clone https://github.com/khoj-ai/khoj && cd khoj
```

#### 2. Configure

1. Update [docker-compose.yml](https://github.com/khoj-ai/khoj/blob/master/docker-compose.yml) to use relevant environment variables.
2. Comment out the `image` line and uncomment the `build` line in the `server` service

#### 3. Run

This will start the Khoj server, and the database.

```shell
docker-compose up -d
```

#### 4. Upgrade

If you've made changes to the codebase, you'll need to rebuild the Docker image before running the container again.

```shell
docker-compose build --no-cache
```

## Update clients
In whichever clients you're using for testing, you'll need to update the server URL to point to your local server. By default, the local server URL should be `http://127.0.0.1:42110`.

## Validate
### Before Making Changes
1. Install Git Hooks for Validation
   ```shell
   pre-commit install -t pre-push -t pre-commit
   ```
   - This ensures standard code formatting fixes and other checks run automatically on every commit and push
   - Note 1: If [pre-commit](https://pre-commit.com/#intro) didn't already get installed, [install it](https://pre-commit.com/#install) via `pip install pre-commit`
   - Note 2: To run the pre-commit changes manually, use `pre-commit run --hook-stage manual --all` before creating PR

### Before Creating PR

!> **Note**: You should be in an active virtual environment for Khoj in order to run the unit tests and linter.

1. Ensure that you have a [Github Issue](https://github.com/khoj-ai/khoj/issues) that can be linked to the PR. If not, create one. Make sure you've tagged one of the maintainers to the issue. This will ensure that the maintainers are notified of the PR and can review it. It's best discuss the code design on an existing issue or Discord thread before creating a PR. This helps get your PR merged faster.
1. Run unit tests.
   ```shell
   pytest
   ```
2. Run the linter.
   ```shell
   mypy
   ```
4. Think about how to add unit tests to verify the functionality you're adding in the PR. If you're not sure how to do this, ask for help in the Github issue or on Discord's `#contributors` channel.

### After Creating PR
1. Automated [validation workflows](.github/workflows) should run for every PR. Tag one of the maintainers in the PR to trigger it.

## Obsidian Plugin Development
### Plugin development setup
The core code for the Obsidian plugin is under `src/interface/obsidian`. The file `main.ts` is a good place to start.

1. In your CLI, go to the directory `src/interface/obsidian` in the Khoj repository.
2. Run `yarn install` to install the dependencies.
3. Run `yarn dev` to start the development server. This will continually rebuild the plugin as you make changes to the code.
    - Your code changes will be outputted to a file called `main.js` in the `obsidian` directory.

### Loading your development plugin in Obsidian
1. Make sure you have the Khoj plugin installed in Obsidian. [See the plugin page](https://publish.obsidian.md/hub/02+-+Community+Expansions/02.05+All+Community+Expansions/Plugins/khoj).
1. Open Obsidian and go to your settings (gear icon in the bottom left corner)
2. Click on 'Community Plugins' in the left panel
3. Next to the 'Installed Plugins' heading, click on the folder icon to open the folder with the plugin's source code.
4. Open the `khoj` folder in the file explorer that opens. You'll see a file called `main.js` in this folder. To test your changes, replace this file with the `main.js` file that was generated by the development server in the previous section.

## Create Khoj Release (Only for Maintainers)
Follow the steps below to [release](https://github.com/debanjum/khoj/releases/) Khoj. This will create a stable release of Khoj on [Pypi](https://pypi.org/project/khoj-assistant/), [Melpa](https://stable.melpa.org/#%252Fkhoj) and [Obsidian](https://obsidian.md/plugins?id%253Dkhoj). It will also create desktop apps of Khoj and attach them to the latest release.

1. Create and tag release commit by running the bump_version script. The release commit sets version number in required metadata files.
  ```shell
  ./scripts/bump_version.sh -c "<release_version>"
  ```
2. Push commit and then the tag to trigger the release workflow to create Release with auto generated release notes.
  ```shell
  git push origin master  # push release commit to khoj repository
  git push origin <release_version>  # push release tag to khoj repository
  ```
3. [Optional] Update the Release Notes to highlight new features, fixes and updates

## Architecture

![](./assets/khoj_architecture.png)

## Visualize Codebase

*[Interactive Visualization](https://mango-dune-07a8b7110.1.azurestaticapps.net/?repo=debanjum%2Fkhoj)*

![](./assets/khoj_codebase_visualization_0.2.1.png)

## Visualize Khoj Obsidian Plugin Codebase

![](./assets/khoj_obsidian_codebase_visualization_0.2.1.png)
