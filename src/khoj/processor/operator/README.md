# Khoj Operator (Experimental)

## Overview
Give Khoj its own computer to operate in a transparent, controlled manner. Accomplish tasks that require visual browsing, file editing and terminal access. Operator with research mode can work for 30+ minutes to accomplish more substantial tasks like feature development, travel planning, shopping etc.

## Setup

### Prerequisites
- Docker and Docker Compose installed
- Anthropic API key (required - only Anthropic models currently enabled)

### Installation Steps
1. Download the Khoj docker-compose.yml file
    ```shell
    mkdir ~/.khoj && cd ~/.khoj
    wget https://raw.githubusercontent.com/khoj-ai/khoj/master/docker-compose.yml
    ```

2. Update the `docker-compose.yml` to enable computer operator
    - Set `ANTHROPIC_API_KEY` to your [Anthropic API key](https://console.anthropic.com/settings/keys)
    - Uncomment `KHOJ_OPERATOR_ENABLED=True` to enable the operator tool
    - Uncomment `- /var/run/docker.sock:/var/run/docker.sock` to mount docker socket to allow khoj to operate its computer container.

3. Start Khoj services
    ```shell
    docker-compose up
    ```

4. Access the web app at http://localhost:42110
   Ensure you're using a claude 3.7+ models on your [settings page](http://localhost:42110/settings)

## Usage
Use the `/operator` command or ask Khoj in normal or research mode to use the operator tool to have it operate its computer:

**Examples:**
- `/operator Find flights from Bangkok to Mexico City with no US layover`
- `/research Clone the khoj repo and tell me how the operator tool is implemented`

## Supported Models

Currently enables **only Anthropic models**:
- Claude Sonnet 4
- Claude 3.7 Sonnet
- Claude Opus 4

*Note: OpenAI and other operator models are disabled while in development.*

## Capabilities

The operator can:
- **Computer Control**: Take screenshots, click, type, navigate desktop
- **File Operations**: Create, edit, and manage files
- **Terminal Access**: Execute bash commands and scripts
- **Web Browsing**: Navigate websites, documents and extract information

## Architecture

- **Environments**: Operator Computer and Browser environments
- **Models**: Enable Vision Language Models (VLM) to operate computer
- **Execution**: Containerize computer environment for security and isolation
