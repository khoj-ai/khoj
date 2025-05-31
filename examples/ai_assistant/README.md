# AI Assistant Example

This directory contains an example of a versatile AI assistant that can interact with various large language models (OpenAI, Anthropic, Google Gemini) and utilize tools. It includes a command-line interface for interaction and a web interface (`web_interface.html`) for demonstration purposes.

## Features

-   Supports multiple LLM providers (OpenAI, Anthropic, Google Gemini).
-   CLI for direct interaction.
-   Example tools:
    -   `get_current_weather`: Fetches weather information (dummy implementation).
    -   `search_web`: Performs a web search using Tavily API.
    -   `save_note_to_obsidian`: Saves content to an Obsidian vault.
-   Basic conversation history management.
-   Example HTML control panel (`web_interface.html`) to showcase potential frontend integration (note: this HTML is a separate example and the `assistant.py` is primarily CLI-driven).

## Setup

1.  **Configuration (`config.py`)**:
    -   Copy or rename `config.py` (if you have a template) or edit `examples/ai_assistant/config.py` directly.
    -   Fill in your details in `config.py`:
        -   `OBSIDIAN_VAULT_PATH`: Set the absolute path to your Obsidian vault if you want to use the "save note" functionality. This is actively used by `assistant.py`.
        -   The file also contains placeholders for `ELEVENLABS_API_KEY`, `ELEVENLABS_AGENT_ID`, and `KHOJ_URL`. These are **not** currently used by the `assistant.py` script in this example but are included for potential future extensions or if you wish to adapt the script based on other versions.

2.  **Environment Variables for API Keys**:
    The `assistant.py` script expects API keys to be set as environment variables:
    -   For OpenAI models: `OPENAI_API_KEY`. If using OpenAI Assistants API, also set `OPENAI_ASSISTANT_ID`.
    -   For Anthropic models: `ANTHROPIC_API_KEY`.
    -   For Google Gemini models: `GOOGLE_API_KEY`.
    -   For web search functionality: `TAVILY_API_KEY`.

    Set these environment variables in your system before running the script. For example:
    ```bash
    export OPENAI_API_KEY="your_openai_key_here"
    export ANTHROPIC_API_KEY="your_anthropic_key_here"
    # etc.
    ```

3.  **Install Dependencies**:
    -   Navigate to this directory (`examples/ai_assistant/`) in your terminal.
    -   It's highly recommended to use a Python virtual environment:
        ```bash
        python3 -m venv venv
        source venv/bin/activate  # On Windows: venv\Scripts\activate
        ```
    -   Install the required packages:
        ```bash
        pip install -r requirements.txt
        ```

## Running the Assistant (CLI)

1.  Ensure your environment variables are set and `config.py` is configured (at least for `OBSIDIAN_VAULT_PATH` if you intend to use that feature).
2.  Run the script from within the `examples/ai_assistant/` directory:
    ```bash
    python assistant.py
    ```
    Or, from the repository root:
    ```bash
    python -m examples.ai_assistant.assistant
    ```
3.  The script will prompt you to choose a model.
4.  Interact with the assistant by typing your prompts in the command line. Type "exit" or "quit" to end.

## Web Interface (`web_interface.html`)

The `web_interface.html` file provides a basic chat interface that can be opened in a web browser.
**Note**: This HTML interface is a standalone example. It currently has its own JavaScript logic for interacting with LLMs (simulated in the provided version) and does **not** directly connect to or use the `assistant.py` script. It serves as a conceptual demo for a web UI. To make it work with `assistant.py`, you would need to implement a backend server (e.g., using Flask or FastAPI) that wraps `assistant.py` and provides API endpoints for the HTML frontend to call.

## Disclaimer

This is an example script and may require further modifications for production use. API interactions and tool usage may incur costs with the respective providers.
