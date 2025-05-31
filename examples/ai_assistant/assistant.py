#!/usr/bin/env python3

import os
import openai
import anthropic
import google.generativeai as genai
from tavily import TavilyClient
from termcolor import colored
import inspect
import json
import logging
from datetime import datetime
from examples.ai_assistant import config

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

class AIAssistant:
    def __init__(self, api_key_env_var, assistant_id_env_var, model="claude-3-opus-20240229", temperature=0.7, max_tokens=1500):
        """
        Initializes the AI Assistant.

        Args:
            api_key_env_var (str): The environment variable name for the API key.
            assistant_id_env_var (str): The environment variable name for the assistant ID (if applicable).
            model (str): The model name to use.
            temperature (float): The sampling temperature.
            max_tokens (int): The maximum number of tokens to generate.
        """
        self.model_name = model
        self.temperature = temperature
        self.max_tokens = max_tokens
        self.tools = []
        self.tool_schemas = []
        self.conversation_history = []

        if "claude" in model:
            self.api_key = os.getenv(api_key_env_var)
            if not self.api_key:
                raise ValueError(f"API key environment variable '{api_key_env_var}' not set for Claude.")
            self.client = anthropic.Anthropic(api_key=self.api_key)
            self.provider = "anthropic"
        elif "gpt" in model:
            self.api_key = os.getenv(api_key_env_var)
            if not self.api_key:
                raise ValueError(f"API key environment variable '{api_key_env_var}' not set for OpenAI.")
            openai.api_key = self.api_key
            self.client = openai.OpenAI() # Corrected initialization
            self.provider = "openai"
            self.assistant_id = os.getenv(assistant_id_env_var)
            if not self.assistant_id:
                logging.warning(f"Assistant ID environment variable '{assistant_id_env_var}' not set for OpenAI. Threading will not be used.")
        elif "gemini" in model:
            self.api_key = os.getenv(api_key_env_var)
            if not self.api_key:
                raise ValueError(f"API key environment variable '{api_key_env_var}' not set for Google Gemini.")
            genai.configure(api_key=self.api_key)
            self.client = genai.GenerativeModel(model, safety_settings={'HARASSMENT':'block_none'}) # Basic safety, adjust as needed
            self.provider = "google"
        else:
            raise ValueError(f"Unsupported model provider for: {model}")

        logging.info(f"Initialized AIAssistant with model: {self.model_name} (Provider: {self.provider})")

    def add_tool(self, func):
        """Adds a tool (function) to the assistant's capabilities."""
        if not callable(func):
            raise ValueError("Tool must be a callable function.")

        tool_name = func.__name__
        tool_description = inspect.getdoc(func)
        tool_params = inspect.signature(func).parameters

        schema = {
            "type": "function",
            "function": {
                "name": tool_name,
                "description": tool_description,
                "parameters": {
                    "type": "object",
                    "properties": {},
                    "required": []
                }
            }
        }

        for name, param in tool_params.items():
            param_type = "string" # Default type
            if param.annotation != inspect.Parameter.empty:
                if param.annotation == int:
                    param_type = "integer"
                elif param.annotation == float:
                    param_type = "number"
                elif param.annotation == bool:
                    param_type = "boolean"
                # Add more type mappings as needed

            schema["function"]["parameters"]["properties"][name] = {
                "type": param_type,
                "description": f"Parameter '{name}' for {tool_name}" # Generic description
            }
            if param.default == inspect.Parameter.empty:
                schema["function"]["parameters"]["required"].append(name)

        self.tools.append({"name": tool_name, "function": func, "schema": schema["function"]})
        self.tool_schemas.append(schema) # For OpenAI Assistants API
        logging.info(f"Added tool: {tool_name}")

    def _execute_tool(self, tool_name, arguments_str):
        """Executes a tool with the given arguments string."""
        logging.info(f"Attempting to execute tool: {tool_name} with arguments: {arguments_str}")
        for tool in self.tools:
            if tool["name"] == tool_name:
                try:
                    arguments = json.loads(arguments_str)
                    result = tool["function"](**arguments)
                    logging.info(f"Tool '{tool_name}' executed successfully. Result: {result}")
                    return result
                except json.JSONDecodeError as e:
                    logging.error(f"JSON decoding error for tool '{tool_name}' arguments: {e}. Argument string: {arguments_str}")
                    return f"Error: Invalid JSON arguments: {arguments_str}"
                except Exception as e:
                    logging.error(f"Error executing tool '{tool_name}': {e}")
                    return f"Error: {str(e)}"
        logging.warning(f"Tool '{tool_name}' not found.")
        return f"Error: Tool '{tool_name}' not found."

    def _get_openai_response(self, prompt, thread_id=None):
        """Gets a response from OpenAI, handling tool calls and threading."""
        if not self.assistant_id: # No assistant, use simple chat completion
            messages = [{"role": "system", "content": "You are a helpful assistant."}] + self.conversation_history + [{"role": "user", "content": prompt}]
            if self.tool_schemas:
                 response = self.client.chat.completions.create(
                    model=self.model_name,
                    messages=messages,
                    tools=self.tool_schemas,
                    tool_choice="auto",
                    temperature=self.temperature,
                    max_tokens=self.max_tokens,
                )
            else:
                response = self.client.chat.completions.create(
                    model=self.model_name,
                    messages=messages,
                    temperature=self.temperature,
                    max_tokens=self.max_tokens,
                )
            return response.choices[0].message

        # Assistant workflow with threading
        if not thread_id:
            thread = self.client.beta.threads.create()
            thread_id = thread.id
            logging.info(f"Created new OpenAI thread: {thread_id}")

        self.client.beta.threads.messages.create(
            thread_id=thread_id,
            role="user",
            content=prompt
        )
        logging.info(f"Added message to thread {thread_id}: '{prompt}'")

        run = self.client.beta.threads.runs.create(
            thread_id=thread_id,
            assistant_id=self.assistant_id,
            instructions="Please address the user's request. Use tools if necessary."
        )
        logging.info(f"Created run {run.id} for thread {thread_id}")

        while run.status in ['queued', 'in_progress', 'cancelling']:
            # time.sleep(1) # Consider adding a small delay
            run = self.client.beta.threads.runs.retrieve(thread_id=thread_id, run_id=run.id)
            logging.debug(f"Run status: {run.status}")

        if run.status == 'completed':
            messages = self.client.beta.threads.messages.list(thread_id=thread_id)
            logging.info(f"Run {run.id} completed. Retrieving messages.")
            return messages.data[0] # The latest message from the assistant
        elif run.status == 'requires_action':
            logging.info(f"Run {run.id} requires action. Processing tool calls.")
            tool_outputs = []
            for tool_call in run.required_action.submit_tool_outputs.tool_calls:
                tool_name = tool_call.function.name
                arguments_str = tool_call.function.arguments
                output = self._execute_tool(tool_name, arguments_str)
                tool_outputs.append({
                    "tool_call_id": tool_call.id,
                    "output": str(output) # Ensure output is string
                })

            self.client.beta.threads.runs.submit_tool_outputs(
                thread_id=thread_id,
                run_id=run.id,
                tool_outputs=tool_outputs
            )
            logging.info(f"Submitted tool outputs for run {run.id}. Waiting for further processing.")
            # Wait for the run to complete after submitting tool outputs
            # This part might need to re-enter the loop or have a similar waiting mechanism
            # For simplicity, let's retrieve the run again and assume it might complete or require more actions
            # In a real scenario, a more robust loop here is needed.
            # time.sleep(1) # Give time for processing
            updated_run = self.client.beta.threads.runs.retrieve(thread_id=thread_id, run_id=run.id)
            if updated_run.status == 'completed':
                 messages = self.client.beta.threads.messages.list(thread_id=thread_id)
                 logging.info(f"Run {run.id} completed after tool submission. Retrieving messages.")
                 return messages.data[0]
            else:
                # Handle cases where it might still be running or failed
                logging.warning(f"Run {run.id} status after tool submission: {updated_run.status}")
                return {"role": "assistant", "content": f"Tool call processing resulted in status: {updated_run.status}"}


        elif run.status == 'failed':
            logging.error(f"Run {run.id} failed. Reason: {run.last_error}")
            return {"role": "assistant", "content": f"The request processing failed: {run.last_error.message if run.last_error else 'Unknown error'}"}
        else: # e.g. 'expired', 'cancelled'
            logging.warning(f"Run {run.id} ended with status: {run.status}")
            return {"role": "assistant", "content": f"The request ended with status: {run.status}"}


    def _get_anthropic_response(self, prompt):
        """Gets a response from Anthropic Claude, handling tool calls."""
        system_prompt = "You are a helpful assistant. Respond to the user's request. If you need to use tools, do so by calling the provided functions."
        messages = self.conversation_history + [{"role": "user", "content": prompt}]

        kwargs = {
            "model": self.model_name,
            "messages": messages,
            "temperature": self.temperature,
            "max_tokens": self.max_tokens,
            "system": system_prompt
        }

        if self.tools:
            # Anthropic uses a slightly different format for "tools" (functions)
            anthropic_tools = [{"name": t["name"], "description": t["schema"]["description"], "input_schema": t["schema"]["parameters"]} for t in self.tools]
            kwargs["tools"] = anthropic_tools

        response = self.client.messages.create(**kwargs)

        if response.stop_reason == "tool_use":
            logging.info(f"Claude suggests tool use: {response.content}")
            tool_results_content = []
            assistant_messages_content = [] # To store text parts from assistant

            for content_block in response.content:
                if content_block.type == "text":
                    assistant_messages_content.append(content_block.text)
                elif content_block.type == "tool_use":
                    tool_name = content_block.name
                    tool_input = content_block.input
                    tool_use_id = content_block.id # Important for Claude

                    logging.info(f"Executing tool: {tool_name} with input: {tool_input}")
                    # Arguments for Claude are already a dict
                    tool_result = self._execute_tool(tool_name, json.dumps(tool_input))

                    tool_results_content.append({
                        "type": "tool_result",
                        "tool_use_id": tool_use_id,
                        "content": str(tool_result) # Ensure result is string
                        # "is_error": True # Optionally indicate if it was an error
                    })

            # If there was any initial text from the assistant, add it to history
            if assistant_messages_content:
                 self.conversation_history.append({"role": "assistant", "content": "".join(assistant_messages_content)})


            # Send back the tool results
            logging.info(f"Sending tool results back to Claude: {tool_results_content}")
            follow_up_response = self.client.messages.create(
                model=self.model_name,
                messages=self.conversation_history + [
                    {"role": "user", "content": prompt}, # Original prompt
                    {"role": "assistant", "content": response.content}, # Original assistant response with tool_use
                    {
                        "role": "user",
                        "content": tool_results_content # Results from tool execution
                    }
                ],
                temperature=self.temperature,
                max_tokens=self.max_tokens,
                system=system_prompt,
                tools=anthropic_tools # Pass tools again
            )
            return {"role": "assistant", "content": "".join([c.text for c in follow_up_response.content if c.type == "text"]) if follow_up_response.content else ""}
        else:
            # Standard response without tool use
            return {"role": "assistant", "content": "".join([c.text for c in response.content if c.type == "text"]) if response.content else ""}


    def _get_google_response(self, prompt):
        """Gets a response from Google Gemini, handling tool calls."""
        # For Gemini, tools are passed at the generation config level,
        # and function declarations are used.
        gemini_tools = None
        if self.tools:
            gemini_tools = [genai.types.FunctionDeclaration(**t["schema"]) for t in self.tools]


        # Gemini uses a slightly different chat history structure
        gemini_history = []
        for msg in self.conversation_history:
            role = 'user' if msg['role'] == 'user' else 'model'
            # Gemini expects 'parts' which is a list of content parts. For simple text, it's just one part.
            # If a message was a tool call or result, it needs to be formatted correctly.
            # This part needs careful adaptation based on how tool calls/results are stored.
            if isinstance(msg['content'], str):
                gemini_history.append({'role': role, 'parts': [{'text': msg['content']}]})
            # else:
                # TODO: Handle complex content like tool calls / results in history if needed
                # This might involve converting OpenAI/Anthropic style tool content to Gemini's format.
                # For now, assuming simple text history for Gemini's initial turn.

        chat = self.client.start_chat(history=gemini_history)
        logging.info(f"Sending prompt to Gemini: '{prompt}' with tools: {gemini_tools is not None}")

        try:
            response = chat.send_message(prompt, tools=gemini_tools if gemini_tools else None)
        except Exception as e:
            logging.error(f"Error sending message to Gemini: {e}")
            # Check for specific blockages, e.g., safety settings
            if hasattr(e, 'response') and e.response.prompt_feedback.block_reason:
                logging.error(f"Gemini content blocked. Reason: {e.response.prompt_feedback.block_reason}")
                return {"role": "assistant", "content": f"My response was blocked due to: {e.response.prompt_feedback.block_reason}. Please rephrase or try a different prompt."}
            return {"role": "assistant", "content": f"Error communicating with Gemini: {str(e)}"}


        response_content = ""
        # Gemini's response can have multiple parts, including function calls
        for part in response.parts:
            if part.text:
                response_content += part.text
            elif part.function_call:
                fc = part.function_call
                tool_name = fc.name
                # Gemini provides arguments as a native dict-like structure (Struct)
                # Convert fc.args (Struct) to a Python dict for _execute_tool
                arguments_dict = {key: value for key, value in fc.args.items()}
                arguments_str = json.dumps(arguments_dict) # _execute_tool expects JSON string

                logging.info(f"Gemini function call: {tool_name}, args: {arguments_dict}")
                tool_result = self._execute_tool(tool_name, arguments_str)

                # Send the tool result back to Gemini
                logging.info(f"Sending tool result back to Gemini: {tool_result}")
                # The API expects a list of Part objects for function responses
                function_response_part = genai.types.Part(
                    function_response=genai.types.FunctionResponse(
                        name=tool_name,
                        response={'result': str(tool_result)} # Gemini expects a dict here
                    )
                )
                # Continue the chat with the function response
                response = chat.send_message([function_response_part], tools=gemini_tools if gemini_tools else None)
                # Process the new response which should contain the final answer
                response_content = "".join([p.text for p in response.parts if p.text]) # Reset and get new text
                break # Assuming one tool call per turn for simplicity here
        return {"role": "assistant", "content": response_content}


    def get_response(self, prompt: str, thread_id: str = None):
        """
        Gets a response from the configured AI model.

        Args:
            prompt (str): The user's prompt.
            thread_id (str, optional): The ID of the conversation thread (for OpenAI Assistants).
                                       If None, a new thread will be created for OpenAI.

        Returns:
            str: The assistant's response.
            str: The thread ID used (especially relevant for OpenAI).
        """
        logging.info(f"User prompt: {prompt}")
        self.conversation_history.append({"role": "user", "content": prompt})

        response_message = None
        current_thread_id = thread_id

        try:
            if self.provider == "anthropic":
                response_data = self._get_anthropic_response(prompt)
                response_text = response_data["content"]
            elif self.provider == "openai":
                # Pass current_thread_id, which might be None for the first turn
                response_obj = self._get_openai_response(prompt, thread_id=current_thread_id)

                if isinstance(response_obj, dict) and "role" in response_obj: # Simple completion
                     response_text = response_obj.get("content","")
                     if response_obj.get("tool_calls"): # Handle tool calls from simple completion
                        # This part is for non-assistant OpenAI calls that might use tools
                        tool_calls = response_obj["tool_calls"]
                        # ... (tool call handling logic similar to _get_openai_response's requires_action)
                        # For simplicity, this example assumes direct text or assistant path handles tools.
                        # A full implementation would mirror the 'requires_action' logic here.
                        logging.info("Tool calls present in non-assistant OpenAI response, requires further handling.")

                elif hasattr(response_obj, 'content'): # Assistant API message object
                    # OpenAI Assistant messages are lists of content blocks
                    response_text = ""
                    for content_block in response_obj.content:
                        if content_block.type == "text":
                            response_text += content_block.text.value + "\n"
                    current_thread_id = response_obj.thread_id # Update thread_id from response
                else:
                    response_text = "Error: Unexpected response structure from OpenAI."
                    logging.error(f"Unexpected OpenAI response structure: {response_obj}")


            elif self.provider == "google":
                response_data = self._get_google_response(prompt)
                response_text = response_data["content"]
            else:
                response_text = "Error: Provider not implemented correctly."
                logging.error("Provider logic missing in get_response.")

        except Exception as e:
            logging.error(f"Error getting response from {self.provider}: {e}", exc_info=True)
            response_text = f"An error occurred: {str(e)}"

        self.conversation_history.append({"role": "assistant", "content": response_text})
        logging.info(f"Assistant response: {response_text}")
        return response_text, current_thread_id


# --- Example Tools ---
def get_current_weather(location: str, unit: str = "celsius") -> str:
    """
    Get the current weather in a given location.

    Args:
        location (str): The city and state, e.g., San Francisco, CA.
        unit (str): The unit for temperature, either "celsius" or "fahrenheit".
    """
    # This is a dummy function. Replace with actual API call.
    if "tokyo" in location.lower():
        return json.dumps({"location": location, "temperature": "10", "unit": unit, "forecast": "snowy"})
    elif "san francisco" in location.lower():
        return json.dumps({"location": location, "temperature": "72", "unit": unit, "forecast": "sunny"})
    else:
        return json.dumps({"location": location, "temperature": "22", "unit": unit, "forecast": "partly cloudy"})

def search_web(query: str, num_results: int = 5) -> str:
    """
    Searches the web using Tavily API for a given query.

    Args:
        query (str): The search query.
        num_results (int): The number of search results to return.
    """
    tavily_api_key = os.getenv("TAVILY_API_KEY")
    if not tavily_api_key:
        return "Error: TAVILY_API_KEY environment variable not set."
    try:
        tavily_client = TavilyClient(api_key=tavily_api_key)
        response = tavily_client.search(query=query, search_depth="advanced", max_results=num_results)
        # Return a summary of results or specific data points
        return json.dumps([{"title": r["title"], "url": r["url"], "content": r["content"][:200]+"..."} for r in response.get("results", [])])
    except Exception as e:
        return f"Error during Tavily search: {str(e)}"

def save_note_to_obsidian(note_content: str, vault_path: str, note_title: str = None) -> str:
    """
    Saves a note to a specified Obsidian vault.

    Args:
        note_content (str): The content of the note.
        vault_path (str): The absolute path to the Obsidian vault.
        note_title (str, optional): The title of the note. If None, a title will be generated.
                                    Should be a valid filename without the .md extension.
    """
    if not os.path.isdir(vault_path):
        return f"Error: Obsidian vault path '{vault_path}' does not exist or is not a directory."

    if not note_title:
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        note_title = f"Note_{timestamp}"
    elif "/" in note_title or "\\" in note_title or "." in note_title: # Basic check for invalid chars
        return "Error: Note title contains invalid characters or extensions."

    file_path = os.path.join(vault_path, f"{note_title}.md")

    try:
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(note_content)
        return f"Note saved successfully to '{file_path}'"
    except Exception as e:
        return f"Error saving note to Obsidian: {str(e)}"


# --- Main Execution (Example CLI) ---
if __name__ == "__main__":
    print(colored("AI Assistant CLI", "cyan"))
    print(colored("==================", "cyan"))
    print(colored("Available models: gpt-4o, gpt-3.5-turbo, claude-3-opus-20240229, claude-3-sonnet-20240229, claude-3-haiku-20240307, gemini-1.5-pro-latest, gemini-1.0-pro", "yellow"))
    model_choice = input(colored("Choose a model (or press Enter for default 'claude-3-opus-20240229'): ", "green")) or "claude-3-opus-20240229"

    # Determine API key and assistant ID environment variables based on model choice
    # This is a simplified way to handle keys; a config file or more robust logic is better for production
    api_key_env = ""
    assistant_id_env = "" # Only for OpenAI Assistant API

    if "gpt" in model_choice:
        api_key_env = "OPENAI_API_KEY"
        assistant_id_env = "OPENAI_ASSISTANT_ID" # Specify your assistant ID env var
    elif "claude" in model_choice:
        api_key_env = "ANTHROPIC_API_KEY"
    elif "gemini" in model_choice:
        api_key_env = "GOOGLE_API_KEY" # Or specific Gemini key env var if different
    else:
        print(colored(f"Unknown model provider for {model_choice}. Exiting.", "red"))
        exit(1)

    if not os.getenv(api_key_env):
        print(colored(f"Error: The environment variable {api_key_env} is not set.", "red"))
        print(colored(f"Please set it before running the script.", "red"))
        exit(1)

    if "gpt" in model_choice and not os.getenv(assistant_id_env) and assistant_id_env: # Check if assistant_id_env is actually set
        print(colored(f"Warning: OpenAI Assistant ID environment variable '{assistant_id_env}' not set. Will use Chat Completions API without persistent threads.", "yellow"))


    try:
        # Initialize with dummy values if specific env vars are not needed for a model type
        assistant = AIAssistant(
            api_key_env_var=api_key_env,
            assistant_id_env_var=assistant_id_env if "gpt" in model_choice else "DUMMY_ASSISTANT_ID_VAR", # Pass dummy if not OpenAI assistant
            model=model_choice
        )

        # Add tools
        assistant.add_tool(get_current_weather)
        assistant.add_tool(search_web)

        # Example of adding Obsidian tool with vault path from environment variable
        obsidian_vault_path = config.OBSIDIAN_VAULT_PATH
        if obsidian_vault_path:
            # Use a lambda to pass the vault_path to the tool function if it's fixed
            # or modify save_note_to_obsidian to retrieve it from env itself.
            # For this example, let's assume save_note_to_obsidian can take it.
            # If save_note_to_obsidian is defined to take vault_path, this is fine.
            # If it's not, you'd need a wrapper or to ensure it reads the env var.
            # The current save_note_to_obsidian takes it as an argument.
             assistant.add_tool(save_note_to_obsidian) # The function itself
             print(colored(f"Obsidian integration enabled. Vault path: {obsidian_vault_path}", "blue"))
        else:
            print(colored("OBSIDIAN_VAULT_PATH environment variable not set. Obsidian features will be disabled.", "yellow"))


        print(colored("Type 'exit' or 'quit' to end the session.", "magenta"))

        current_openai_thread_id = None # For OpenAI assistant threading

        while True:
            user_input = input(colored("You: ", "green"))
            if user_input.lower() in ["exit", "quit"]:
                print(colored("Exiting assistant. Goodbye!", "cyan"))
                break

            response, thread_id_returned = assistant.get_response(user_input, thread_id=current_openai_thread_id)

            if "gpt" in model_choice and assistant.assistant_id: # Only update thread_id if using OpenAI assistant
                current_openai_thread_id = thread_id_returned

            print(colored(f"AI: {response}", "blue"))

    except ValueError as ve:
        print(colored(f"Configuration Error: {ve}", "red"))
    except Exception as e:
        print(colored(f"An unexpected error occurred: {e}", "red"))
        logging.error("Main loop error", exc_info=True)
