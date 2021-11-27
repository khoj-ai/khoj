# Standard Packages
import os
import json
from datetime import datetime

# External Packages
import openai


def understand(text, api_key=None, temperature=0.5, max_tokens=100):
    """
    Understand user input using OpenAI's GPT
    """
    # Initialize Variables
    openai.api_key = api_key or os.getenv("OPENAI_API_KEY")
    understand_primer = "Extract intent, trigger emotion information as JSON from each chat message\n\n- intent\n  - remember(memory-type, data);\n     - memory-type=[\"companion\", \"notes\", \"ledger\", \"image\", \"music\"]\n  - search(search-type, data);\n     - search-type=[\"google\"]\n  - generate(activity);\n     - activity=[\"paint\",\"write\", \"chat\"]\n- trigger-emotion(emotion)\n   - emotion=[\"happy\",\"confidence\",\"fear\",\"surprise\",\"sadness\",\"disgust\",\"anger\", \"curiosity\", \"calm\"]\n\nQ: How are you doing?\nA: { \"intent\": [\"activity\", \"chat\"], \"trigger-emotion\": \"surprise\" }\nQ: Do you remember what I told you about my brother Antoine when we were at the beach?\nA: { \"intent\": [\"remember\", \"notes\", \"Brother Antoine when we were at the beach\"], \"trigger-emotion\": \"curiosity\" }\nQ: what did we talk about last time?\nA: { \"intent\": [\"remember\", \"notes\", \"talk last time\"], \"trigger-emotion\": \"curiosity\" }\nQ: Let's make some drawings!\nA: { \"intent\": [\"generate\", \"paint\"], \"trigger-emotion: \"happy\" }\nQ: Do you know anything about Lebanon cuisine in the 18th century?\nA: { \"intent\": [\"search\", \"google\", \"lebanon cusine in the 18th century\"], \"trigger-emotion; \"confidence\" }\nQ: Tell me a scary story\nA: { \"intent\": [\"generate\", \"write\", \"A story about some adventure\"], \"trigger-emotion\": \"fear\" }\nQ: What fiction book was I reading last week about AI starship?\nA: { \"intent\": [\"remember\", \"notes\", \"fiction book about AI starship last week\"], \"trigger-emotion\": \"curiosity\" }\nQ: How much did I spend at Subway for dinner last time?\nA: { \"intent\": [\"remember\", \"ledger\", \"last Subway dinner\"], \"trigger-emotion\": \"curiosity\" }\nQ: I'm feeling sleepy\nA: { \"intent\": [\"activity\", \"chat\"], \"trigger-emotion\": \"calm\" }\nQ: What was that popular Sri lankan song that Alex had mentioned?\nA: { \"intent\": [\"remember\", \"music\", \"popular Sri lankan song mentioned by Alex\"], \"trigger-emotion\": \"curiosity\" } \nQ: You're pretty funny!\nA: { \"intent\": [\"activity\", \"chat\"], \"trigger-emotion\": \"pride\" }\nQ: Can you recommend a movie to watch from my notes?\nA: { \"intent\": [\"remember\", \"notes\", \"movie to watch\"], \"trigger-emotion\": \"curiosity\" }",

    # Setup Prompt with Understand Primer
    prompt = message_to_prompt(text, understand_primer, start_sequence="\nA:", restart_sequence="\nQ:")

    # Get Response from GPT
    response = openai.Completion.create(
        engine="davinci",
        prompt=prompt,
        temperature=temperature,
        max_tokens=max_tokens,
        top_p=1,
        frequency_penalty=0.2,
        presence_penalty=0,
        stop=["\n"])

    # Extract, Clean Message from GPT's Response
    story = response['choices'][0]['text']
    return str(story)


def converse(text, conversation_history=None, api_key=None, temperature=0.9, max_tokens=150):
    """
    Converse with user using OpenAI's GPT
    """
    # Initialize Variables
    openai.api_key = api_key or os.getenv("OPENAI_API_KEY")

    start_sequence = "\nAI:"
    restart_sequence = "\nHuman:"
    conversation_primer = f"The following is a conversation with an AI assistant. The assistant is helpful, creative, clever, and very friendly companion.\n{restart_sequence} Hello, who are you?{start_sequence} Hi, I am an AI conversational companion created by OpenAI. How can I help you today?"

    # Setup Prompt with Primer or Conversation History
    prompt = message_to_prompt(text, conversation_history or conversation_primer, start_sequence=start_sequence, restart_sequence=restart_sequence)

    # Get Response from GPT
    response = openai.Completion.create(
        engine="davinci",
        prompt=prompt,
        temperature=temperature,
        max_tokens=max_tokens,
        top_p=1,
        frequency_penalty=0,
        presence_penalty=0.6,
        stop=["\n", "Human:", "AI:"])

    # Extract, Clean Message from GPT's Response
    story = response['choices'][0]['text']
    return str(story).strip()


def message_to_prompt(user_message, conversation_history="", gpt_message=None, start_sequence="\nAI:", restart_sequence="\nHuman:"):
    """Create prompt for GPT from messages and conversation history"""
    gpt_message = f" {gpt_message}" if gpt_message else ""

    return f"{conversation_history}{restart_sequence} {user_message}{start_sequence}{gpt_message}"


def message_to_log(user_message, user_message_metadata, gpt_message, conversation_log=[]):
    """Create json logs from messages, metadata for conversation log"""
    # Create json log from Human's message
    human_log = json.loads(user_message_metadata)
    human_log["message"] = user_message
    human_log["by"] = "Human"
    human_log["created"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # Create json log from GPT's response
    ai_log = {"message": gpt_message, "by": "AI", "created": datetime.now().strftime("%Y-%m-%d %H:%M:%S")}

    conversation_log.extend([human_log, ai_log])
    return conversation_log
