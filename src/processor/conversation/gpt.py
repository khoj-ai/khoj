# Standard Packages
import os
import json
from datetime import datetime

# External Packages
import openai

# Internal Packages
from src.utils.constants import empty_escape_sequences


def summarize(text, summary_type, model, user_query=None, api_key=None, temperature=0.5, max_tokens=200):
    """
    Summarize user input using OpenAI's GPT
    """
    # Initialize Variables
    openai.api_key = api_key or os.getenv("OPENAI_API_KEY")

    # Setup Prompt based on Summary Type
    if summary_type == "chat":
        prompt = f'''
You are an AI. Summarize the conversation below from your perspective:

{text}

Summarize the conversation from the AI's first-person perspective:'''
    elif summary_type == "notes":
        prompt = f'''
Summarize the below notes about {user_query}:

{text}

Summarize the notes in second person perspective:'''

    # Get Response from GPT
    response = openai.Completion.create(
        prompt=prompt,
        model=model,
        temperature=temperature,
        max_tokens=max_tokens,
        frequency_penalty=0.2,
        stop="\"\"\"")

    # Extract, Clean Message from GPT's Response
    story = response['choices'][0]['text']
    return str(story).replace("\n\n", "")


def extract_search_type(text, model, api_key=None, temperature=0.5, max_tokens=100, verbose=0):
    """
    Extract search type from user query using OpenAI's GPT
    """
    # Initialize Variables
    openai.api_key = api_key or os.getenv("OPENAI_API_KEY")
    understand_primer = '''
Objective: Extract search type from user query and return information as JSON

Allowed search types are listed below:
  - search-type=["notes","ledger","image","music"]

Some examples are given below for reference:
Q:What fiction book was I reading last week about AI starship?
A:{ "search-type": "notes" }
Q:Play some calm classical music?
A:{ "search-type": "music" }
Q:How much did I spend at Subway for dinner last time?
A:{ "search-type": "ledger" }
Q:What was that popular Sri lankan song that Alex had mentioned?
A:{ "search-type": "music" }
Q:Can you recommend a movie to watch from my notes?
A:{ "search-type": "notes" }
Q: When did I buy Groceries last?
A:{ "search-type": "ledger" }
Q:When did I go surfing last?
A:{ "search-type": "notes" }'''

    # Setup Prompt with Understand Primer
    prompt = message_to_prompt(text, understand_primer, start_sequence="\nA:", restart_sequence="\nQ:")
    if verbose > 1:
        print(f"Message -> Prompt: {text} -> {prompt}")

    # Get Response from GPT
    response = openai.Completion.create(
        prompt=prompt,
        model=model,
        temperature=temperature,
        max_tokens=max_tokens,
        frequency_penalty=0.2,
        stop=["\n"])

    # Extract, Clean Message from GPT's Response
    story = str(response['choices'][0]['text'])
    return json.loads(story.strip(empty_escape_sequences))


def understand(text, model, api_key=None, temperature=0.5, max_tokens=100, verbose=0):
    """
    Understand user input using OpenAI's GPT
    """
    # Initialize Variables
    openai.api_key = api_key or os.getenv("OPENAI_API_KEY")
    understand_primer = '''
Objective: Extract intent and trigger emotion information as JSON from each chat message

Potential intent types and valid argument values are listed below:
- intent
  - remember(memory-type, query);
     - memory-type=["companion","notes","ledger","image","music"]
  - search(search-type, query);
     - search-type=["google"]
  - generate(activity, query);
     - activity=["paint","write","chat"]
- trigger-emotion(emotion)
   - emotion=["happy","confidence","fear","surprise","sadness","disgust","anger","shy","curiosity","calm"]

Some examples are given below for reference:
Q: How are you doing?
A: { "intent": {"type": "generate", "activity": "chat", "query": "How are you doing?"}, "trigger-emotion": "happy" }
Q: Do you remember what I told you about my brother Antoine when we were at the beach?
A: { "intent": {"type": "remember", "memory-type": "companion", "query": "Brother Antoine when we were at the beach"}, "trigger-emotion": "curiosity" }
Q: what was that fantasy story you told me last time?
A: { "intent": {"type": "remember", "memory-type": "companion", "query": "fantasy story told last time"}, "trigger-emotion": "curiosity" }
Q: Let's make some drawings about the stars on a clear full moon night!
A: { "intent": {"type": "generate", "activity": "paint", "query": "stars on a clear full moon night"}, "trigger-emotion: "happy" }
Q: Do you know anything about Lebanon cuisine in the 18th century?
A: { "intent": {"type": "search", "search-type": "google", "query": "lebanon cusine in the 18th century"}, "trigger-emotion; "confidence" }
Q: Tell me a scary story
A: { "intent": {"type": "generate", "activity": "write", "query": "A scary story"}, "trigger-emotion": "fear" }
Q: What fiction book was I reading last week about AI starship?
A: { "intent": {"type": "remember", "memory-type": "notes", "query": "fiction book about AI starship last week"}, "trigger-emotion": "curiosity" }
Q: How much did I spend at Subway for dinner last time?
A: { "intent": {"type": "remember", "memory-type": "ledger", "query": "last Subway dinner"}, "trigger-emotion": "calm" }
Q: I'm feeling sleepy
A: { "intent": {"type": "generate", "activity": "chat", "query": "I'm feeling sleepy"}, "trigger-emotion": "calm" }
Q: What was that popular Sri lankan song that Alex had mentioned?
A: { "intent": {"type": "remember", "memory-type": "music", "query": "popular Sri lankan song mentioned by Alex"}, "trigger-emotion": "curiosity" }
Q: You're pretty funny!
A: { "intent": {"type": "generate", "activity": "chat", "query": "You're pretty funny!"}, "trigger-emotion": "shy" }
Q: Can you recommend a movie to watch from my notes?
A: { "intent": {"type": "remember", "memory-type": "notes", "query": "recommend movie to watch"}, "trigger-emotion": "curiosity" }
Q: When did I go surfing last?
A: { "intent": {"type": "remember", "memory-type": "notes", "query": "When did I go surfing last"}, "trigger-emotion": "calm" }
Q: Can you dance for me?
A: { "intent": {"type": "generate", "activity": "chat", "query": "Can you dance for me?"}, "trigger-emotion": "sad" }'''

    # Setup Prompt with Understand Primer
    prompt = message_to_prompt(text, understand_primer, start_sequence="\nA:", restart_sequence="\nQ:")
    if verbose > 1:
        print(f"Message -> Prompt: {text} -> {prompt}")

    # Get Response from GPT
    response = openai.Completion.create(
        prompt=prompt,
        model=model,
        temperature=temperature,
        max_tokens=max_tokens,
        frequency_penalty=0.2,
        stop=["\n"])

    # Extract, Clean Message from GPT's Response
    story = str(response['choices'][0]['text'])
    return json.loads(story.strip(empty_escape_sequences))


def converse(text, model, conversation_history=None, api_key=None, temperature=0.9, max_tokens=150):
    """
    Converse with user using OpenAI's GPT
    """
    # Initialize Variables
    max_words = 500
    openai.api_key = api_key or os.getenv("OPENAI_API_KEY")

    conversation_primer = f'''
The following is a conversation with an AI assistant. The assistant is helpful, creative, clever, and a very friendly companion.

Human: Hello, who are you?
AI: Hi, I am an AI conversational companion created by OpenAI. How can I help you today?'''

    # Setup Prompt with Primer or Conversation History
    prompt = message_to_prompt(text, conversation_history or conversation_primer)
    prompt = ' '.join(prompt.split()[:max_words])

    # Get Response from GPT
    response = openai.Completion.create(
        prompt=prompt,
        model=model,
        temperature=temperature,
        max_tokens=max_tokens,
        presence_penalty=0.6,
        stop=["\n", "Human:", "AI:"])

    # Extract, Clean Message from GPT's Response
    story = str(response['choices'][0]['text'])
    return story.strip(empty_escape_sequences)


def message_to_prompt(user_message, conversation_history="", gpt_message=None, start_sequence="\nAI:", restart_sequence="\nHuman:"):
    """Create prompt for GPT from messages and conversation history"""
    gpt_message = f" {gpt_message}" if gpt_message else ""

    return f"{conversation_history}{restart_sequence} {user_message}{start_sequence}{gpt_message}"


def message_to_log(user_message, gpt_message, user_message_metadata={}, conversation_log=[]):
    """Create json logs from messages, metadata for conversation log"""
    default_user_message_metadata = {
        "intent": {
            "type": "remember",
            "memory-type": "notes",
            "query": user_message
        },
        "trigger-emotion": "calm"
    }
    current_dt = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # Create json log from Human's message
    human_log = user_message_metadata or default_user_message_metadata
    human_log["message"] = user_message
    human_log["by"] = "you"
    human_log["created"] = current_dt

    # Create json log from GPT's response
    khoj_log = {"message": gpt_message, "by": "khoj", "created": current_dt}

    conversation_log.extend([human_log, khoj_log])
    return conversation_log


def extract_summaries(metadata):
    """Extract summaries from metadata"""
    return ''.join(
        [f'\n{session["summary"]}' for session in metadata])