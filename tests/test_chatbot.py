# External Packages
import pytest

# Internal Packages
from src.processor.conversation.gpt import converse, understand, message_to_prompt


# Initialize variables for tests
model = 'text-davinci-003'
api_key = None  # Input your OpenAI API key to run the tests below


# Test
# ----------------------------------------------------------------------------------------------------
def test_message_to_understand_prompt():
    # Arrange
    understand_primer = "Extract information from each chat message\n\nremember(memory-type, data);\nmemory-type=[\"companion\", \"notes\", \"ledger\", \"image\", \"music\"]\nsearch(search-type, data);\nsearch-type=[\"google\", \"youtube\"]\ngenerate(activity);\nactivity=[\"paint\",\"write\", \"chat\"]\ntrigger-emotion(emotion);\nemotion=[\"happy\",\"confidence\",\"fear\",\"surprise\",\"sadness\",\"disgust\",\"anger\", \"curiosity\", \"calm\"]\n\nQ: How are you doing?\nA: activity(\"chat\"); trigger-emotion(\"surprise\")\nQ: Do you remember what I told you about my brother Antoine when we were at the beach?\nA: remember(\"notes\", \"Brother Antoine when we were at the beach\"); trigger-emotion(\"curiosity\");\nQ: what did we talk about last time?\nA: remember(\"notes\", \"talk last time\"); trigger-emotion(\"curiosity\");\nQ: Let's make some drawings!\nA: generate(\"paint\"); trigger-emotion(\"happy\");\nQ: Do you know anything about Lebanon?\nA: search(\"google\", \"lebanon\");  trigger-emotion(\"confidence\");\nQ: Find a video about a panda rolling in the grass\nA: search(\"youtube\",\"panda rolling in the grass\");  trigger-emotion(\"happy\"); \nQ: Tell me a scary story\nA: generate(\"write\" \"A story about some adventure\"); trigger-emotion(\"fear\");\nQ: What fiction book was I reading last week about AI starship?\nA: remember(\"notes\", \"read fiction book about AI starship last week\"); trigger-emotion(\"curiosity\");\nQ: How much did I spend at Subway for dinner last time?\nA: remember(\"ledger\", \"last Subway dinner\"); trigger-emotion(\"curiosity\");\nQ: I'm feeling sleepy\nA: activity(\"chat\"); trigger-emotion(\"calm\")\nQ: What was that popular Sri lankan song that Alex showed me recently?\nA: remember(\"music\", \"popular Sri lankan song that Alex showed recently\");  trigger-emotion(\"curiosity\"); \nQ: You're pretty funny!\nA: activity(\"chat\"); trigger-emotion(\"pride\")"
    expected_response = "Extract information from each chat message\n\nremember(memory-type, data);\nmemory-type=[\"companion\", \"notes\", \"ledger\", \"image\", \"music\"]\nsearch(search-type, data);\nsearch-type=[\"google\", \"youtube\"]\ngenerate(activity);\nactivity=[\"paint\",\"write\", \"chat\"]\ntrigger-emotion(emotion);\nemotion=[\"happy\",\"confidence\",\"fear\",\"surprise\",\"sadness\",\"disgust\",\"anger\", \"curiosity\", \"calm\"]\n\nQ: How are you doing?\nA: activity(\"chat\"); trigger-emotion(\"surprise\")\nQ: Do you remember what I told you about my brother Antoine when we were at the beach?\nA: remember(\"notes\", \"Brother Antoine when we were at the beach\"); trigger-emotion(\"curiosity\");\nQ: what did we talk about last time?\nA: remember(\"notes\", \"talk last time\"); trigger-emotion(\"curiosity\");\nQ: Let's make some drawings!\nA: generate(\"paint\"); trigger-emotion(\"happy\");\nQ: Do you know anything about Lebanon?\nA: search(\"google\", \"lebanon\");  trigger-emotion(\"confidence\");\nQ: Find a video about a panda rolling in the grass\nA: search(\"youtube\",\"panda rolling in the grass\");  trigger-emotion(\"happy\"); \nQ: Tell me a scary story\nA: generate(\"write\" \"A story about some adventure\"); trigger-emotion(\"fear\");\nQ: What fiction book was I reading last week about AI starship?\nA: remember(\"notes\", \"read fiction book about AI starship last week\"); trigger-emotion(\"curiosity\");\nQ: How much did I spend at Subway for dinner last time?\nA: remember(\"ledger\", \"last Subway dinner\"); trigger-emotion(\"curiosity\");\nQ: I'm feeling sleepy\nA: activity(\"chat\"); trigger-emotion(\"calm\")\nQ: What was that popular Sri lankan song that Alex showed me recently?\nA: remember(\"music\", \"popular Sri lankan song that Alex showed recently\");  trigger-emotion(\"curiosity\"); \nQ: You're pretty funny!\nA: activity(\"chat\"); trigger-emotion(\"pride\")\nQ: When did I last dine at Burger King?\nA:"

    # Act
    actual_response = message_to_prompt("When did I last dine at Burger King?", understand_primer, start_sequence="\nA:", restart_sequence="\nQ:")

    # Assert
    assert actual_response == expected_response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.skipif(api_key is None,
                    reason="Set api_key variable to your OpenAI API key from https://beta.openai.com/account/api-keys")
def test_minimal_chat_with_gpt():
    # Act
    response = converse("What will happen when the stars go out?", model=model, api_key=api_key)

    # Assert
    assert len(response) > 0


# ----------------------------------------------------------------------------------------------------
@pytest.mark.skipif(api_key is None,
                    reason="Set api_key variable to your OpenAI API key from https://beta.openai.com/account/api-keys")
def test_chat_with_history():
    # Arrange
    ai_prompt="AI:"
    human_prompt="Human:"

    conversation_primer = f'''
The following is a conversation with an AI assistant. The assistant is helpful, creative, clever, and very friendly companion.

{human_prompt} Hello, I am Testatron. Who are you?
{ai_prompt} Hi, I am Khoj, an AI conversational companion created by OpenAI. How can I help you today?'''

    # Act
    response = converse("Hi Khoj, What is my name?", model=model, conversation_history=conversation_primer, api_key=api_key, temperature=0, max_tokens=50)

    # Assert
    assert len(response) > 0
    assert "Testatron" in response or "testatron" in response


# ----------------------------------------------------------------------------------------------------
@pytest.mark.skipif(api_key is None,
                    reason="Set api_key variable to your OpenAI API key from https://beta.openai.com/account/api-keys")
def test_understand_message_using_gpt():
    # Act
    response = understand("When did I last dine at Subway?", model=model, api_key=api_key)

    # Assert
    assert len(response) > 0
    assert response['intent']['memory-type'] == 'ledger'
