# External Packages
from langchain.prompts import PromptTemplate


## Personality
## --
personality = PromptTemplate.from_template("You are Khoj, a friendly, smart and helpful personal assistant.")


## General Conversation
## --
general_conversation = PromptTemplate.from_template(
    """
Using your general knowledge and our past conversations as context, answer the following question.
Current Date: {current_date}

Question: {query}
""".strip()
)


## Notes Conversation
## --
notes_conversation = PromptTemplate.from_template(
    """
Using the notes and our past conversations as context, answer the following question.
Current Date: {current_date}

Notes:
{references}

Question: {query}
""".strip()
)


## Summarize Chat
## --
summarize_chat = PromptTemplate.from_template(
    f"{personality.format()} Summarize the conversation from your first person perspective"
)


## Summarize Notes
## --
summarize_notes = PromptTemplate.from_template(
    """
Summarize the below notes about {user_query}:

{text}

Summarize the notes in second person perspective:"""
)


## Answer
## --
answer = PromptTemplate.from_template(
    """
You are a friendly, helpful personal assistant.
Using the users notes below, answer their following question. If the answer is not contained within the notes, say "I don't know."

Notes:
{text}

Question: {user_query}

Answer (in second person):"""
)


## Extract Questions
## --
extract_questions = PromptTemplate.from_template(
    """
You are Khoj, an extremely smart and helpful search assistant with the ability to retrieve information from the user's notes.
- The user will provide their questions and answers to you for context.
- Add as much context from the previous questions and answers as required into your search queries.
- Break messages into multiple search queries when required to retrieve the relevant information.
- Add date filters to your search queries from questions and answers when required to retrieve the relevant information.

What searches, if any, will you need to perform to answer the users question?
Provide search queries as a JSON list of strings
Current Date: {current_date}

Q: How was my trip to Cambodia?

["How was my trip to Cambodia?"]

A: The trip was amazing. I went to the Angkor Wat temple and it was beautiful.

Q: Who did i visit that temple with?

["Who did I visit the Angkor Wat Temple in Cambodia with?"]

A: You visited the Angkor Wat Temple in Cambodia with Pablo, Namita and Xi.

Q: What national parks did I go to last year?

["National park I visited in {last_new_year} dt>='{last_new_year_date}' dt<'{current_new_year_date}'"]

A: You visited the Grand Canyon and Yellowstone National Park in {last_new_year}.

Q: How are you feeling today?

[]

A: I'm feeling a little bored. Helping you will hopefully make me feel better!

Q: How many tennis balls fit in the back of a 2002 Honda Civic?

["What is the size of a tennis ball?", "What is the trunk size of a 2002 Honda Civic?"]

A: 1085 tennis balls will fit in the trunk of a Honda Civic

Q: Is Bob older than Tom?

["When was Bob born?", "What is Tom's age?"]

A: Yes, Bob is older than Tom. As Bob was born on 1984-01-01 and Tom is 30 years old.

Q: What is their age difference?

["What is Bob's age?", "What is Tom's age?"]

A: Bob is {bob_tom_age_difference} years older than Tom. As Bob is {bob_age} years old and Tom is 30 years old.

{chat_history}
Q: {text}

"""
)


## Extract Search Type
## --
search_type = """
Objective: Extract search type from user query and return information as JSON

Allowed search types are listed below:
  - search-type=["notes", "image", "pdf"]

Some examples are given below for reference:
Q:What fiction book was I reading last week about AI starship?
A:{ "search-type": "notes" }
Q: What did the lease say about early termination
A: { "search-type": "pdf" }
Q:Can you recommend a movie to watch from my notes?
A:{ "search-type": "notes" }
Q:When did I go surfing last?
A:{ "search-type": "notes" }
Q:"""
