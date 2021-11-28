# Standard Packages
import sys, json, yaml
from typing import Optional

# External Packages
import uvicorn
from fastapi import FastAPI, Request
from fastapi.responses import HTMLResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates

# Internal Packages
from src.search_type import asymmetric, symmetric_ledger, image_search
from src.utils.helpers import get_absolute_path
from src.utils.cli import cli
from src.utils.config import SearchType, SearchModels, TextSearchConfig, ImageSearchConfig, SearchConfig, ProcessorConfig, ConversationProcessorConfig
from src.utils.rawconfig import FullConfig
from src.processor.conversation.gpt import converse, message_to_prompt

# Application Global State
model = SearchModels()
search_config = SearchConfig()
processor_config = ProcessorConfig()
config = {}
config_file = ""
app = FastAPI()

app.mount("/views", StaticFiles(directory="views"), name="views")
templates = Jinja2Templates(directory="views/")

@app.get('/ui', response_class=HTMLResponse)
def ui(request: Request):
    return templates.TemplateResponse("config.html", context={'request': request})

@app.get('/config', response_model=FullConfig)
def config():
    return config

@app.post('/config')
async def config(updated_config: FullConfig):
    global config
    config = updated_config
    with open(config_file, 'w') as outfile:
        yaml.dump(yaml.safe_load(config.json(by_alias=True)), outfile)
        outfile.close()
    return config

@app.get('/search')
def search(q: str, n: Optional[int] = 5, t: Optional[SearchType] = None):
    if q is None or q == '':
        print(f'No query param (q) passed in API call to initiate search')
        return {}

    user_query = q
    results_count = n

    if (t == SearchType.Notes or t == None) and model.notes_search:
        # query notes
        hits = asymmetric.query(user_query, model.notes_search)

        # collate and return results
        return asymmetric.collate_results(hits, model.notes_search.entries, results_count)

    if (t == SearchType.Music or t == None) and model.music_search:
        # query music library
        hits = asymmetric.query(user_query, model.music_search)

        # collate and return results
        return asymmetric.collate_results(hits, model.music_search.entries, results_count)

    if (t == SearchType.Ledger or t == None) and model.ledger_search:
        # query transactions
        hits = symmetric_ledger.query(user_query, model.ledger_search)

        # collate and return results
        return symmetric_ledger.collate_results(hits, model.ledger_search.entries, results_count)

    if (t == SearchType.Image or t == None) and model.image_search:
        # query transactions
        hits = image_search.query(user_query, results_count, model.image_search)

        # collate and return results
        return image_search.collate_results(
            hits,
            model.image_search.image_names,
            search_config.image.input_directory,
            results_count)

    else:
        return {}


@app.get('/regenerate')
def regenerate(t: Optional[SearchType] = None):
    if (t == SearchType.Notes or t == None) and search_config.notes:
        # Extract Entries, Generate Embeddings
        model.notes_search = asymmetric.setup(search_config.notes, regenerate=True)

    if (t == SearchType.Music or t == None) and search_config.music:
        # Extract Entries, Generate Song Embeddings
        model.music_search = asymmetric.setup(search_config.music, regenerate=True)

    if (t == SearchType.Ledger or t == None) and search_config.ledger:
        # Extract Entries, Generate Embeddings
        model.ledger_search = symmetric_ledger.setup(search_config.ledger, regenerate=True)

    if (t == SearchType.Image or t == None) and search_config.image:
        # Extract Images, Generate Embeddings
        model.image_search = image_search.setup(search_config.image, regenerate=True)

    return {'status': 'ok', 'message': 'regeneration completed'}


@app.get('/chat')
def chat(q: str):
    # Load Conversation History
    conversation_history = processor_config.conversation.conversation_history

    # Converse with OpenAI GPT
    gpt_response = converse(q, conversation_history, api_key=processor_config.conversation.openai_api_key)

    # Update Conversation History
    processor_config.conversation.conversation_history = message_to_prompt(q, conversation_history, gpt_response)

    return {'status': 'ok', 'response': gpt_response}


def initialize_search(regenerate, verbose):
    model = SearchModels()
    search_config = SearchConfig()

    # Initialize Org Notes Search
    if config.content_type.org:
        search_config.notes = TextSearchConfig(config.content_type.org, verbose)
        model.notes_search = asymmetric.setup(search_config.notes, regenerate=regenerate)

    # Initialize Org Music Search
    if config.content_type.music:
        search_config.music = TextSearchConfig(config.content_type.music, verbose)
        model.music_search = asymmetric.setup(search_config.music, regenerate=regenerate)

    # Initialize Ledger Search
    if config.content_type.ledger:
        search_config.ledger = TextSearchConfig(config.content_type.org, verbose)
        model.ledger_search = symmetric_ledger.setup(search_config.ledger, regenerate=regenerate)

    # Initialize Image Search
    if config.content_type.image:
        search_config.image = ImageSearchConfig(config.content_type.image, verbose)
        model.image_search = image_search.setup(search_config.image, regenerate=regenerate)

    return model, search_config


def initialize_processor(verbose):
    processor_config = ProcessorConfig()

    # Initialize Conversation Processor
    processor_config.conversation = ConversationProcessorConfig(config.processor.conversation, verbose)

    # Load or Initialize Conversation History from Disk
    conversation_logfile = processor_config.conversation.conversation_logfile
    if processor_config.conversation.verbose:
        print('Saving conversation logs to disk...')

    if conversation_logfile.expanduser().absolute().is_file():
        with open(get_absolute_path(conversation_logfile), 'r') as f:
            processor_config.conversation.conversation_history = json.load(f).get('chat', '')
    else:
        processor_config.conversation.conversation_history = ''

    return processor_config


@app.on_event('shutdown')
def shutdown_event():
    if processor_config.conversation.verbose:
        print('Saving conversation logs to disk...')

    # Save Conversation History to Disk
    conversation_logfile = get_absolute_path(processor_config.conversation.conversation_logfile)
    with open(conversation_logfile, "w+", encoding='utf-8') as logfile:
        json.dump({"chat": processor_config.conversation.conversation_history}, logfile)

    print('Conversation logs saved to disk.')


if __name__ == '__main__':
    # Load config from CLI
    args = cli(sys.argv[1:])
    
    # Stores the file path to the config file.
    config_file = args.config_file

    # Store the raw config data.
    config = args.config

    # Initialize Search from Config
    model, search_config = initialize_search(args.regenerate, args.verbose)

    # Initialize Processor from Config
    processor_config = initialize_processor(args.verbose)

    # Start Application Server
    if args.socket:
        uvicorn.run(app, proxy_headers=True, uds=args.socket)
    else:
        uvicorn.run(app, host=args.host, port=args.port)
