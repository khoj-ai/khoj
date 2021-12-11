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
from src.utils.config import SearchType, SearchModels, ProcessorConfigModel, ConversationProcessorConfigModel
from src.utils.rawconfig import FullConfig
from src.processor.conversation.gpt import converse, message_to_log, message_to_prompt, understand

# Application Global State
config = FullConfig()
model = SearchModels()
processor_config = ProcessorConfigModel()
config_file = ""
verbose = 0
app = FastAPI()

app.mount("/views", StaticFiles(directory="views"), name="views")
templates = Jinja2Templates(directory="views/")

@app.get('/ui', response_class=HTMLResponse)
def ui(request: Request):
    return templates.TemplateResponse("config.html", context={'request': request})

@app.get('/config', response_model=FullConfig)
def config_data():
    return config

@app.post('/config')
async def config_data(updated_config: FullConfig):
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
            config.content_type.image.input_directory,
            results_count)

    else:
        return {}


@app.get('/regenerate')
def regenerate(t: Optional[SearchType] = None):
    initialize_search(config, regenerate=True, t=t)
    return {'status': 'ok', 'message': 'regeneration completed'}


@app.get('/chat')
def chat(q: str):
    # Load Conversation History
    chat_log = processor_config.conversation.chat_log
    meta_log = processor_config.conversation.meta_log

    # Converse with OpenAI GPT
    user_message_metadata = understand(q, api_key=processor_config.conversation.openai_api_key)
    gpt_response = converse(q, chat_log, api_key=processor_config.conversation.openai_api_key)

    # Update Conversation History
    processor_config.conversation.chat_log = message_to_prompt(q, chat_log, gpt_message=gpt_response)
    processor_config.conversation.meta_log= message_to_log(q, user_message_metadata, gpt_response, meta_log)

    return {'status': 'ok', 'response': gpt_response}


def initialize_search(config: FullConfig, regenerate: bool, t: SearchType = None):
    model = SearchModels()

    # Initialize Org Notes Search
    if (t == SearchType.Notes or t == None) and config.content_type.org:
        # Extract Entries, Generate Notes Embeddings
        model.notes_search = asymmetric.setup(config.content_type.org, regenerate=regenerate, verbose=verbose)

    # Initialize Org Music Search
    if (t == SearchType.Music or t == None) and config.content_type.music:
        # Extract Entries, Generate Music Embeddings
        model.music_search = asymmetric.setup(config.content_type.music, regenerate=regenerate, verbose=verbose)

    # Initialize Ledger Search
    if (t == SearchType.Ledger or t == None) and config.content_type.ledger:
        # Extract Entries, Generate Ledger Embeddings
        model.ledger_search = symmetric_ledger.setup(config.content_type.ledger, regenerate=regenerate, verbose=verbose)

    # Initialize Image Search
    if (t == SearchType.Image or t == None) and config.content_type.image:
        # Extract Entries, Generate Image Embeddings
        model.image_search = image_search.setup(config.content_type.image, regenerate=regenerate, verbose=verbose)

    return model


def initialize_processor(config: FullConfig):
    if not config.processor:
        return
    
    processor_config = ProcessorConfigModel()

    # Initialize Conversation Processor
    processor_config.conversation = ConversationProcessorConfigModel(config.processor.conversation, verbose)

    conversation_logfile = processor_config.conversation.conversation_logfile
    if processor_config.conversation.verbose:
        print('INFO:\tLoading conversation logs from disk...')

    if conversation_logfile.expanduser().absolute().is_file():
        # Load Metadata Logs from Conversation Logfile
        with open(get_absolute_path(conversation_logfile), 'r') as f:
            processor_config.conversation.meta_log = json.load(f)

        # Extract Chat Logs from Metadata
        processor_config.conversation.chat_log = ''.join(
            [f'\n{item["by"]}: {item["message"]}'
            for item
            in processor_config.conversation.meta_log])

        print('INFO:\tConversation logs loaded from disk.')
    else:
        # Initialize Conversation Logs
        processor_config.conversation.meta_log = []
        processor_config.conversation.chat_log = ""

    return processor_config


@app.on_event('shutdown')
def shutdown_event():
    # No need to create empty log file
    if not processor_config.conversation.meta_log:
        return
    elif processor_config.conversation.verbose:
        print('INFO:\tSaving conversation logs to disk...')

    # Save Conversation Metadata Logs to Disk
    conversation_logfile = get_absolute_path(processor_config.conversation.conversation_logfile)
    with open(conversation_logfile, "w+", encoding='utf-8') as logfile:
        json.dump(processor_config.conversation.meta_log, logfile)

    print('INFO:\tConversation logs saved to disk.')


if __name__ == '__main__':
    # Load config from CLI
    args = cli(sys.argv[1:])
    
    # Stores the file path to the config file.
    config_file = args.config_file

    # Store the verbose flag
    verbose = args.verbose

    # Store the raw config data.
    config = args.config

    # Initialize the search model from Config
    model = initialize_search(args.config, args.regenerate)

    # Initialize Processor from Config
    processor_config = initialize_processor(args.config)

    # Start Application Server
    if args.socket:
        uvicorn.run(app, proxy_headers=True, uds=args.socket)
    else:
        uvicorn.run(app, host=args.host, port=args.port)
