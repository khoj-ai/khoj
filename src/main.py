# Standard Packages
import sys
from typing import Optional

# External Packages
import uvicorn
from fastapi import FastAPI

# Internal Packages
from src.search_type import asymmetric, symmetric_ledger, image_search
from src.utils.helpers import get_from_dict
from src.utils.cli import cli
from src.utils.config import SearchType, SearchModels, TextSearchConfig, ImageSearchConfig, SearchConfig


# Application Global State
model = SearchModels()
search_config = SearchConfig()
app = FastAPI()


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


def initialize_search(config, regenerate, verbose):
    model = SearchModels()
    search_config = SearchConfig()

    # Initialize Org Notes Search
    search_config.notes = TextSearchConfig.create_from_dictionary(config, ('content-type', 'org'), verbose)
    if search_config.notes:
        model.notes_search = asymmetric.setup(search_config.notes, regenerate=regenerate)

    # Initialize Org Music Search
    search_config.music = TextSearchConfig.create_from_dictionary(config, ('content-type', 'music'), verbose)
    if search_config.music:
        model.music_search = asymmetric.setup(search_config.music, regenerate=regenerate)

    # Initialize Ledger Search
    search_config.ledger = TextSearchConfig.create_from_dictionary(config, ('content-type', 'ledger'), verbose)
    if search_config.ledger:
        model.ledger_search = symmetric_ledger.setup(search_config.ledger, regenerate=regenerate)

    # Initialize Image Search
    search_config.image = ImageSearchConfig.create_from_dictionary(config, ('content-type', 'image'), verbose)
    if search_config.image:
        model.image_search = image_search.setup(search_config.image, regenerate=regenerate)

    return model, search_config


if __name__ == '__main__':
    # Load config from CLI
    args = cli(sys.argv[1:])

    # Initialize Search from Config
    model, search_config = initialize_search(args.config, args.regenerate, args.verbose)

    # Start Application Server
    if args.socket:
        uvicorn.run(app, proxy_headers=True, uds=args.socket)
    else:
        uvicorn.run(app, host=args.host, port=args.port)
