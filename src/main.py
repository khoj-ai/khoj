# Standard Packages
import sys
import pathlib
from typing import Optional

# External Packages
import uvicorn
from fastapi import FastAPI

# Internal Packages
from search_type import asymmetric, symmetric_ledger, image_search
from utils.helpers import get_from_dict
from utils.cli import cli
from utils.config import SearchType, SearchSettings, SearchModels


# Application Global State
model = SearchModels()
search_settings = SearchSettings()
app = FastAPI()


@app.get('/search')
def search(q: str, n: Optional[int] = 5, t: Optional[SearchType] = None):
    if q is None or q == '':
        print(f'No query param (q) passed in API call to initiate search')
        return {}

    user_query = q
    results_count = n

    if (t == SearchType.Notes or t == None) and search_settings.notes_search_enabled:
        # query notes
        hits = asymmetric.query_notes(user_query, model.notes_search)

        # collate and return results
        return asymmetric.collate_results(hits, model.notes_search.entries, results_count)

    if (t == SearchType.Music or t == None) and search_settings.music_search_enabled:
        # query music library
        hits = asymmetric.query_notes(user_query, model.music_search)

        # collate and return results
        return asymmetric.collate_results(hits, model.music_search.entries, results_count)

    if (t == SearchType.Ledger or t == None) and search_settings.ledger_search_enabled:
        # query transactions
        hits = symmetric_ledger.query_transactions(user_query, model.ledger_search)

        # collate and return results
        return symmetric_ledger.collate_results(hits, model.ledger_search.entries, results_count)

    if (t == SearchType.Image or t == None) and search_settings.image_search_enabled:
        # query transactions
        hits = image_search.query_images(user_query, model.image_search, args.verbose)

        # collate and return results
        return image_search.collate_results(
            hits,
            model.image_search.image_names,
            image_config['input-directory'],
            results_count)

    else:
        return {}


@app.get('/regenerate')
def regenerate(t: Optional[SearchType] = None):
    if (t == SearchType.Notes or t == None) and search_settings.notes_search_enabled:
        # Extract Entries, Generate Embeddings
        models.notes_search = asymmetric.setup(
            org_config['input-files'],
            org_config['input-filter'],
            pathlib.Path(org_config['compressed-jsonl']),
            pathlib.Path(org_config['embeddings-file']),
            regenerate=True,
            verbose=args.verbose)

    if (t == SearchType.Music or t == None) and search_settings.music_search_enabled:
        # Extract Entries, Generate Song Embeddings
        model.music_search = asymmetric.setup(
            song_config['input-files'],
            song_config['input-filter'],
            pathlib.Path(song_config['compressed-jsonl']),
            pathlib.Path(song_config['embeddings-file']),
            regenerate=True,
            verbose=args.verbose)

    if (t == SearchType.Ledger or t == None) and search_settings.ledger_search_enabled:
        # Extract Entries, Generate Embeddings
        model.ledger_search = symmetric_ledger.setup(
            ledger_config['input-files'],
            ledger_config['input-filter'],
            pathlib.Path(ledger_config['compressed-jsonl']),
            pathlib.Path(ledger_config['embeddings-file']),
            regenerate=True,
            verbose=args.verbose)

    if (t == SearchType.Image or t == None) and search_settings.image_search_enabled:
        # Extract Images, Generate Embeddings
        model.image_search = image_search.setup(
            pathlib.Path(image_config['input-directory']),
            pathlib.Path(image_config['embeddings-file']),
            regenerate=True,
            verbose=args.verbose)

    return {'status': 'ok', 'message': 'regeneration completed'}


if __name__ == '__main__':
    args = cli(sys.argv[1:])

    # Initialize Org Notes Search
    org_config = get_from_dict(args.config, 'content-type', 'org')
    if org_config and ('input-files' in org_config or 'input-filter' in org_config):
        search_settings.notes_search_enabled = True
        model.notes_search = asymmetric.setup(
            org_config['input-files'],
            org_config['input-filter'],
            pathlib.Path(org_config['compressed-jsonl']),
            pathlib.Path(org_config['embeddings-file']),
            args.regenerate,
            args.verbose)

    # Initialize Org Music Search
    song_config = get_from_dict(args.config, 'content-type', 'music')
    music_search_enabled = False
    if song_config and ('input-files' in song_config or 'input-filter' in song_config):
        search_settings.music_search_enabled = True
        model.music_search = asymmetric.setup(
            song_config['input-files'],
            song_config['input-filter'],
            pathlib.Path(song_config['compressed-jsonl']),
            pathlib.Path(song_config['embeddings-file']),
            args.regenerate,
            args.verbose)

    # Initialize Ledger Search
    ledger_config = get_from_dict(args.config, 'content-type', 'ledger')
    if ledger_config and ('input-files' in ledger_config or 'input-filter' in ledger_config):
        search_settings.ledger_search_enabled = True
        model.ledger_search = symmetric_ledger.setup(
            ledger_config['input-files'],
            ledger_config['input-filter'],
            pathlib.Path(ledger_config['compressed-jsonl']),
            pathlib.Path(ledger_config['embeddings-file']),
            args.regenerate,
            args.verbose)

    # Initialize Image Search
    image_config = get_from_dict(args.config, 'content-type', 'image')
    if image_config and 'input-directory' in image_config:
        search_settings.image_search_enabled = True
        model.image_search = image_search.setup(
            pathlib.Path(image_config['input-directory']),
            pathlib.Path(image_config['embeddings-file']),
            batch_size=image_config['batch-size'],
            regenerate=args.regenerate,
            use_xmp_metadata={'yes': True, 'no': False}[image_config['use-xmp-metadata']],
            verbose=args.verbose)

    # Start Application Server
    uvicorn.run(app)
