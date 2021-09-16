# Standard Packages
import argparse
import pathlib
import copy

# External Packages
from sentence_transformers import SentenceTransformer, util
from PIL import Image
from tqdm import trange
import torch

# Internal Packages
from utils.helpers import get_absolute_path, resolve_absolute_path
import utils.exiftool as exiftool

def initialize_model():
    # Initialize Model
    torch.set_num_threads(4)
    model = SentenceTransformer('clip-ViT-B-32')  #Load the CLIP model
    return model


def extract_entries(image_directory, verbose=0):
    image_directory = resolve_absolute_path(image_directory, strict=True)
    image_names = list(image_directory.glob('*.jpg'))
    image_names.extend(list(image_directory.glob('*.jpeg')))

    if verbose > 0:
        print(f'Found {len(image_names)} images in {image_directory}')
    return image_names


def compute_embeddings(image_names, model, embeddings_file, regenerate=False, verbose=0):
    "Compute (and Save) Embeddings or Load Pre-Computed Embeddings"
    image_embeddings = None
    image_metadata_embeddings = None

    # Load pre-computed image embeddings from file if exists
    if resolve_absolute_path(embeddings_file).exists() and not regenerate:
        image_embeddings = torch.load(embeddings_file)
        if verbose > 0:
            print(f"Loaded pre-computed embeddings from {embeddings_file}")

    # load pre-computed image metadata embedding file if exists
    if resolve_absolute_path(f"{embeddings_file}_metadata").exists() and not regenerate:
        image_metadata_embeddings = torch.load(f"{embeddings_file}_metadata")
        if verbose > 0:
            print(f"Loaded pre-computed embeddings from {embeddings_file}_metadata")

    if image_embeddings is None or image_metadata_embeddings is None:  # Else compute the image_embeddings from scratch, which can take a while
        if verbose > 0:
            print(f"Loading the {len(image_names)} images into memory")

    batch_size = 50
    if image_embeddings is None:
        image_embeddings = []
        for index in trange(0, len(image_names), batch_size):
            images = [Image.open(image_name) for image_name in image_names[index:index+batch_size]]
            image_embeddings += model.encode(images, convert_to_tensor=True, batch_size=batch_size)
        torch.save(image_embeddings, embeddings_file)
        if verbose > 0:
            print(f"Saved computed embeddings to {embeddings_file}")

    if image_metadata_embeddings is None:
        image_metadata = [extract_metadata(image_name, verbose) for image_name in image_names],
        image_metadata_embeddings = model.encode(image_metadata, batch_size=batch_size, convert_to_tensor=True, show_progress_bar=True)
        torch.save(image_metadata_embeddings, f"{embeddings_file}_metadata")
        if verbose > 0:
            print(f"Saved computed metadata embeddings to {embeddings_file}_metadata")

    return image_embeddings, image_metadata_embeddings


def extract_metadata(image_name, verbose=0):
    with exiftool.ExifTool() as et:
        image_metadata = et.get_tags(["XMP:Subject", "XMP:Description"], str(image_name))
        image_metadata_subjects = set([subject.split(":")[1] for subject in image_metadata.get("XMP:Subject", "") if ":" in subject])
        image_processed_metadata = image_metadata.get("XMP:Description", "") + ". " + ", ".join(image_metadata_subjects)
        if verbose > 1:
            print(f"{image_name}:\t{image_processed_metadata}")
        return image_processed_metadata


def query_images(query, image_embeddings, image_metadata_embeddings, model, count=3, verbose=0):
    # Set query to image content if query is a filepath
    if pathlib.Path(query).is_file():
        query_imagepath = resolve_absolute_path(pathlib.Path(query), strict=True)
        query = copy.deepcopy(Image.open(query_imagepath))
        if verbose > 0:
            print(f"Find Images similar to Image at {query_imagepath}")
    else:
        if verbose > 0:
            print(f"Find Images by Text: {query}")

    # Now we encode the query (which can either be an image or a text string)
    query_embedding = model.encode([query], convert_to_tensor=True, show_progress_bar=False)

    # Compute top_k ranked images based on cosine-similarity b/w query and all image embeddings.
    image_hits = {result['corpus_id']: result['score']
                  for result
                  in util.semantic_search(query_embedding, image_embeddings, top_k=count)[0]}

    # Compute top_k ranked images based on cosine-similarity b/w query and all image metadata embeddings.
    metadata_hits = {result['corpus_id']: result['score']
                     for result
                     in util.semantic_search(query_embedding, image_metadata_embeddings, top_k=count)[0]}

    # Sum metadata, image scores of the highest ranked images
    for corpus_id, score in metadata_hits.items():
        image_hits[corpus_id] = image_hits.get(corpus_id, 0) + score

    # Reformat results in original form from sentence transformer semantic_search()
    hits = [{'corpus_id': corpus_id, 'score': score} for corpus_id, score in image_hits.items()]

    # Sort the images based on their combined metadata, image scores
    return sorted(hits, key=lambda hit: hit["score"], reverse=True)


def render_results(hits, image_names, image_directory, count):
    image_directory = resolve_absolute_path(image_directory, strict=True)

    for hit in hits[:count]:
        print(image_names[hit['corpus_id']])
        image_path = image_directory.joinpath(image_names[hit['corpus_id']])
        with Image.open(image_path) as img:
            img.show()


def collate_results(hits, image_names, image_directory, count=5):
    image_directory = resolve_absolute_path(image_directory, strict=True)
    return [
        {
            "Entry": image_directory.joinpath(image_names[hit['corpus_id']]),
            "Score": f"{hit['score']:.3f}"
        }
        for hit
        in hits[0:count]]


def setup(image_directory, embeddings_file, regenerate=False, verbose=0):
    # Initialize Model
    model = initialize_model()

    # Extract Entries
    image_directory = resolve_absolute_path(image_directory, strict=True)
    image_names = extract_entries(image_directory, verbose)

    # Compute or Load Embeddings
    embeddings_file = resolve_absolute_path(embeddings_file)
    image_embeddings, image_metadata_embeddings = compute_embeddings(image_names, model, embeddings_file, regenerate=regenerate, verbose=verbose)

    return image_names, image_embeddings, image_metadata_embeddings, model


if __name__ == '__main__':
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Semantic Search on Images")
    parser.add_argument('--image-directory', '-i', required=True, type=pathlib.Path, help="Image directory to query")
    parser.add_argument('--embeddings-file', '-e', default='image_embeddings.pt', type=pathlib.Path, help="File to save/load model embeddings to/from. Default: ./embeddings.pt")
    parser.add_argument('--regenerate', action='store_true', default=False, help="Regenerate embeddings of Images in Image Directory . Default: false")
    parser.add_argument('--results-count', '-n', default=5, type=int, help="Number of results to render. Default: 5")
    parser.add_argument('--interactive', action='store_true', default=False, help="Interactive mode allows user to run queries on the model. Default: true")
    parser.add_argument('--verbose', action='count', default=0, help="Show verbose conversion logs. Default: 0")
    args = parser.parse_args()

    image_names, image_embeddings, image_metadata_embeddings, model = setup(args.image_directory, args.embeddings_file, regenerate=args.regenerate)

    # Run User Queries on Entries in Interactive Mode
    while args.interactive:
        # get query from user
        user_query = input("Enter your query: ")
        if user_query == "exit":
            exit(0)

        # query images
        hits = query_images(user_query, image_embeddings, image_metadata_embeddings, model, args.results_count, args.verbose)

        # render results
        render_results(hits, image_names, args.image_directory, count=args.results_count)
