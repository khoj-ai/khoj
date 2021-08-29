# Standard Packages
import argparse
import pathlib
import copy

# External Packages
from sentence_transformers import SentenceTransformer, util
from PIL import Image
import torch

# Internal Packages
from utils.helpers import get_absolute_path, resolve_absolute_path


def initialize_model():
    # Initialize Model
    torch.set_num_threads(4)
    model = SentenceTransformer('clip-ViT-B-32')  #Load the CLIP model
    return model


def extract_entries(image_directory, verbose=False):
    image_directory = resolve_absolute_path(image_directory, strict=True)
    image_names = list(image_directory.glob('*.jpg'))
    if verbose:
        print(f'Found {len(image_names)} images in {image_directory}')
    return image_names


def compute_embeddings(image_names, model, embeddings_file, regenerate=False, verbose=False):
    "Compute (and Save) Embeddings or Load Pre-Computed Embeddings"
    image_embeddings = None

    # Load pre-computed embeddings from file if exists
    if resolve_absolute_path(embeddings_file).exists() and not regenerate:
        image_embeddings = torch.load(embeddings_file)
        if verbose:
            print(f"Loaded pre-computed embeddings from {embeddings_file}")

    else:  # Else compute the image_embeddings from scratch, which can take a while
        images = []
        if verbose:
            print(f"Loading the {len(image_names)} images into memory")
        for image_name in image_names:
            images.append(copy.deepcopy(Image.open(image_name)))

        if len(images) > 0:
            image_embeddings = model.encode(images, batch_size=128, convert_to_tensor=True, show_progress_bar=True)
            torch.save(image_embeddings, embeddings_file)
            if verbose:
                print(f"Saved computed embeddings to {embeddings_file}")

    return image_embeddings


def query_images(query, image_embeddings, model, count=3, verbose=False):
    # Set query to image content if query is a filepath
    if pathlib.Path(query).is_file():
        query_imagepath = resolve_absolute_path(pathlib.Path(query), strict=True)
        query = copy.deepcopy(Image.open(query_imagepath))
        if verbose:
            print(f"Find Images similar to Image at {query_imagepath}")
    else:
        print(f"Find Images by Text: {query}")

    # Now we encode the query (which can either be an image or a text string)
    query_embedding = model.encode([query], convert_to_tensor=True, show_progress_bar=False)

    # Then, we use the util.semantic_search function, which computes the cosine-similarity
    # between the query embedding and all image embeddings.
    # It then returns the top_k highest ranked images, which we output
    hits = util.semantic_search(query_embedding, image_embeddings, top_k=count)[0]

    return hits


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


def setup(image_directory, embeddings_file, regenerate=False, verbose=False):
    # Initialize Model
    model = initialize_model()

    # Extract Entries
    image_directory = resolve_absolute_path(image_directory, strict=True)
    image_names = extract_entries(image_directory, verbose)

    # Compute or Load Embeddings
    embeddings_file = resolve_absolute_path(embeddings_file)
    image_embeddings = compute_embeddings(image_names, model, embeddings_file, regenerate=regenerate, verbose=verbose)

    return image_names, image_embeddings, model


if __name__ == '__main__':
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Semantic Search on Images")
    parser.add_argument('--image-directory', '-i', required=True, type=pathlib.Path, help="Image directory to query")
    parser.add_argument('--embeddings-file', '-e', default='image_embeddings.pt', type=pathlib.Path, help="File to save/load model embeddings to/from. Default: ./embeddings.pt")
    parser.add_argument('--regenerate', action='store_true', default=False, help="Regenerate embeddings of Images in Image Directory . Default: false")
    parser.add_argument('--results-count', '-n', default=5, type=int, help="Number of results to render. Default: 5")
    parser.add_argument('--interactive', action='store_true', default=False, help="Interactive mode allows user to run queries on the model. Default: true")
    parser.add_argument('--verbose', action='store_true', default=False, help="Show verbose conversion logs. Default: false")
    args = parser.parse_args()

    image_names, image_embeddings, model = setup(args.image_directory, args.embeddings_file, regenerate=args.regenerate)

    # Run User Queries on Entries in Interactive Mode
    while args.interactive:
        # get query from user
        user_query = input("Enter your query: ")
        if user_query == "exit":
            exit(0)

        # query images
        hits = query_images(user_query, image_embeddings, model, args.results_count, args.verbose)

        # render results
        render_results(hits, image_names, args.image_directory, count=args.results_count)
