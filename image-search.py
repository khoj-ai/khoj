import sentence_transformers
from sentence_transformers import SentenceTransformer, util
from PIL import Image
import glob
import torch
import argparse
import pathlib
import copy


def initialize_model():
    # Initialize Model
    torch.set_num_threads(4)
    top_k = 3
    model = SentenceTransformer('clip-ViT-B-32')  #Load the CLIP model
    return model, top_k


def extract_entries(image_directory, verbose=False):
    image_names = glob.glob(f'{image_directory.expanduser()}/*.jpg')
    if verbose:
        print(f'Found {len(image_names)} images in {image_directory}')
    return image_names


def compute_embeddings(image_names, model, embeddings_file, verbose=False):
    "Compute (and Save) Embeddings or Load Pre-Computed Embeddings"

    # Load pre-computed embeddings from file if exists
    if embeddings_file.exists():
        image_embeddings = torch.load(embeddings_file.expanduser())
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
            torch.save(image_embeddings, embeddings_file.expanduser())
            if verbose:
                print(f"Saved computed embeddings to {embeddings_file}")

    return image_embeddings


def search(query, image_embeddings, model, count=3):
    # First, we encode the query (which can either be an image or a text string)
    query_embedding = model.encode([query], convert_to_tensor=True, show_progress_bar=False)

    # Then, we use the util.semantic_search function, which computes the cosine-similarity
    # between the query embedding and all image embeddings.
    # It then returns the top_k highest ranked images, which we output
    hits = util.semantic_search(query_embedding, image_embeddings, top_k=count)[0]

    return hits


def render_results(hits, image_names, image_directory, count):
    for hit in hits[:count]:
        print(image_names[hit['corpus_id']])
        image_path = image_directory.joinpath(image_names[hit['corpus_id']]).expanduser()
        with Image.open(image_path) as img:
            img.show()


if __name__ == '__main__':
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Semantic Search on Images")
    parser.add_argument('--image-directory', '-i', required=True, type=pathlib.Path, help="Image directory to query")
    parser.add_argument('--embeddings-file', '-e', default='embeddings.pt', type=pathlib.Path, help="File to save/load model embeddings to/from. Default: ./embeddings.pt")
    parser.add_argument('--results-count', '-n', default=5, type=int, help="Number of results to render. Default: 5")
    parser.add_argument('--interactive', action='store_true', default=False, help="Interactive mode allows user to run queries on the model. Default: true")
    parser.add_argument('--verbose', action='store_true', default=False, help="Show verbose conversion logs. Default: false")
    args = parser.parse_args()

    # Initialize Model
    model, count = initialize_model()

    # Extract Entries
    image_names = extract_entries(args.image_directory, args.verbose)

    # Compute or Load Embeddings
    image_embeddings = compute_embeddings(image_names, model, args.embeddings_file, args.verbose)

    # Run User Queries on Entries in Interactive Mode
    while args.interactive:
        # get query from user
        user_query = input("Enter your query: ")
        if user_query == "exit":
            exit(0)

        # query notes
        hits = search(user_query, image_embeddings, model, args.results_count)

        # render results
        render_results(hits, image_names, args.image_directory, count=args.results_count)
