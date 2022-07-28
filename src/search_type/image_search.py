# Standard Packages
import argparse
import pathlib
import copy
import shutil

# External Packages
from sentence_transformers import SentenceTransformer, util
from PIL import Image
from tqdm import trange
import torch

# Internal Packages
from src.utils.helpers import resolve_absolute_path, load_model
import src.utils.exiftool as exiftool
from src.utils.config import ImageSearchModel
from src.utils.rawconfig import ImageContentConfig, ImageSearchConfig


def initialize_model(search_config: ImageSearchConfig):
    # Initialize Model
    torch.set_num_threads(4)

    # Load the CLIP model
    encoder = load_model(
        model_dir  = search_config.model_directory,
        model_name = search_config.encoder,
        model_type = SentenceTransformer)

    return encoder


def extract_entries(image_directories, verbose=0):
    image_names = []
    for image_directory in image_directories:
        image_directory = resolve_absolute_path(image_directory, strict=True)
        image_names.extend(list(image_directory.glob('*.jpg')))
        image_names.extend(list(image_directory.glob('*.jpeg')))

    if verbose > 0:
        image_directory_names = ', '.join([str(image_directory) for image_directory in image_directories])
        print(f'Found {len(image_names)} images in {image_directory_names}')
    return sorted(image_names)


def compute_embeddings(image_names, encoder, embeddings_file, batch_size=50, use_xmp_metadata=False, regenerate=False, verbose=0):
    "Compute (and Save) Embeddings or Load Pre-Computed Embeddings"

    image_embeddings = compute_image_embeddings(image_names, encoder, embeddings_file, batch_size, regenerate, verbose)
    image_metadata_embeddings = compute_metadata_embeddings(image_names, encoder, embeddings_file, batch_size, use_xmp_metadata, regenerate, verbose)

    return image_embeddings, image_metadata_embeddings


def compute_image_embeddings(image_names, encoder, embeddings_file, batch_size=50, regenerate=False, verbose=0):
    image_embeddings = None

    # Load pre-computed image embeddings from file if exists
    if resolve_absolute_path(embeddings_file).exists() and not regenerate:
        image_embeddings = torch.load(embeddings_file)
        if verbose > 0:
            print(f"Loaded pre-computed embeddings from {embeddings_file}")

    # Else compute the image embeddings from scratch, which can take a while
    if image_embeddings is None:
        image_embeddings = []
        for index in trange(0, len(image_names), batch_size):
            images = [Image.open(image_name) for image_name in image_names[index:index+batch_size]]
            image_embeddings += encoder.encode(
                images,
                convert_to_tensor=True,
                batch_size=min(len(images), batch_size))
        torch.save(image_embeddings, embeddings_file)
        if verbose > 0:
            print(f"Saved computed embeddings to {embeddings_file}")

    return image_embeddings


def compute_metadata_embeddings(image_names, encoder, embeddings_file, batch_size=50, use_xmp_metadata=False, regenerate=False, verbose=0):
    image_metadata_embeddings = None

    # Load pre-computed image metadata embedding file if exists
    if use_xmp_metadata and resolve_absolute_path(f"{embeddings_file}_metadata").exists() and not regenerate:
        image_metadata_embeddings = torch.load(f"{embeddings_file}_metadata")
        if verbose > 0:
            print(f"Loaded pre-computed embeddings from {embeddings_file}_metadata")

    # Else compute the image metadata embeddings from scratch, which can take a while
    if use_xmp_metadata and image_metadata_embeddings is None:
        image_metadata_embeddings = []
        for index in trange(0, len(image_names), batch_size):
            image_metadata = [extract_metadata(image_name, verbose) for image_name in image_names[index:index+batch_size]]
            try:
                image_metadata_embeddings += encoder.encode(
                    image_metadata,
                    convert_to_tensor=True,
                    batch_size=min(len(image_metadata), batch_size))
            except RuntimeError as e:
                print(f"Error encoding metadata for images starting from\n\tindex: {index},\n\timages: {image_names[index:index+batch_size]}\nException: {e}")
                continue
        torch.save(image_metadata_embeddings, f"{embeddings_file}_metadata")
        if verbose > 0:
            print(f"Saved computed metadata embeddings to {embeddings_file}_metadata")

    return image_metadata_embeddings


def extract_metadata(image_name, verbose=0):
    with exiftool.ExifTool() as et:
        image_metadata = et.get_tags(["XMP:Subject", "XMP:Description"], str(image_name))
        image_metadata_subjects = set([subject.split(":")[1] for subject in image_metadata.get("XMP:Subject", "") if ":" in subject])

        image_processed_metadata = image_metadata.get("XMP:Description", "")
        if len(image_metadata_subjects) > 0:
            image_processed_metadata += ". " + ", ".join(image_metadata_subjects)

        if verbose > 2:
            print(f"{image_name}:\t{image_processed_metadata}")

        return image_processed_metadata


def query(raw_query, count, model: ImageSearchModel):
    # Set query to image content if query is a filepath
    if pathlib.Path(raw_query).is_file():
        query_imagepath = resolve_absolute_path(pathlib.Path(raw_query), strict=True)
        query = copy.deepcopy(Image.open(query_imagepath))
        if model.verbose > 0:
            print(f"Find Images similar to Image at {query_imagepath}")
    else:
        query = raw_query
        if model.verbose > 0:
            print(f"Find Images by Text: {query}")

    # Now we encode the query (which can either be an image or a text string)
    query_embedding = model.image_encoder.encode([query], convert_to_tensor=True, show_progress_bar=False)

    # Compute top_k ranked images based on cosine-similarity b/w query and all image embeddings.
    image_hits = {result['corpus_id']: {'image_score': result['score'], 'score': result['score']}
                  for result
                  in util.semantic_search(query_embedding, model.image_embeddings, top_k=count)[0]}

    # Compute top_k ranked images based on cosine-similarity b/w query and all image metadata embeddings.
    if model.image_metadata_embeddings:
        metadata_hits = {result['corpus_id']: result['score']
                         for result
                         in util.semantic_search(query_embedding, model.image_metadata_embeddings, top_k=count)[0]}

        # Sum metadata, image scores of the highest ranked images
        for corpus_id, score in metadata_hits.items():
            scaling_factor = 0.33
            if 'corpus_id' in image_hits:
                image_hits[corpus_id].update({
                    'metadata_score': score,
                    'score': image_hits[corpus_id].get('score', 0) + scaling_factor*score,
                })
            else:
                image_hits[corpus_id] = {'metadata_score': score, 'score': scaling_factor*score}

    # Reformat results in original form from sentence transformer semantic_search()
    hits = [
        {
            'corpus_id': corpus_id,
            'score': scores['score'],
            'image_score': scores.get('image_score', 0),
            'metadata_score': scores.get('metadata_score', 0),
        } for corpus_id, scores in image_hits.items()]

    # Sort the images based on their combined metadata, image scores
    return sorted(hits, key=lambda hit: hit["score"], reverse=True)


def render_results(hits, image_names, image_directory, count):
    image_directory = resolve_absolute_path(image_directory, strict=True)

    for hit in hits[:count]:
        print(image_names[hit['corpus_id']])
        image_path = image_directory.joinpath(image_names[hit['corpus_id']])
        with Image.open(image_path) as img:
            img.show()


def collate_results(hits, image_names, output_directory, image_files_url, count=5):
    results = []

    for index, hit in enumerate(hits[:count]):
        source_path = image_names[hit['corpus_id']]

        target_image_name = f"{index}{source_path.suffix}"
        target_path = resolve_absolute_path(f"{output_directory}/{target_image_name}")

        # Create output directory, if it doesn't exist
        if not target_path.parent.exists():
            target_path.parent.mkdir(exist_ok=True)

        # Copy the image to the output directory
        shutil.copy(source_path, target_path)

        # Add the image metadata to the results
        results += [{
            "entry": f'{image_files_url}/{target_image_name}',
            "score": f"{hit['score']:.3f}",
            "image_score": f"{hit['image_score']:.3f}",
            "metadata_score": f"{hit['metadata_score']:.3f}",
        }]

    return results


def setup(config: ImageContentConfig, search_config: ImageSearchConfig, regenerate: bool, verbose: bool=False) -> ImageSearchModel:
    # Initialize Model
    encoder = initialize_model(search_config)

    # Extract Entries
    image_directories = [resolve_absolute_path(directory, strict=True) for directory in config.input_directories]
    image_names = extract_entries(image_directories, verbose)

    # Compute or Load Embeddings
    embeddings_file = resolve_absolute_path(config.embeddings_file)
    image_embeddings, image_metadata_embeddings = compute_embeddings(
        image_names,
        encoder,
        embeddings_file,
        batch_size=config.batch_size,
        regenerate=regenerate,
        use_xmp_metadata=config.use_xmp_metadata,
        verbose=verbose)

    return ImageSearchModel(image_names,
                            image_embeddings,
                            image_metadata_embeddings,
                            encoder,
                            verbose)


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
        hits = query(user_query, image_embeddings, image_metadata_embeddings, model, args.results_count, args.verbose)

        # render results
        render_results(hits, image_names, args.image_directory, count=args.results_count)
