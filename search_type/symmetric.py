import pandas as pd
import faiss
import numpy as np

from sentence_transformers import SentenceTransformer

import argparse
import os

def create_index(
        model,
        dataset_path,
        index_path,
        column_name,
        recreate):
    # Load Dataset
    dataset = pd.read_csv(dataset_path)

    # Clean Dataset
    dataset = dataset.dropna()
    dataset[column_name] = dataset[column_name].str.strip()

    # Create Index or Load it if it already exists
    if os.path.exists(index_path) and not recreate:
        index = faiss.read_index(index_path)
    else:
        # Create Embedding Vectors of Documents
        embeddings = model.encode(dataset[column_name].to_list(), show_progress_bar=True)
        embeddings = np.array([embedding for embedding in embeddings]).astype("float32")

        index = faiss.IndexIDMap(
            faiss.IndexFlatL2(
                embeddings.shape[1]))

        index.add_with_ids(embeddings, dataset.index.values)

        faiss.write_index(index, index_path)

    return index, dataset


def resolve_column(dataset, Id, column):
    return [list(dataset[dataset.index == idx][column]) for idx in Id[0]]


def vector_search(query, index, dataset, column_name, num_results=10):
    query_vector = np.array(query).astype("float32")
    D, Id = index.search(query_vector, k=num_results)

    return zip(D[0], Id[0], resolve_column(dataset, Id, column_name))

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Find most suitable match based on users exclude, include preferences")
    parser.add_argument('positives', type=str, help="Terms to find closest match to")
    parser.add_argument('--negatives', '-n', type=str, help="Terms to find farthest match from")

    parser.add_argument('--recreate', action='store_true', default=False, help="Recreate index at index_path from dataset at dataset path")
    parser.add_argument('--index', type=str, default="./.faiss_index", help="Path to index for storing vector embeddings")
    parser.add_argument('--dataset', type=str, default="./.dataset", help="Path to dataset to generate index from")
    parser.add_argument('--column', type=str, default="DATA", help="Name of dataset column to index")
    parser.add_argument('--num_results', type=int, default=10, help="Number of most suitable matches to show")
    parser.add_argument('--model_name', type=str, default='paraphrase-distilroberta-base-v1', help="Specify name of the SentenceTransformer model to use for encoding")
    args = parser.parse_args()

    model = SentenceTransformer(args.model_name)

    if args.positives and not args.negatives:
        # Get index, create it from dataset if doesn't exist
        index, dataset = create_index(model, args.dataset, args.index, args.column, args.recreate)

        # Create vector to represent user's stated positive preference
        preference_vector = model.encode([args.positives])

        # Find and display most suitable matches for users preferences in the dataset
        results = vector_search(preference_vector, index, dataset, args.column, args.num_results)

        print("Most Suitable Matches:")
        for similarity, id_, data in results:
            print(f"Id: {id_}\nSimilarity: {similarity}\n{args.column}: {data[0]}")

    elif args.positives and args.negatives:
        # Get index, create it from dataset if doesn't exist
        index, dataset = create_index(model, args.dataset, args.index, args.column, args.recreate)

        # Create vector to represent user's stated preference
        positives_vector = np.array(model.encode([args.positives])).astype("float32")
        negatives_vector = np.array(model.encode([args.negatives])).astype("float32")

        # preference_vector = np.mean([positives_vector, -1 * negatives_vector], axis=0)
        preference_vector = np.add(positives_vector, -1 * negatives_vector)

        # Find and display most suitable matches for users preferences in the dataset
        results = vector_search(preference_vector, index, dataset, args.column, args.num_results)

        print("Most Suitable Matches:")
        for similarity, id_, data in results:
            print(f"Id: {id_}\nSimilarity: {similarity}\n{args.column}: {data[0]}")
