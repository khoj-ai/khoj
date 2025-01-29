import argparse
import json
import logging
import os
import tempfile

import pandas as pd
from datasets import Dataset
from dotenv import load_dotenv
from huggingface_hub import HfApi

load_dotenv()

logger = logging.getLogger(__name__)


def load_dataset_from_jsonl(dataset_path: str) -> pd.DataFrame:
    """Load data trace from JSONL into pandas Dataframe."""
    if not os.path.exists(dataset_path):
        return Dataset.from_dict({"system": [], "conversations": []})

    try:
        # Read JSONL line by line to catch errors
        data = []
        with open(dataset_path, "r") as f:
            for i, line in enumerate(f, 1):
                try:
                    data.append(json.loads(line.strip()))
                except json.JSONDecodeError as e:
                    logger.error(f"Error on line {i}: {e}")
                    logger.error(f"Problematic line: {line.strip()}")
                    continue

        # Convert to DataFrame
        if not data:
            return Dataset.from_dict({"system": [], "conversations": []})
        return pd.DataFrame(data)

    except Exception as e:
        logger.error(f"Error processing dataset: {e}")
        return Dataset.from_dict({"system": [], "conversations": []})


def load_eval_results_from_csv(eval_results_paths: str | list[str]) -> pd.DataFrame:
    """
    Load evaluation results from one or more CSV files into a single Pandas DataFrame.

    Args:
        eval_results_paths: Single path string or list of paths to CSV files

    Returns:
        Combined DataFrame with all evaluation results
    """
    # Convert single path to list
    if isinstance(eval_results_paths, str):
        eval_results_paths = [eval_results_paths]

    # Initialize empty DataFrame
    combined_df = pd.DataFrame()

    # Load and concatenate each CSV file
    for path in eval_results_paths:
        if os.path.exists(path):
            df = pd.read_csv(path)
            combined_df = pd.concat([combined_df, df], ignore_index=True)

    return combined_df


def get_good_dataset_rows(datatrace: pd.DataFrame, eval: pd.DataFrame) -> pd.DataFrame:
    """
    Filter dataset rows to keep only those with successful evaluations.

    Args:
        dataset: Dataframe with 'system' and 'conversations' columns
        eval: Dataframe with 'prompt', 'agent_response' and 'success' columns

    Returns:
        Filtered data trace rows corresponding to the successful evaluations
    """

    # Filter successful evaluations
    successful_evals = eval[eval["evaluation_decision"] == 1.0]

    # Create matching columns in datatrace
    datatrace["prompt"] = datatrace["conversations"].apply(lambda x: x[-2]["value"] if len(x) >= 2 else None)

    # Merge datatrace with successful evaluations
    good_rows = pd.merge(datatrace, successful_evals[["prompt"]], on=["prompt"], how="inner")

    # Drop temporary columns and return
    return good_rows.drop(columns=["prompt"])


def main():
    # Set up argument parser
    parser = argparse.ArgumentParser(description="Create filtered dataset from evaluation results")
    datatrace_path = os.getenv("DATATRACE_PATH")
    fullthoughts_path = os.getenv("FULLTHOUGHTS_PATH")
    eval_paths = os.getenv("EVAL_PATH").split(",") if os.getenv("EVAL_PATH") else None
    output_path = os.getenv("OUTPUT_PATH")
    repo_name = os.getenv("REPO_NAME")
    thoughts_repo_name = os.getenv("THOUGHTS_REPO_NAME")

    # parser.add_argument("--datatrace_path", type=str, required=True, help="Path to datatrace CSV")
    # parser.add_argument("--eval_path", type=str, required=True, help="Path to evaluation results CSV")
    # parser.add_argument("--output_path", type=str, required=True, help="Path to save filtered dataset")
    # parser.add_argument("--repo_name", type=str, required=True, help="HuggingFace repo in user/dataset format")
    # args = parser.parse_args()

    hf_token = os.getenv("HF_TOKEN")

    try:
        # Load data
        datatrace_df = load_dataset_from_jsonl(datatrace_path)
        eval_df = load_eval_results_from_csv(eval_paths)

        # Get filtered rows
        good_rows = get_good_dataset_rows(datatrace_df, eval_df)

        # Convert to pandas and save as JSON
        with tempfile.TemporaryDirectory() as tmp_dir:
            json_path = os.path.join(tmp_dir, "data.json")
            good_rows.to_json(json_path, orient="records", indent=2)

            if repo_name and hf_token:
                # Initialize HF API
                api = HfApi(token=hf_token)

                # Upload JSON file
                api.upload_file(
                    path_or_fileobj=json_path,
                    path_in_repo="data.json",
                    repo_id=repo_name,
                    repo_type="dataset",
                    commit_message="Upload filtered dataset as JSON",
                )
                logging.info(f"Pushed JSON dataset with {len(good_rows)} rows to {repo_name}")
            elif output_path:
                # Save locally if no HF repo specified
                output_json = os.path.join(output_path, "data.json")
                good_rows.to_json(output_json, orient="records", indent=2)
                logging.info(f"Saved JSON dataset with {len(good_rows)} rows to {output_json}")

        if fullthoughts_path:
            # Load fullthoughts dataset
            fullthoughts_df = load_dataset_from_jsonl(fullthoughts_path)

            good_rows = get_good_dataset_rows(fullthoughts_df, eval_df)

            with tempfile.TemporaryDirectory() as tmp_dir:
                json_path = os.path.join(tmp_dir, "data.json")
                good_rows.to_json(json_path, orient="records", indent=2)

                if repo_name and hf_token:
                    api = HfApi(token=hf_token)
                    api.upload_file(
                        path_or_fileobj=json_path,
                        path_in_repo="data.json",
                        repo_id=thoughts_repo_name,
                        repo_type="dataset",
                        commit_message="Upload filtered dataset as JSON",
                    )
                    logging.info(f"Pushed JSON dataset with {len(fullthoughts_df)} rows to {repo_name}")
                elif output_path:
                    output_json = os.path.join(output_path, "data.json")
                    fullthoughts_df.to_json(output_json, orient="records", indent=2)
                    logging.info(f"Saved JSON dataset with {len(fullthoughts_df)} rows to {output_json}")

    except Exception as e:
        logging.error(f"Error processing dataset: {str(e)}")
        raise


if __name__ == "__main__":
    main()
