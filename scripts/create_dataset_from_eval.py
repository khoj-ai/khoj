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

    # Read JSONL
    datatrace = pd.read_json(dataset_path, lines=True)

    return datatrace


def load_eval_results_from_csv(eval_results_path: str) -> pd.DataFrame:
    """
    Load evaluation results from CSV into Pandas DataFrame.99
    CSV rows should have the following columns:
    index,prompt,ground_truth,agent_response,evaluation_decision,evaluation_explanation,reasoning_type,usage


    """
    if not os.path.exists(eval_results_path):
        return pd.DataFrame()

    # Read CSV
    eval = pd.read_csv(eval_results_path)

    return eval


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
    eval_path = os.getenv("EVAL_PATH")
    output_path = os.getenv("OUTPUT_PATH")
    repo_name = os.getenv("REPO_NAME")

    # parser.add_argument("--datatrace_path", type=str, required=True, help="Path to datatrace CSV")
    # parser.add_argument("--eval_path", type=str, required=True, help="Path to evaluation results CSV")
    # parser.add_argument("--output_path", type=str, required=True, help="Path to save filtered dataset")
    # parser.add_argument("--repo_name", type=str, required=True, help="HuggingFace repo in user/dataset format")
    # args = parser.parse_args()

    hf_token = os.getenv("HF_TOKEN")

    try:
        # Load data
        datatrace_df = load_dataset_from_jsonl(datatrace_path)
        eval_df = load_eval_results_from_csv(eval_path)

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

    except Exception as e:
        logging.error(f"Error processing dataset: {str(e)}")
        raise


if __name__ == "__main__":
    main()
