import argparse
import json
import logging
import os

import pandas as pd
from datasets import Dataset

logger = logging.getLogger(__name__)


def load_dataset_from_csv(dataset_path: str) -> pd.DataFrame:
    """Load data trace from CSV into pandas Dataframe."""
    if not os.path.exists(dataset_path):
        return Dataset.from_dict({"system": [], "conversations": []})

    # Read CSV
    csv_path = os.path.join(dataset_path, "data.csv")
    datatrace = pd.read_csv(csv_path)

    # Parse JSON strings in conversations column
    datatrace["conversations"] = datatrace["conversations"].apply(json.loads)

    return datatrace


def load_eval_results_from_csv(eval_results_path: str) -> pd.DataFrame:
    """
    Load evaluation results from CSV into Pandas DataFrame.
    CSV rows index,prompt,ground_truth,agent_response,evaluation_decision,evaluation_explanation,reasoning_type,usage,references

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
    successful_evals = eval[eval["success"] == True]

    # Create matching columns in datatrace
    datatrace["prompt"] = datatrace["conversations"].apply(lambda x: x[-2]["value"] if len(x) >= 2 else None)
    datatrace["agent_response"] = datatrace["conversations"].apply(lambda x: x[-1]["value"] if len(x) >= 2 else None)

    # Merge datatrace with successful evaluations
    good_rows = pd.merge(
        datatrace, successful_evals[["prompt", "agent_response"]], on=["prompt", "agent_response"], how="inner"
    )

    # Drop temporary columns and return
    return good_rows.drop(columns=["prompt", "agent_response"])


def main():
    # Set up argument parser
    parser = argparse.ArgumentParser(description="Create filtered dataset from evaluation results")
    parser.add_argument("--datatrace_path", type=str, required=True, help="Path to datatrace CSV")
    parser.add_argument("--eval_path", type=str, required=True, help="Path to evaluation results CSV")
    parser.add_argument("--output_path", type=str, required=True, help="Path to save filtered dataset")
    args = parser.parse_args()

    try:
        # Load data
        datatrace_df = load_dataset_from_csv(args.datatrace_path)
        eval_df = load_eval_results_from_csv(args.eval_path)

        # Get filtered rows
        good_rows = get_good_dataset_rows(datatrace_df, eval_df)

        # Convert to HF Dataset
        dataset = Dataset.from_pandas(good_rows)

        # Save dataset
        dataset.save_to_disk(args.output_path)
        logging.info(f"Saved filtered dataset with {len(dataset)} rows to {args.output_path}")

    except Exception as e:
        logging.error(f"Error processing dataset: {str(e)}")
        raise


if __name__ == "__main__":
    main()
