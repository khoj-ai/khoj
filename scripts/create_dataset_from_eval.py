import argparse
import json
import logging
import os
import tempfile

import pandas as pd
from datasets import Dataset, DatasetDict, load_dataset
from dotenv import load_dotenv
from huggingface_hub import HfApi

load_dotenv()

logger = logging.getLogger(__name__)

SYSTEM_PROMPT = """
Your role as an assistant involves thoroughly exploring questions through a systematic long thinking process before providing the final precise and accurate solutions. This requires engaging in a comprehensive cycle of analysis, summarizing, exploration, reassessment, reflection, backtracing, and iteration to develop well-considered thinking process. Please structure your response into two main sections: Thought and Solution. In the Thought section, detail your reasoning process using the specified format: <|begin_of_thought|> {thought with steps separated with '\\n\\n'} <|end_of_thought|> Each step should include detailed considerations such as analisying questions, summarizing relevant findings, brainstorming new ideas, verifying the accuracy of the current steps, refining any errors, and revisiting previous steps.In the Solution section, based on various attempts, explorations, and reflections from the Thought section, systematically present the final solution that you deem correct. The solution should remain a logical, accurate, concise expression style and detail necessary step needed to reach the conclusion, formatted as follows: <|begin_of_solution|> {final formatted, precise, and clear solution} <|end_of_solution|>

Make sure to use the specific LaTeX math mode delimiters for your response. LaTex math mode specific delimiters as following
    - inline math mode : \\( and \\)
    - display math mode: insert linebreak after opening $$, \\[ and before closing $$, \\]

Now, try to solve the following question through the above guidelines"""


def load_dataset_from_jsonl(dataset_path: str) -> pd.DataFrame:
    """Load data trace from JSONL into pandas Dataframe."""
    if not os.path.exists(dataset_path):
        return Dataset.from_dict({"system": "", "conversations": []})

    try:
        # Read JSONL line by line to catch errors
        data = []
        with open(dataset_path, "r") as f:
            for i, line in enumerate(f, 1):
                try:
                    loaded_data = json.loads(line.strip())
                    if "system" in loaded_data:
                        loaded_data["system"] = SYSTEM_PROMPT
                    data.append(loaded_data)
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


def deduplicate_rows(good_rows: pd.DataFrame, parent_dataset: DatasetDict) -> pd.DataFrame:
    """
    Deduplicate the good_rows and parent_dataset. Within the `conversation` column, the first item in the list is the user prompt. The duplicate rows would have matching `value` fields in the user prompt.

    Args:
        good_rows: Dataframe with 'system' and 'conversations' columns
        parent_dataset: Dataframe with 'system' and 'conversations' columns

    Returns:
        Deduplicated rows
    """
    # Convert the good_rows and parent_dataset to JSON
    user_prompts = set(good_rows["conversations"].apply(lambda x: x[0]["value"]))

    logger.info(f"Found {len(good_rows)} rows in good_rows")

    logger.info(f"Found {len(user_prompts)} unique user prompts in good_rows")

    # Convert parent dataset to DataFrame and filter
    parent_df = parent_dataset.data["train"].to_pandas()

    logger.info(f"Found {len(parent_df)} rows in parent dataset")
    parent_filtered = parent_df[~parent_df["conversations"].apply(lambda x: x[0]["value"]).isin(user_prompts)]

    logger.info(f"Found {len(parent_filtered)} rows in parent dataset after filtering")

    # Combine filtered parent data with good rows
    return pd.concat([parent_filtered, good_rows], ignore_index=True)


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

        # Load in the parent dataset, stratos
        parent_dataset = load_dataset("bespokelabs/Bespoke-Stratos-17k", split=None)
        logger.info(f"Loaded parent dataset with {len(parent_dataset)} rows")

        # load full thoughts dataset
        fullthoughts_df = load_dataset_from_jsonl(fullthoughts_path)
        logger.info(f"Loaded fullthoughts dataset with {len(fullthoughts_df)} rows")

        # Convert to pandas and save as JSON
        with tempfile.TemporaryDirectory() as tmp_dir:
            json_path = os.path.join(tmp_dir, "data.json")
            # Dedupe the good_rows and parent_dataset.
            deduplicated_rows = deduplicate_rows(good_rows, parent_dataset)
            deduplicated_rows.to_json(json_path, orient="records", indent=2)

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
            good_rows = get_good_dataset_rows(fullthoughts_df, eval_df)
            deduplicated_rows = deduplicate_rows(good_rows, parent_dataset)

            with tempfile.TemporaryDirectory() as tmp_dir:
                json_path = os.path.join(tmp_dir, "data.json")
                deduplicated_rows.to_json(json_path, orient="records", indent=2)

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
