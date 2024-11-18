import argparse
import concurrent.futures
import json
import logging
import os
import time
from datetime import datetime
from io import StringIO
from typing import Any, Dict

import pandas as pd
import requests
from datasets import Dataset, load_dataset

from khoj.utils.helpers import is_none_or_empty, timer

# Configure root logger
logging.basicConfig(level=logging.INFO, format="%(message)s")
logger = logging.getLogger(__name__)

# Configuration
KHOJ_URL = os.getenv("KHOJ_URL", "http://localhost:42110")
KHOJ_CHAT_API_URL = f"{KHOJ_URL}/api/chat"
KHOJ_API_KEY = os.getenv("KHOJ_API_KEY")
KHOJ_MODE = os.getenv("KHOJ_MODE", "default")  # E.g research, general, notes etc.

GEMINI_API_KEY = os.getenv("GEMINI_API_KEY")
GEMINI_EVAL_MODEL = os.getenv("GEMINI_EVAL_MODEL", "gemini-1.5-pro-002")
GEMINI_API_URL = (
    f"https://generativelanguage.googleapis.com/v1beta/models/{GEMINI_EVAL_MODEL}:generateContent?key={GEMINI_API_KEY}"
)

SAMPLE_SIZE = os.getenv("SAMPLE_SIZE")  # Number of examples to evaluate
RANDOMIZE = os.getenv("RANDOMIZE", "false").lower() == "true"  # Randomize examples
BATCH_SIZE = int(
    os.getenv("BATCH_SIZE", int(SAMPLE_SIZE) / 10 if SAMPLE_SIZE else 10)
)  # Examples to evaluate in each batch
SLEEP_SECONDS = 3 if KHOJ_MODE == "general" else 1  # Sleep between API calls to avoid rate limiting


def load_frames_dataset():
    """
    Load the Google FRAMES benchmark dataset from HuggingFace

    FRAMES is a benchmark dataset to evaluate retrieval and answering capabilities of agents.
    It contains ~800 requiring multi-hop retrieval and reasoning across various topics.

    ### Data Fields
    - Prompt: The question to be answered
    - Answer: The ground truth answer
    - reasoning_types: The type of reasoning required to answer the question
    """
    try:
        dataset = load_dataset("google/frames-benchmark")
        # Use test split for evaluation. Sample and shuffle dataset if configured
        dataset = dataset.shuffle() if RANDOMIZE else dataset
        return dataset["test"][: int(SAMPLE_SIZE)] if SAMPLE_SIZE else dataset["test"]

    except Exception as e:
        logger.error(f"Error loading dataset: {e}")
        return None


def load_simpleqa_dataset():
    """
    Load the OpenAI SimpleQA benchmark dataset from their public bucket.

    SimpleQA is a dataset of moderately difficult q&a for 2024 models to answer across various topics.
    It contains ~4000 human vetted questions and answers with additional metadata.
    Its usage can be seen in openai/simple-evals github repository as well.

    ### Data Fields
    - problem: The question to be answered
    - answer: The ground truth answer
    - metadata: Additional metadata including topic information
    """

    try:
        # Load SimpleQA benchmark from OpenAI public bucket
        raw_url = "https://openaipublic.blob.core.windows.net/simple-evals/simple_qa_test_set.csv"
        response = requests.get(raw_url)
        response.raise_for_status()

        # Parse benchmark from raw CSV response
        csv_data = pd.read_csv(StringIO(response.text))
        # Normalize it into FRAMES format
        formatted_data = [
            {
                "Prompt": d["problem"],
                "Answer": d["answer"],
                "reasoning_types": json.loads(csv_data.to_dict("records")[0]["metadata"].replace("'", '"'))["topic"],
            }
            for d in csv_data.to_dict("records")
        ]

        # Convert benchmark to HF Dataset
        dataset = Dataset.from_list(formatted_data)
        dataset = dataset.shuffle() if RANDOMIZE else dataset
        dataset = dataset.select(range(int(SAMPLE_SIZE))) if SAMPLE_SIZE else dataset

        return dataset
    except Exception as e:
        logger.error(f"Error loading simpleqa dataset: {e}")
        return None


def get_agent_response(prompt: str) -> str:
    """Get response from the Khoj API"""
    try:
        response = requests.post(
            KHOJ_CHAT_API_URL,
            headers={"Content-Type": "application/json", "Authorization": f"Bearer {KHOJ_API_KEY}"},
            json={
                "q": prompt,
                "create_new": True,
            },
        )
        response.raise_for_status()
        return response.json().get("response", "")
    except Exception as e:
        logger.error(f"Error getting agent response: {e}")
        return ""


def evaluate_response(query: str, agent_response: str, ground_truth: str) -> Dict[str, Any]:
    """Evaluate Khoj response against benchmark ground truth using Gemini"""
    evaluation_prompt = f"""
    Compare the following agent response with the ground truth answer.
    Determine if the agent response contains the key information from the ground truth.
    Focus on factual correctness rather than exact wording.

    Query: {query}
    Agent Response: {agent_response}
    Ground Truth: {ground_truth}

    Provide your evaluation in the following json format:
    {"explanation:" "[How you made the decision?)", "decision:" "(TRUE if response contains key information, FALSE otherwise)"}
    """

    try:
        response = requests.post(
            GEMINI_API_URL,
            headers={"Content-Type": "application/json"},
            json={
                "contents": [{"parts": [{"text": evaluation_prompt}]}],
                "generationConfig": {"response_mime_type": "application/json"},
            },
        )
        response.raise_for_status()

        # Parse evaluation response
        eval_response: dict[str, str] = json.loads(
            clean_json(response.json()["candidates"][0]["content"]["parts"][0]["text"])
        )
        decision = str(eval_response.get("decision", "")).upper() == "TRUE"
        explanation = eval_response.get("explanation", "")
        # Handle evaluation service errors
        if "503 Service Error" in explanation:
            decision = None
        # Extract decision and explanation from structured response
        return decision, explanation
    except Exception as e:
        logger.error(f"Error in evaluation: {e}")
        return None, f"Evaluation failed: {str(e)}"


def process_batch(batch, batch_start, results, dataset_length):
    for idx, (prompt, answer, reasoning_type) in enumerate(batch):
        current_index = batch_start + idx
        logger.info(f"Processing example: {current_index}/{dataset_length}")

        # Trigger research mode if enabled
        prompt = f"/{KHOJ_MODE} {prompt}" if KHOJ_MODE and not prompt.startswith(f"/{KHOJ_MODE}") else prompt

        # Get agent response
        agent_response = get_agent_response(prompt)

        # Evaluate response
        if is_none_or_empty(agent_response):
            decision = None
            explanation = "Agent response is empty. This maybe due to a service error."
        else:
            decision, explanation = evaluate_response(prompt, agent_response, answer)

        # Store results
        results.append(
            {
                "index": current_index,
                "prompt": prompt,
                "ground_truth": answer,
                "agent_response": agent_response,
                "evaluation_decision": decision,
                "evaluation_explanation": explanation,
                "reasoning_type": reasoning_type,
            }
        )

        # Log results
        decision_color = {True: "green", None: "blue", False: "red"}[decision]
        colored_decision = color_text(str(decision), decision_color)
        logger.info(
            f"Decision: {colored_decision}\nQuestion: {prompt}\nExpected Answer: {answer}\nAgent Answer: {agent_response}\nExplanation: {explanation}\n"
        )

        time.sleep(SLEEP_SECONDS)  # Rate limiting


def color_text(text, color):
    colors = {
        "red": "\033[91m",  # Bright red
        "green": "\033[32m",  # Standard green
        "blue": "\033[34m",  # Bright blue
        "reset": "\033[0m",
    }
    return f"{colors[color]}{text}{colors['reset']}"


def clean_json(response: str):
    """Remove any markdown json codeblock and newline formatting if present. Useful for non schema enforceable models"""
    return response.strip().replace("\n", "").removeprefix("```json").removesuffix("```")


def parse_args():
    parser = argparse.ArgumentParser(description="Evaluate Khoj on a supported benchmark.")
    parser.add_argument(
        "--output",
        "-o",
        default=None,
        help="Path to store evaluation results CSV (default: [benchmark]_evaluation_results_[datetime].csv)",
    )
    parser.add_argument(
        "--dataset",
        "-d",
        default="frames",
        choices=["frames", "simpleqa"],
        help="Dataset to use for evaluation (default: frames)",
    )
    return parser.parse_args()


def main():
    # Initialize variables
    args = parse_args()
    dataset = None

    # Load dataset
    with timer(f"Loaded {args.dataset} dataset in", logger, log_level=logging.INFO):
        if args.dataset == "frames":
            dataset = load_frames_dataset()
        elif args.dataset == "simpleqa":
            dataset = load_simpleqa_dataset()
    if dataset is None:
        return

    # Initialize variables
    results = []
    dataset_length = len(dataset["Prompt"])

    # Process examples in batches
    with concurrent.futures.ThreadPoolExecutor() as executor:
        futures = []
        for i in range(0, dataset_length, BATCH_SIZE):
            batch_start = i
            batch = zip(
                dataset["Prompt"][i : i + BATCH_SIZE],
                dataset["Answer"][i : i + BATCH_SIZE],
                dataset["reasoning_types"][i : i + BATCH_SIZE],
            )
            futures.append(executor.submit(process_batch, batch, batch_start, results, dataset_length))

        # Wait for all futures to complete
        concurrent.futures.wait(futures)

    # Calculate metrics
    df = pd.DataFrame(results)
    eval_df = df.dropna(subset=["evaluation_decision"])  # Exclude rows with missing evaluation decision
    accuracy = (eval_df["evaluation_decision"] == True).mean()

    # Calculate accuracy by reasoning type
    reasoning_type_accuracy = eval_df.groupby("reasoning_type")["evaluation_decision"].apply(
        lambda x: (x == True).mean()
    )

    # Print summary
    colored_accuracy = color_text(f"{accuracy:.2%}", "blue")
    logger.info(f"\nOverall Accuracy: {colored_accuracy}")
    logger.info(f"\nAccuracy by Reasoning Type:\n{reasoning_type_accuracy}")

    # Save summary to file
    sample_type = f"Sampling Type: {SAMPLE_SIZE} samples." if SAMPLE_SIZE else "Whole dataset."
    sample_type += " Randomized." if RANDOMIZE else ""
    summary = (
        f"Overall Accuracy: {accuracy:.2%}\n\nAccuracy by Reasoning Type:\n{reasoning_type_accuracy}\n\n{sample_type}\n"
    )
    summary_file = args.output.replace(".csv", ".txt") if args.output else None
    summary_file = (
        summary_file or f"{args.dataset}_evaluation_summary_{datetime.now().strftime('%Y-%m-%d_%H-%M-%S')}.txt"
    )
    with open(summary_file, "w") as f:
        f.write(summary)

    # Save raw results to file
    output_file = args.output or f"{args.dataset}_evaluation_results_{datetime.now().strftime('%Y-%m-%d_%H-%M-%S')}.csv"
    df.to_csv(output_file, index=False)
    logger.info(f"Results saved to {summary_file}, {output_file}")


if __name__ == "__main__":
    """
    Evaluate Khoj on supported benchmarks.
    Response are evaluated by GEMINI_EVAL_MODEL (default: gemini-pro-1.5-002).

    Khoj should be running at KHOJ_URL (default: http://localhost:42110).
    The Gemini judge model is accessed via the Gemini API with your GEMINI_API_KEY.
    To evaluate Khoj in research mode, set the KHOJ_MODE environment variable to "research".

    Run the script using the following command:
    KHOJ_MODE="research" GEMINI_API_KEY="<your_gemini_api_key>" python eval_frames.py
    """
    logger.info(f"{datetime.now()} - Begin Quizzing Khoj.")
    with timer("Ran eval script in", logger, log_level=logging.INFO):
        main()
    logger.info(f"{datetime.now()} - End Quizzing Khoj.")
