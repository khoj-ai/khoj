import concurrent.futures
import json
import logging
import os
import time
from typing import Any, Dict

import pandas as pd
import requests
from datasets import load_dataset

from khoj.utils.helpers import timer

# Configure root logger
logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(message)s")
logger = logging.getLogger(__name__)

# Configuration
KHOJ_URL = os.getenv("KHOJ_URL", "http://localhost:42110")
KHOJ_CHAT_API_URL = f"{KHOJ_URL}/api/chat"
KHOJ_API_KEY = os.getenv("KHOJ_API_KEY")
KHOJ_MODE = os.getenv("KHOJ_MODE")  # E.g research, general, notes etc.

GEMINI_API_KEY = os.getenv("GEMINI_API_KEY")
GEMINI_EVAL_MODEL = os.getenv("GEMINI_EVAL_MODEL", "gemini-1.5-pro-002")
GEMINI_API_URL = (
    f"https://generativelanguage.googleapis.com/v1beta/models/{GEMINI_EVAL_MODEL}:generateContent?key={GEMINI_API_KEY}"
)

SAMPLE_SIZE = os.getenv("SAMPLE_SIZE")  # Number of examples to evaluate
RANDOMIZE = os.getenv("RANDOMIZE", "false").lower() == "true"  # Randomize examples
BATCH_SIZE = int(os.getenv("BATCH_SIZE", 10))  # Number of examples to evaluate in parallel
SLEEP_SECONDS = 1  # Delay between API calls to avoid rate limiting


def load_frames_dataset():
    """Load the FRAMES benchmark dataset from HuggingFace"""
    try:
        dataset = load_dataset("google/frames-benchmark")
        dataset = dataset.shuffle() if RANDOMIZE else dataset
        # Use test split for evaluation. Sample and shuffle dataset if configured
        return dataset["test"][: int(SAMPLE_SIZE)] if SAMPLE_SIZE else dataset["test"]

    except Exception as e:
        logger.error(f"Error loading dataset: {e}")
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
            headers={"Content-Type": "application/json", "response_mime_type": "application/json"},
            json={"contents": [{"parts": [{"text": evaluation_prompt}]}]},
        )
        response.raise_for_status()

        # Parse evaluation response
        eval_response = json.loads(clean_json(response.json()["candidates"][0]["content"]["parts"][0]["text"]))
        if "decision" in eval_response and isinstance(eval_response["decision"], str):
            eval_response["decision"] = eval_response["decision"].upper() == "TRUE"
        # Extract decision and explanation from structured response
        return {
            "decision": eval_response.get("decision", False),
            "explanation": eval_response.get("explanation", ""),
        }
    except Exception as e:
        logger.error(f"Error in evaluation: {e}")
        return {"decision": "FALSE", "explanation": f"Evaluation failed: {str(e)}"}


def process_batch(batch, batch_start, results, dataset_length):
    for idx, (prompt, answer, reasoning_type) in enumerate(batch):
        current_index = batch_start + idx
        logger.info(f"Processing example: {current_index}/{dataset_length}")

        # Trigger research mode if enabled
        prompt = f"/{KHOJ_MODE} {prompt}" if KHOJ_MODE else prompt

        # Get agent response
        agent_response = get_agent_response(prompt)

        # Evaluate response
        if agent_response is None or agent_response.strip() == "":
            evaluation["decision"] = False
            evaluation["explanation"] = "Agent response is empty. This maybe due to a service error."
        else:
            evaluation = evaluate_response(prompt, agent_response, answer)

        # Store results
        results.append(
            {
                "index": current_index,
                "prompt": prompt,
                "ground_truth": answer,
                "agent_response": agent_response,
                "evaluation_decision": evaluation["decision"],
                "evaluation_explanation": evaluation["explanation"],
                "reasoning_type": reasoning_type,
            }
        )

        # Color the decision based on its value
        decision = evaluation["decision"]
        decision_color = "green" if decision == True else "red"
        colored_decision = color_text(str(decision), decision_color)
        logger.info(
            f'Decision: {colored_decision}\nQuestion: {prompt}\nExpected Answer: {answer}\nAgent Answer: {agent_response}\nExplanation: {evaluation["explanation"]}\n'
        )

        time.sleep(SLEEP_SECONDS)  # Rate limiting


def color_text(text, color):
    colors = {"red": "\033[91m", "green": "\033[92m", "reset": "\033[0m"}
    return f"{colors[color]}{text}{colors['reset']}"


def clean_json(response: str):
    """Remove any markdown json codeblock and newline formatting if present. Useful for non schema enforceable models"""
    return response.strip().replace("\n", "").removeprefix("```json").removesuffix("```")


def main():
    # Load dataset
    with timer("Loaded dataset in", logger):
        dataset = load_frames_dataset()
    if dataset is None:
        return

    # Initialize variables
    counter = 0
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
    accuracy = (df["evaluation_decision"] == True).mean()

    # Calculate accuracy by reasoning type
    reasoning_type_accuracy = df.groupby("reasoning_type")["evaluation_decision"].apply(lambda x: (x == True).mean())

    # Save results
    df.to_csv("frames_evaluation_results.csv", index=False)

    # Print summary
    logger.info(f"\nOverall Accuracy: {accuracy:.2%}")
    logger.info("\nAccuracy by Reasoning Type:")
    logger.info(reasoning_type_accuracy)


if __name__ == "__main__":
    """
    Evaluate Khoj on the Google FRAMES benchmark.
    Response are evaluated by GEMINI_EVAL_MODEL (default: gemini-pro-1.5-002).

    Khoj should be running at KHOJ_URL, default at http://localhost:42110.
    The Gemini judge model is accessed via the Gemini API with your GEMINI_API_KEY.
    To evaluate Khoj in research mode, set the KHOJ_MODE environment variable to "research".

    Run the script using the following command:
    KHOJ_MODE="research" GEMINI_API_KEY="<your_gemini_api_key>" python eval_frames.py
    """
    with timer("Ran eval in", logger):
        main()
