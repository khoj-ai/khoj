import base64
import io
import logging
import os
import time
from typing import Any, Callable, Dict, List, Optional

import openai
import requests
from google import genai
from google.genai import types as gtypes
from tenacity import (
    retry,
    retry_if_exception,
    retry_if_exception_type,
    stop_after_attempt,
    wait_random_exponential,
)
from tenacity.before_sleep import before_sleep_log

from khoj.database.adapters import ConversationAdapters
from khoj.database.models import (
    Agent,
    ChatMessageModel,
    Intent,
    KhojUser,
    TextToImageModelConfig,
)
from khoj.processor.conversation.google.utils import _is_retryable_error
from khoj.routers.helpers import ChatEvent, ImageShape, generate_better_image_prompt
from khoj.routers.storage import upload_generated_image_to_bucket
from khoj.utils import state
from khoj.utils.helpers import convert_image_to_webp, timer
from khoj.utils.rawconfig import LocationData

logger = logging.getLogger(__name__)


async def text_to_image(
    message: str,
    user: KhojUser,
    chat_history: List[ChatMessageModel],
    location_data: LocationData,
    references: List[Dict[str, Any]],
    online_results: Dict[str, Any],
    send_status_func: Optional[Callable] = None,
    query_images: Optional[List[str]] = None,
    agent: Agent = None,
    query_files: str = None,
    tracer: dict = {},
):
    status_code = 200
    image = None
    image_url = None

    text_to_image_config = await ConversationAdapters.aget_user_text_to_image_model(user)
    if not text_to_image_config:
        # If the user has not configured a text to image model, return an unsupported on server error
        status_code = 501
        message = "Failed to generate image. Setup image generation on the server."
        yield image_url or image, status_code, message
        return

    text2image_model = text_to_image_config.model_name
    image_chat_history: List[ChatMessageModel] = []
    default_intent = Intent(type="remember")
    for chat in chat_history[-4:]:
        if chat.by == "you":
            image_chat_history += [ChatMessageModel(by=chat.by, message=chat.message, intent=default_intent)]
        elif chat.by == "khoj" and chat.images and chat.intent and chat.intent.inferred_queries:
            image_chat_history += [
                ChatMessageModel(by=chat.by, message=chat.intent.inferred_queries[0], intent=default_intent)
            ]
        elif chat.by == "khoj" and chat.intent and chat.intent.type in ["remember", "reminder"]:
            image_chat_history += [ChatMessageModel(by=chat.by, message=chat.message, intent=default_intent)]

    if send_status_func:
        async for event in send_status_func("**Enhancing the Painting Prompt**"):
            yield {ChatEvent.STATUS: event}

    # Generate a better image prompt
    # Use the user's message, chat history, and other context
    image_prompt_response = await generate_better_image_prompt(
        message,
        image_chat_history,
        location_data=location_data,
        note_references=references,
        online_results=online_results,
        model_type=text_to_image_config.model_type,
        query_images=query_images,
        user=user,
        agent=agent,
        query_files=query_files,
        tracer=tracer,
    )
    image_prompt = image_prompt_response["description"]
    image_shape = image_prompt_response["shape"]

    if send_status_func:
        async for event in send_status_func(f"**Painting to Imagine**:\n{image_prompt}"):
            yield {ChatEvent.STATUS: event}

    # Generate image using the configured model and API
    with timer(f"Generate image with {text_to_image_config.model_type}", logger):
        try:
            if text_to_image_config.model_type == TextToImageModelConfig.ModelType.OPENAI:
                webp_image_bytes = generate_image_with_openai(
                    image_prompt, text_to_image_config, text2image_model, image_shape
                )
            elif text_to_image_config.model_type == TextToImageModelConfig.ModelType.STABILITYAI:
                webp_image_bytes = generate_image_with_stability(image_prompt, text_to_image_config, text2image_model)
            elif text_to_image_config.model_type == TextToImageModelConfig.ModelType.REPLICATE:
                webp_image_bytes = generate_image_with_replicate(
                    image_prompt, text_to_image_config, text2image_model, image_shape
                )
            elif text_to_image_config.model_type == TextToImageModelConfig.ModelType.GOOGLE:
                webp_image_bytes = generate_image_with_google(
                    image_prompt, text_to_image_config, text2image_model, image_shape
                )
        except openai.OpenAIError or openai.BadRequestError or openai.APIConnectionError as e:
            if "content_policy_violation" in e.message:
                logger.error(f"Image Generation blocked by OpenAI: {e}")
                status_code = e.status_code  # type: ignore
                message = "Image generation blocked by OpenAI due to policy violation"  # type: ignore
                yield image_url or image, status_code, message
                return
            else:
                logger.error(f"Image Generation failed with {e}", exc_info=True)
                message = "Image generation failed using OpenAI"  # type: ignore
                status_code = e.status_code  # type: ignore
                yield image_url or image, status_code, message
                return
        except ValueError as e:
            logger.error(f"Image Generation failed with {e}", exc_info=True)
            message = f"Image generation using {text2image_model} via {text_to_image_config.model_type} failed due to an unknown error"
            status_code = 500
            yield image_url or image, status_code, message
            return
        except requests.RequestException as e:
            logger.error(f"Image Generation failed with {e}", exc_info=True)
            message = f"Image generation using {text2image_model} via {text_to_image_config.model_type} failed due to a network error."
            status_code = 502
            yield image_url or image, status_code, message
            return

    # Decide how to store the generated image
    with timer("Upload image to S3", logger):
        image_url = upload_generated_image_to_bucket(webp_image_bytes, user.uuid)

    if not image_url:
        image = f"data:image/webp;base64,{base64.b64encode(webp_image_bytes).decode('utf-8')}"

    yield image_url or image, status_code, image_prompt


@retry(
    retry=(
        retry_if_exception_type(openai.APITimeoutError)
        | retry_if_exception_type(openai.APIError)
        | retry_if_exception_type(openai.APIConnectionError)
        | retry_if_exception_type(openai.RateLimitError)
        | retry_if_exception_type(openai.APIStatusError)
    ),
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def generate_image_with_openai(
    improved_image_prompt: str,
    text_to_image_config: TextToImageModelConfig,
    text2image_model: str,
    shape: ImageShape = ImageShape.SQUARE,
):
    "Generate image using OpenAI (compatible) API"

    # Get the API config from the user's configuration
    api_key = None
    if text_to_image_config.api_key:
        api_key = text_to_image_config.api_key
        openai_client = openai.OpenAI(api_key=api_key)
    elif text_to_image_config.ai_model_api:
        api_key = text_to_image_config.ai_model_api.api_key
        api_base_url = text_to_image_config.ai_model_api.api_base_url
        openai_client = openai.OpenAI(api_key=api_key, base_url=api_base_url)
    elif state.openai_client:
        openai_client = state.openai_client

    # Convert shape to size for OpenAI
    if shape == ImageShape.PORTRAIT:
        size = "1024x1536"
    elif shape == ImageShape.LANDSCAPE:
        size = "1536x1024"
    else:  # Square
        size = "1024x1024"

    # Generate image using OpenAI API
    OPENAI_IMAGE_GEN_STYLE = "vivid"
    response = openai_client.images.generate(
        prompt=improved_image_prompt,
        model=text2image_model,
        style=OPENAI_IMAGE_GEN_STYLE,
        size=size,
        response_format="b64_json",
    )

    # Extract the base64 image from the response
    image = response.data[0].b64_json
    # Decode base64 png and convert it to webp for faster loading
    return convert_image_to_webp(base64.b64decode(image))


@retry(
    retry=retry_if_exception_type(requests.RequestException),
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def generate_image_with_stability(
    improved_image_prompt: str, text_to_image_config: TextToImageModelConfig, text2image_model: str
):
    "Generate image using Stability AI"

    # Call Stability AI API to generate image
    response = requests.post(
        "https://api.stability.ai/v2beta/stable-image/generate/sd3",
        headers={"authorization": f"Bearer {text_to_image_config.api_key}", "accept": "image/*"},
        files={"none": ""},
        data={
            "prompt": improved_image_prompt,
            "model": text2image_model,
            "mode": "text-to-image",
            "output_format": "png",
            "aspect_ratio": "1:1",
        },
    )
    # Convert png to webp for faster loading
    return convert_image_to_webp(response.content)


@retry(
    retry=retry_if_exception_type(requests.RequestException),
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def generate_image_with_replicate(
    improved_image_prompt: str,
    text_to_image_config: TextToImageModelConfig,
    text2image_model: str,
    shape: ImageShape = ImageShape.SQUARE,
):
    "Generate image using Replicate API"

    # Convert shape to aspect ratio for Replicate
    # Replicate supports only 1:1, 3:4, and 4:3 aspect ratios
    if shape == ImageShape.PORTRAIT:
        aspect_ratio = "3:4"
    elif shape == ImageShape.LANDSCAPE:
        aspect_ratio = "4:3"
    else:  # Square
        aspect_ratio = "1:1"

    # Create image generation task on Replicate
    replicate_create_prediction_url = f"https://api.replicate.com/v1/models/{text2image_model}/predictions"
    headers = {
        "Authorization": f"Bearer {text_to_image_config.api_key}",
        "Content-Type": "application/json",
    }
    json = {
        "input": {
            "prompt": improved_image_prompt,
            "num_outputs": 1,
            "aspect_ratio": aspect_ratio,
            "output_format": "webp",
            "output_quality": 100,
        }
    }

    seed = int(os.getenv("KHOJ_LLM_SEED")) if os.getenv("KHOJ_LLM_SEED") else None
    if seed:
        json["input"]["seed"] = seed

    create_prediction = requests.post(replicate_create_prediction_url, headers=headers, json=json).json()

    # Get status of image generation task
    get_prediction_url = create_prediction["urls"]["get"]
    get_prediction = requests.get(get_prediction_url, headers=headers).json()
    status = get_prediction["status"]
    retry_count = 1

    # Poll the image generation task for completion status
    while status not in ["succeeded", "failed", "canceled"] and retry_count < 20:
        time.sleep(2)
        get_prediction = requests.get(get_prediction_url, headers=headers).json()
        status = get_prediction["status"]
        retry_count += 1

    # Raise exception if the image generation task fails
    if status != "succeeded":
        error = get_prediction.get("error")
        if retry_count >= 10:
            raise requests.RequestException("Image generation timed out")
        raise requests.RequestException(f"Image generation failed with status: {status}, message: {error}")

    # Get the generated image
    image_url = get_prediction["output"][0] if isinstance(get_prediction["output"], list) else get_prediction["output"]
    return io.BytesIO(requests.get(image_url).content).getvalue()


@retry(
    retry=retry_if_exception(_is_retryable_error),
    wait=wait_random_exponential(min=1, max=10),
    stop=stop_after_attempt(3),
    before_sleep=before_sleep_log(logger, logging.DEBUG),
    reraise=True,
)
def generate_image_with_google(
    improved_image_prompt: str,
    text_to_image_config: TextToImageModelConfig,
    text2image_model: str,
    shape: ImageShape = ImageShape.SQUARE,
):
    """Generate image using Google's AI over API"""

    # Initialize the Google AI client
    api_key = text_to_image_config.api_key or text_to_image_config.ai_model_api.api_key
    client = genai.Client(api_key=api_key)

    # Convert shape to aspect ratio for Google
    if shape == ImageShape.PORTRAIT:
        aspect_ratio = "3:4"
    elif shape == ImageShape.LANDSCAPE:
        aspect_ratio = "4:3"
    else:  # Square
        aspect_ratio = "1:1"

    # Configure image generation settings
    config = gtypes.GenerateImagesConfig(
        number_of_images=1,
        safety_filter_level=gtypes.SafetyFilterLevel.BLOCK_LOW_AND_ABOVE,
        person_generation=gtypes.PersonGeneration.ALLOW_ADULT,
        include_rai_reason=True,
        output_mime_type="image/png",
        aspect_ratio=aspect_ratio,
    )

    # Call the Gemini API to generate the image
    response = client.models.generate_images(model=text2image_model, prompt=improved_image_prompt, config=config)

    if not response.generated_images:
        raise ValueError("Failed to generate image using Google AI")

    # Extract the image bytes from the first generated image
    image_bytes = response.generated_images[0].image.image_bytes

    # Convert to webp for faster loading
    return convert_image_to_webp(image_bytes)
