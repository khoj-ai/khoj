import logging
import os

from khoj.database.adapters import ConversationAdapters
from khoj.database.models import (
    ChatModelOptions,
    KhojUser,
    OpenAIProcessorConversationConfig,
    SpeechToTextModelOptions,
    TextToImageModelConfig,
)
from khoj.processor.conversation.utils import model_to_prompt_size, model_to_tokenizer
from khoj.utils.constants import default_offline_chat_model, default_online_chat_model

logger = logging.getLogger(__name__)


def initialization():
    def _create_admin_user():
        logger.info(
            "👩‍✈️ Setting up admin user. These credentials will allow you to configure your server at /server/admin."
        )
        email_addr = os.getenv("KHOJ_ADMIN_EMAIL") or input("Email: ")
        password = os.getenv("KHOJ_ADMIN_PASSWORD") or input("Password: ")
        admin_user = KhojUser.objects.create_superuser(email=email_addr, username=email_addr, password=password)
        logger.info(f"👩‍✈️ Created admin user: {admin_user.email}")

    def _create_chat_configuration():
        logger.info(
            "🗣️  Configure chat models available to your server. You can always update these at /server/admin using the credentials of your admin account"
        )

        try:
            use_offline_model = input("Use offline chat model? (y/n): ")
            if use_offline_model == "y":
                logger.info("🗣️ Setting up offline chat model")

                offline_chat_model = input(
                    f"Enter the offline chat model you want to use. See HuggingFace for available GGUF models (default: {default_offline_chat_model}): "
                )
                if offline_chat_model == "":
                    ChatModelOptions.objects.create(
                        chat_model=default_offline_chat_model, model_type=ChatModelOptions.ModelType.OFFLINE
                    )
                else:
                    default_max_tokens = model_to_prompt_size.get(offline_chat_model, 2000)
                    max_tokens = input(
                        f"Enter the maximum number of tokens to use for the offline chat model (default {default_max_tokens}):"
                    )
                    max_tokens = max_tokens or default_max_tokens

                    default_tokenizer = model_to_tokenizer.get(
                        offline_chat_model, "hf-internal-testing/llama-tokenizer"
                    )
                    tokenizer = input(
                        f"Enter the tokenizer to use for the offline chat model (default: {default_tokenizer}):"
                    )
                    tokenizer = tokenizer or default_tokenizer

                    ChatModelOptions.objects.create(
                        chat_model=offline_chat_model,
                        model_type=ChatModelOptions.ModelType.OFFLINE,
                        max_prompt_size=max_tokens,
                        tokenizer=tokenizer,
                    )
        except ModuleNotFoundError as e:
            logger.warning("Offline models are not supported on this device.")

        use_openai_model = input("Use OpenAI models? (y/n): ")
        if use_openai_model == "y":
            logger.info("🗣️ Setting up your OpenAI configuration")
            api_key = input("Enter your OpenAI API key: ")
            OpenAIProcessorConversationConfig.objects.create(api_key=api_key)

            openai_chat_model = input(
                f"Enter the OpenAI chat model you want to use (default: {default_online_chat_model}): "
            )
            openai_chat_model = openai_chat_model or default_online_chat_model

            default_max_tokens = model_to_prompt_size.get(openai_chat_model, 2000)
            max_tokens = input(
                f"Enter the maximum number of tokens to use for the OpenAI chat model (default: {default_max_tokens}): "
            )
            max_tokens = max_tokens or default_max_tokens
            ChatModelOptions.objects.create(
                chat_model=openai_chat_model, model_type=ChatModelOptions.ModelType.OPENAI, max_prompt_size=max_tokens
            )

            default_speech2text_model = "whisper-1"
            openai_speech2text_model = input(
                f"Enter the OpenAI speech to text model you want to use (default: {default_speech2text_model}): "
            )
            openai_speech2text_model = openai_speech2text_model or default_speech2text_model
            SpeechToTextModelOptions.objects.create(
                model_name=openai_speech2text_model, model_type=SpeechToTextModelOptions.ModelType.OPENAI
            )

            default_text_to_image_model = "dall-e-3"
            openai_text_to_image_model = input(
                f"Enter the OpenAI text to image model you want to use (default: {default_text_to_image_model}): "
            )
            openai_speech2text_model = openai_text_to_image_model or default_text_to_image_model
            TextToImageModelConfig.objects.create(
                model_name=openai_text_to_image_model, model_type=TextToImageModelConfig.ModelType.OPENAI
            )

        if use_offline_model == "y" or use_openai_model == "y":
            logger.info("🗣️  Chat model configuration complete")

        use_offline_speech2text_model = input("Use offline speech to text model? (y/n): ")
        if use_offline_speech2text_model == "y":
            logger.info("🗣️ Setting up offline speech to text model")
            # Delete any existing speech to text model options. There can only be one.
            SpeechToTextModelOptions.objects.all().delete()

            default_offline_speech2text_model = "base"
            offline_speech2text_model = input(
                f"Enter the Whisper model to use Offline (default: {default_offline_speech2text_model}): "
            )
            offline_speech2text_model = offline_speech2text_model or default_offline_speech2text_model
            SpeechToTextModelOptions.objects.create(
                model_name=offline_speech2text_model, model_type=SpeechToTextModelOptions.ModelType.OFFLINE
            )

            logger.info(f"🗣️  Offline speech to text model configured to {offline_speech2text_model}")

    admin_user = KhojUser.objects.filter(is_staff=True).first()
    if admin_user is None:
        while True:
            try:
                _create_admin_user()
                break
            except Exception as e:
                logger.error(f"🚨 Failed to create admin user: {e}", exc_info=True)

    chat_config = ConversationAdapters.get_default_conversation_config()
    if admin_user is None and chat_config is None:
        while True:
            try:
                _create_chat_configuration()
                break
            # Some environments don't support interactive input. We catch the exception and return if that's the case. The admin can still configure their settings from the admin page.
            except EOFError:
                return
            except Exception as e:
                logger.error(f"🚨 Failed to create chat configuration: {e}", exc_info=True)
