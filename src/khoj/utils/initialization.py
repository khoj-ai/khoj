import logging
import os
from typing import Tuple

from khoj.database.adapters import ConversationAdapters
from khoj.database.models import (
    ChatModelOptions,
    KhojUser,
    OpenAIProcessorConversationConfig,
    ServerChatSettings,
    SpeechToTextModelOptions,
    TextToImageModelConfig,
)
from khoj.processor.conversation.utils import model_to_prompt_size, model_to_tokenizer
from khoj.utils.constants import (
    default_anthropic_chat_models,
    default_gemini_chat_models,
    default_offline_chat_models,
    default_openai_chat_models,
)

logger = logging.getLogger(__name__)


def initialization(interactive: bool = True):
    def _create_admin_user():
        logger.info(
            "👩‍✈️ Setting up admin user. These credentials will allow you to configure your server at /server/admin."
        )
        if not interactive and (not os.getenv("KHOJ_ADMIN_EMAIL") or not os.getenv("KHOJ_ADMIN_PASSWORD")):
            logger.error(
                "🚨 Admin user cannot be created. Please set the KHOJ_ADMIN_EMAIL, KHOJ_ADMIN_PASSWORD environment variables or start server in interactive mode."
            )
            exit(1)
        email_addr = os.getenv("KHOJ_ADMIN_EMAIL") or input("Email: ")
        password = os.getenv("KHOJ_ADMIN_PASSWORD") or input("Password: ")
        admin_user = KhojUser.objects.create_superuser(email=email_addr, username=email_addr, password=password)
        logger.info(f"👩‍✈️ Created admin user: {admin_user.email}")

    def _create_chat_configuration():
        logger.info(
            "🗣️ Configure chat models available to your server. You can always update these at /server/admin using your admin account"
        )

        # Set up OpenAI's online chat models
        openai_configured, openai_provider = _setup_chat_model_provider(
            ChatModelOptions.ModelType.OPENAI,
            default_openai_chat_models,
            default_api_key=os.getenv("OPENAI_API_KEY"),
            vision_enabled=True,
            is_offline=False,
            interactive=interactive,
        )

        # Setup OpenAI speech to text model
        if openai_configured:
            default_speech2text_model = "whisper-1"
            if interactive:
                openai_speech2text_model = input(
                    f"Enter the OpenAI speech to text model you want to use (default: {default_speech2text_model}): "
                )
                openai_speech2text_model = openai_speech2text_model or default_speech2text_model
            else:
                openai_speech2text_model = default_speech2text_model
            SpeechToTextModelOptions.objects.create(
                model_name=openai_speech2text_model, model_type=SpeechToTextModelOptions.ModelType.OPENAI
            )

        # Setup OpenAI text to image model
        if openai_configured:
            default_text_to_image_model = "dall-e-3"
            if interactive:
                openai_text_to_image_model = input(
                    f"Enter the OpenAI text to image model you want to use (default: {default_text_to_image_model}): "
                )
                openai_text_to_image_model = openai_text_to_image_model or default_text_to_image_model
            else:
                openai_text_to_image_model = default_text_to_image_model
            TextToImageModelConfig.objects.create(
                model_name=openai_text_to_image_model,
                model_type=TextToImageModelConfig.ModelType.OPENAI,
                openai_config=openai_provider,
            )

        # Set up Google's Gemini online chat models
        _setup_chat_model_provider(
            ChatModelOptions.ModelType.GOOGLE,
            default_gemini_chat_models,
            default_api_key=os.getenv("GEMINI_API_KEY"),
            vision_enabled=False,
            is_offline=False,
            interactive=interactive,
            provider_name="Google Gemini",
        )

        # Set up Anthropic's online chat models
        _setup_chat_model_provider(
            ChatModelOptions.ModelType.ANTHROPIC,
            default_anthropic_chat_models,
            default_api_key=os.getenv("ANTHROPIC_API_KEY"),
            vision_enabled=False,
            is_offline=False,
            interactive=interactive,
        )

        # Set up offline chat models
        _setup_chat_model_provider(
            ChatModelOptions.ModelType.OFFLINE,
            default_offline_chat_models,
            default_api_key=None,
            vision_enabled=False,
            is_offline=True,
            interactive=interactive,
        )

        # Explicitly set default chat model
        chat_models_configured = ChatModelOptions.objects.count()
        if chat_models_configured > 0:
            default_chat_model_name = ChatModelOptions.objects.first().chat_model
            # If there are multiple chat models, ask the user to choose the default chat model
            if chat_models_configured > 1 and interactive:
                user_chat_model_name = input(
                    f"Enter the default chat model to use (default: {default_chat_model_name}): "
                )
            else:
                user_chat_model_name = None

            # If the user's choice is valid, set it as the default chat model
            if user_chat_model_name and ChatModelOptions.objects.filter(chat_model=user_chat_model_name).exists():
                default_chat_model_name = user_chat_model_name

            # Create a server chat settings object with the default chat model
            default_chat_model = ChatModelOptions.objects.filter(chat_model=default_chat_model_name).first()
            ServerChatSettings.objects.create(chat_default=default_chat_model)
            logger.info("🗣️ Chat model configuration complete")

        # Set up offline speech to text model
        use_offline_speech2text_model = "n" if not interactive else input("Use offline speech to text model? (y/n): ")
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

    def _setup_chat_model_provider(
        model_type: ChatModelOptions.ModelType,
        default_chat_models: list,
        default_api_key: str,
        interactive: bool,
        vision_enabled: bool = False,
        is_offline: bool = False,
        provider_name: str = None,
    ) -> Tuple[bool, OpenAIProcessorConversationConfig]:
        supported_vision_models = ["gpt-4o-mini", "gpt-4o"]
        provider_name = provider_name or model_type.name.capitalize()
        default_use_model = {True: "y", False: "n"}[default_api_key is not None or is_offline]
        use_model_provider = (
            default_use_model if not interactive else input(f"Add {provider_name} chat models? (y/n): ")
        )

        if use_model_provider != "y":
            return False, None

        logger.info(f"️💬 Setting up your {provider_name} chat configuration")

        chat_model_provider = None
        if not is_offline:
            if interactive:
                user_api_key = input(f"Enter your {provider_name} API key (default: {default_api_key}): ")
                api_key = user_api_key if user_api_key != "" else default_api_key
            else:
                api_key = default_api_key
            chat_model_provider = OpenAIProcessorConversationConfig.objects.create(api_key=api_key, name=provider_name)

        if interactive:
            chat_model_names = input(
                f"Enter the {provider_name} chat models you want to use (default: {','.join(default_chat_models)}): "
            )
            chat_models = chat_model_names.split(",") if chat_model_names != "" else default_chat_models
            chat_models = [model.strip() for model in chat_models]
        else:
            chat_models = default_chat_models

        for chat_model in chat_models:
            default_max_tokens = model_to_prompt_size.get(chat_model)
            default_tokenizer = model_to_tokenizer.get(chat_model)
            vision_enabled = vision_enabled and chat_model in supported_vision_models

            chat_model_options = {
                "chat_model": chat_model,
                "model_type": model_type,
                "max_prompt_size": default_max_tokens,
                "vision_enabled": vision_enabled,
                "tokenizer": default_tokenizer,
                "openai_config": chat_model_provider,
            }

            ChatModelOptions.objects.create(**chat_model_options)

        logger.info(f"🗣️ {provider_name} chat model configuration complete")
        return True, chat_model_provider

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
            # Some environments don't support interactive input. We catch the exception and return if that's the case.
            # The admin can still configure their settings from the admin page.
            except EOFError:
                return
            except Exception as e:
                logger.error(f"🚨 Failed to create chat configuration: {e}", exc_info=True)
