import logging
import os

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

        # Set up OpenAI's online models
        default_openai_api_key = os.getenv("OPENAI_API_KEY")
        default_use_openai_model = {True: "y", False: "n"}[default_openai_api_key != None]
        use_model_provider = default_use_openai_model if not interactive else input("Add OpenAI models? (y/n): ")
        if use_model_provider == "y":
            logger.info("️💬 Setting up your OpenAI configuration")
            if interactive:
                user_api_key = input(f"Enter your OpenAI API key (default: {default_openai_api_key}): ")
                api_key = user_api_key if user_api_key != "" else default_openai_api_key
            else:
                api_key = default_openai_api_key
            chat_model_provider = OpenAIProcessorConversationConfig.objects.create(api_key=api_key, name="OpenAI")

            if interactive:
                chat_model_names = input(
                    f"Enter the OpenAI chat models you want to use (default: {','.join(default_openai_chat_models)}): "
                )
                chat_models = chat_model_names.split(",") if chat_model_names != "" else default_openai_chat_models
                chat_models = [model.strip() for model in chat_models]
            else:
                chat_models = default_openai_chat_models

            # Add OpenAI chat models
            for chat_model in chat_models:
                vision_enabled = chat_model in ["gpt-4o-mini", "gpt-4o"]
                default_max_tokens = model_to_prompt_size.get(chat_model)
                ChatModelOptions.objects.create(
                    chat_model=chat_model,
                    model_type=ChatModelOptions.ModelType.OPENAI,
                    max_prompt_size=default_max_tokens,
                    openai_config=chat_model_provider,
                    vision_enabled=vision_enabled,
                )

            # Add OpenAI speech to text model
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

            # Add OpenAI text to image model
            default_text_to_image_model = "dall-e-3"
            if interactive:
                openai_text_to_image_model = input(
                    f"Enter the OpenAI text to image model you want to use (default: {default_text_to_image_model}): "
                )
                openai_text_to_image_model = openai_text_to_image_model or default_text_to_image_model
            else:
                openai_text_to_image_model = default_text_to_image_model
            TextToImageModelConfig.objects.create(
                model_name=openai_text_to_image_model, model_type=TextToImageModelConfig.ModelType.OPENAI
            )

        # Set up Google's Gemini online chat models
        default_gemini_api_key = os.getenv("GEMINI_API_KEY")
        default_use_gemini_model = {True: "y", False: "n"}[default_gemini_api_key != None]
        use_model_provider = default_use_gemini_model if not interactive else input("Add Google's chat models? (y/n): ")
        if use_model_provider == "y":
            logger.info("️💬 Setting up your Google Gemini configuration")
            if interactive:
                user_api_key = input(f"Enter your Gemini API key (default: {default_gemini_api_key}): ")
                api_key = user_api_key if user_api_key != "" else default_gemini_api_key
            else:
                api_key = default_gemini_api_key
            chat_model_provider = OpenAIProcessorConversationConfig.objects.create(api_key=api_key, name="Gemini")

            if interactive:
                chat_model_names = input(
                    f"Enter the Gemini chat models you want to use (default: {','.join(default_gemini_chat_models)}): "
                )
                chat_models = chat_model_names.split(",") if chat_model_names != "" else default_gemini_chat_models
                chat_models = [model.strip() for model in chat_models]
            else:
                chat_models = default_gemini_chat_models

            # Add Gemini chat models
            for chat_model in chat_models:
                default_max_tokens = model_to_prompt_size.get(chat_model)
                vision_enabled = False
                ChatModelOptions.objects.create(
                    chat_model=chat_model,
                    model_type=ChatModelOptions.ModelType.GOOGLE,
                    max_prompt_size=default_max_tokens,
                    openai_config=chat_model_provider,
                    vision_enabled=False,
                )

        # Set up Anthropic's online chat models
        default_anthropic_api_key = os.getenv("ANTHROPIC_API_KEY")
        default_use_anthropic_model = {True: "y", False: "n"}[default_anthropic_api_key != None]
        use_model_provider = (
            default_use_anthropic_model if not interactive else input("Add Anthropic's chat models? (y/n): ")
        )
        if use_model_provider == "y":
            logger.info("️💬 Setting up your Anthropic configuration")
            if interactive:
                user_api_key = input(f"Enter your Anthropic API key (default: {default_anthropic_api_key}): ")
                api_key = user_api_key if user_api_key != "" else default_anthropic_api_key
            else:
                api_key = default_anthropic_api_key
            chat_model_provider = OpenAIProcessorConversationConfig.objects.create(api_key=api_key, name="Anthropic")

            if interactive:
                chat_model_names = input(
                    f"Enter the Anthropic chat models you want to use (default: {','.join(default_anthropic_chat_models)}): "
                )
                chat_models = chat_model_names.split(",") if chat_model_names != "" else default_anthropic_chat_models
                chat_models = [model.strip() for model in chat_models]
            else:
                chat_models = default_anthropic_chat_models

            # Add Anthropic chat models
            for chat_model in chat_models:
                vision_enabled = False
                default_max_tokens = model_to_prompt_size.get(chat_model)
                ChatModelOptions.objects.create(
                    chat_model=chat_model,
                    model_type=ChatModelOptions.ModelType.ANTHROPIC,
                    max_prompt_size=default_max_tokens,
                    openai_config=chat_model_provider,
                    vision_enabled=False,
                )

        # Set up offline chat models
        use_model_provider = "y" if not interactive else input("Add Offline chat models? (y/n): ")
        if use_model_provider == "y":
            logger.info("️💬 Setting up Offline chat models")

            if interactive:
                chat_model_names = input(
                    f"Enter the offline chat models you want to use. See HuggingFace for available GGUF models (default: {','.join(default_offline_chat_models)}): "
                )
                chat_models = chat_model_names.split(",") if chat_model_names != "" else default_offline_chat_models
                chat_models = [model.strip() for model in chat_models]
            else:
                chat_models = default_offline_chat_models

            # Add chat models
            for chat_model in chat_models:
                default_max_tokens = model_to_prompt_size.get(chat_model)
                default_tokenizer = model_to_tokenizer.get(chat_model)
                ChatModelOptions.objects.create(
                    chat_model=chat_model,
                    model_type=ChatModelOptions.ModelType.OFFLINE,
                    max_prompt_size=default_max_tokens,
                    tokenizer=default_tokenizer,
                )

        chat_models_configured = ChatModelOptions.objects.count()

        # Explicitly set default chat model
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
