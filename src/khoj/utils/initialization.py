import logging
import os

from khoj.database.models import (
    KhojUser,
    OfflineChatProcessorConversationConfig,
    OpenAIProcessorConversationConfig,
    ChatModelOptions,
)

from khoj.utils.constants import default_offline_chat_model, default_online_chat_model
from khoj.processor.conversation.utils import model_to_prompt_size, model_to_tokenizer

from khoj.database.adapters import ConversationAdapters


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
            # Note: gpt4all package is not available on all devices.
            # So ensure gpt4all package is installed before continuing this step.
            import gpt4all

            use_offline_model = input("Use offline chat model? (y/n): ")
            if use_offline_model == "y":
                logger.info("🗣️ Setting up offline chat model")
                OfflineChatProcessorConversationConfig.objects.create(enabled=True)

                offline_chat_model = input(
                    f"Enter the offline chat model you want to use, See GPT4All for supported models (default: {default_offline_chat_model}): "
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

        use_openai_model = input("Use OpenAI chat model? (y/n): ")

        if use_openai_model == "y":
            logger.info("🗣️ Setting up OpenAI chat model")
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

        logger.info("🗣️  Chat model configuration complete")

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
