from django.core.management.base import BaseCommand
from tqdm import tqdm

from khoj.database.models import Conversation
from khoj.utils.helpers import ImageIntentType, is_none_or_empty


class Command(BaseCommand):
    help = "Serve Khoj generated images from a different URL."

    def add_arguments(self, parser):
        # Pass Source URL
        parser.add_argument(
            "--source",
            action="store",
            help="URL from which generated images are currently served.",
        )
        # Pass Destination URL
        parser.add_argument("--destination", action="store", help="URL to serve generated image from going forward.")

        # Add a new argument 'reverse' to the command
        parser.add_argument(
            "--reverse",
            action="store_true",
            help="Revert to serve generated images from source instead of destination URL.",
        )

    def handle(self, *args, **options):
        updated_count = 0
        if not options.get("source") or not options.get("destination"):
            self.stdout.write(
                self.style.ERROR(
                    "Set --source, --destination args to migrate serving images from source to destination URL."
                )
            )
            return

        destination = options["source"] if options["reverse"] else options["destination"]
        source = options["destination"] if options["reverse"] else options["source"]
        for conversation in Conversation.objects.all():
            conversation_updated = False
            for chat in tqdm(conversation.conversation_log.get("chat", []), desc="Processing Conversations"):
                if (
                    chat.get("by", "") == "khoj"
                    and not is_none_or_empty(chat.get("message"))
                    and chat.get("message", "").startswith(source)
                    and chat.get("intent", {}).get("type", "") == ImageIntentType.TEXT_TO_IMAGE2.value
                    and chat.get("message", "").endswith(".webp")
                ):
                    # Convert source url to destination url
                    chat["message"] = chat["message"].replace(source, destination)
                    conversation_updated = True
                    updated_count += 1

            if conversation_updated:
                print(f"Save the updated conversation {conversation.id} to the database.")
                conversation.save()

        if updated_count > 0:
            success = f"Successfully converted {updated_count} image URLs from {source} to {destination}.".strip()
            self.stdout.write(self.style.SUCCESS(success))
