from django.core.management.base import BaseCommand

from khoj.database.models import Conversation
from khoj.utils.helpers import ImageIntentType


class Command(BaseCommand):
    help = "Convert all images to WebP format or reverse."

    def add_arguments(self, parser):
        # Add a new argument 'reverse' to the command
        parser.add_argument(
            "--reverse",
            action="store_true",
            help="Convert from WebP to PNG instead of PNG to WebP",
        )

    def handle(self, *args, **options):
        updated_count = 0
        for conversation in Conversation.objects.all():
            conversation_updated = False
            for chat in conversation.conversation_log["chat"]:
                if chat["by"] == "khoj" and chat["intent"]["type"] == ImageIntentType.TEXT_TO_IMAGE2.value:
                    if options["reverse"] and chat["message"].endswith(".webp"):
                        # Convert WebP url to PNG url
                        chat["message"] = chat["message"].replace(".webp", ".png")
                        conversation_updated = True
                        updated_count += 1
                    elif chat["message"].endswith(".png"):
                        # Convert PNG url to WebP url
                        chat["message"] = chat["message"].replace(".png", ".webp")
                        conversation_updated = True
                        updated_count += 1
            if conversation_updated:
                conversation.save()

        if updated_count > 0 and options["reverse"]:
            self.stdout.write(self.style.SUCCESS(f"Successfully converted {updated_count} WebP images to PNG format."))
        elif updated_count > 0:
            self.stdout.write(self.style.SUCCESS(f"Successfully converted {updated_count} PNG images to WebP format."))
