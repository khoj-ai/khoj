import base64
import io

from django.core.management.base import BaseCommand
from PIL import Image

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
            for chat in conversation.conversation_log.get("chat", []):
                if (
                    chat.get("by", "") == "khoj"
                    and chat.get("intent", {}).get("type", "") == ImageIntentType.TEXT_TO_IMAGE.value
                    and not options["reverse"]
                ):
                    # Decode the base64 encoded PNG image
                    print("Decode the base64 encoded PNG image")
                    decoded_image = base64.b64decode(chat["message"])

                    # Convert images from PNG to WebP format
                    print("Convert images from PNG to WebP format")
                    image_io = io.BytesIO(decoded_image)
                    with Image.open(image_io) as png_image:
                        webp_image_io = io.BytesIO()
                        png_image.save(webp_image_io, "WEBP")

                        # Encode the WebP image back to base64
                        webp_image_bytes = webp_image_io.getvalue()
                        chat["message"] = base64.b64encode(webp_image_bytes).decode()
                        chat["intent"]["type"] = ImageIntentType.TEXT_TO_IMAGE_V3.value
                        webp_image_io.close()
                    conversation_updated = True
                    updated_count += 1

                elif (
                    chat.get("by", "") == "khoj"
                    and chat.get("intent", {}).get("type", "") == ImageIntentType.TEXT_TO_IMAGE_V3.value
                    and options["reverse"]
                ):
                    # Decode the base64 encoded WebP image
                    print("Decode the base64 encoded WebP image")
                    decoded_image = base64.b64decode(chat["message"])

                    # Convert images from WebP to PNG format
                    print("Convert images from WebP to PNG format")
                    image_io = io.BytesIO(decoded_image)
                    with Image.open(image_io) as png_image:
                        webp_image_io = io.BytesIO()
                        png_image.save(webp_image_io, "PNG")

                        # Encode the WebP image back to base64
                        webp_image_bytes = webp_image_io.getvalue()
                        chat["message"] = base64.b64encode(webp_image_bytes).decode()
                        chat["intent"]["type"] = ImageIntentType.TEXT_TO_IMAGE.value
                        webp_image_io.close()
                    conversation_updated = True
                    updated_count += 1

                elif (
                    chat.get("by", "") == "khoj"
                    and chat.get("intent", {}).get("type", "") == ImageIntentType.TEXT_TO_IMAGE2.value
                ):
                    if options["reverse"] and chat.get("message", "").endswith(".webp"):
                        # Convert WebP url to PNG url
                        print("Convert WebP url to PNG url")
                        chat["message"] = chat["message"].replace(".webp", ".png")
                        conversation_updated = True
                        updated_count += 1
                    elif chat.get("message", "").endswith(".png"):
                        # Convert PNG url to WebP url
                        print("Convert PNG url to WebP url")
                        chat["message"] = chat["message"].replace(".png", ".webp")
                        conversation_updated = True
                        updated_count += 1

            if conversation_updated:
                print("Save the updated conversation")
                conversation.save()

        if updated_count > 0 and options["reverse"]:
            self.stdout.write(self.style.SUCCESS(f"Successfully converted {updated_count} WebP images to PNG format."))
        elif updated_count > 0:
            self.stdout.write(self.style.SUCCESS(f"Successfully converted {updated_count} PNG images to WebP format."))
