# Made manually by sabaimran for use by Django 5.0.9 on 2024-12-01 16:59

from django.db import migrations, models

# This script was written alongside when Pydantic validation was added to the Conversation conversation_log field.


def migrate_generated_assets(apps, schema_editor):
    Conversation = apps.get_model("database", "Conversation")

    # Process conversations in chunks
    for conversation in Conversation.objects.iterator():
        try:
            meta_log = conversation.conversation_log
            modified = False

            for chat in meta_log.get("chat", []):
                intent_type = chat.get("intent", {}).get("type")

                if intent_type and chat["by"] == "khoj":
                    if intent_type and "text-to-image" in intent_type:
                        # Migrate the generated image to the new format
                        chat["images"] = [chat.get("message")]
                        chat["message"] = chat["intent"]["inferred-queries"][0]
                        modified = True

                    if intent_type and "excalidraw" in intent_type:
                        # Migrate the generated excalidraw to the new format
                        chat["excalidrawDiagram"] = chat.get("message")
                        chat["message"] = chat["intent"]["inferred-queries"][0]
                        modified = True

            # Only save if changes were made
            if modified:
                conversation.conversation_log = meta_log
                conversation.save()

        except Exception as e:
            print(f"Error processing conversation {conversation.id}: {str(e)}")
            continue


def reverse_migration(apps, schema_editor):
    Conversation = apps.get_model("database", "Conversation")

    # Process conversations in chunks
    for conversation in Conversation.objects.iterator():
        try:
            meta_log = conversation.conversation_log
            modified = False

            for chat in meta_log.get("chat", []):
                intent_type = chat.get("intent", {}).get("type")

                if intent_type and chat["by"] == "khoj":
                    if intent_type and "text-to-image" in intent_type:
                        # Migrate the generated image back to the old format
                        chat["message"] = chat.get("images", [])[0]
                        chat.pop("images", None)
                        modified = True

                    if intent_type and "excalidraw" in intent_type:
                        # Migrate the generated excalidraw back to the old format
                        chat["message"] = chat.get("excalidrawDiagram")
                        chat.pop("excalidrawDiagram", None)
                        modified = True

            # Only save if changes were made
            if modified:
                conversation.conversation_log = meta_log
                conversation.save()

        except Exception as e:
            print(f"Error processing conversation {conversation.id}: {str(e)}")
            continue


class Migration(migrations.Migration):
    dependencies = [
        ("database", "0074_alter_conversation_title"),
    ]

    operations = [
        migrations.RunPython(migrate_generated_assets, reverse_migration),
    ]
