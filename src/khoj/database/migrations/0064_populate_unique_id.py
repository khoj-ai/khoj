import uuid

from django.db import migrations


def populate_unique_id(apps, schema_editor):
    Conversation = apps.get_model("database", "Conversation")
    for conversation in Conversation.objects.all():
        conversation.unique_id = uuid.uuid4()
        conversation.save()


class Migration(migrations.Migration):
    dependencies = [
        ("database", "0063_conversation_add_unique_id_field"),
    ]

    operations = [
        migrations.RunPython(populate_unique_id),
    ]
