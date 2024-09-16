import uuid

from django.db import migrations, models


class Migration(migrations.Migration):
    dependencies = [
        ("database", "0064_populate_unique_id"),
    ]

    operations = [
        migrations.AlterField(
            model_name="conversation",
            name="unique_id",
            field=models.UUIDField(default=uuid.uuid4, editable=False, unique=True),
        ),
    ]
