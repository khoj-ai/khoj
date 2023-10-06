from django.db import migrations
from pgvector.django import VectorExtension


class Migration(migrations.Migration):
    dependencies = [
        ("database", "0003_user_khoj_configurations_and_more"),
    ]

    operations = [VectorExtension()]
