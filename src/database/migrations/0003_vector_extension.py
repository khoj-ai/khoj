from django.db import migrations
from pgvector.django import VectorExtension


class Migration(migrations.Migration):
    dependencies = [
        ("database", "0002_googleuser"),
    ]

    operations = [VectorExtension()]
