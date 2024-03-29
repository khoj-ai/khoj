# Generated by Django 4.2.5 on 2023-10-13 19:28

import django.db.models.deletion
from django.db import migrations, models


class Migration(migrations.Migration):
    dependencies = [
        ("database", "0005_embeddings_corpus_id"),
    ]

    operations = [
        migrations.CreateModel(
            name="EmbeddingsDates",
            fields=[
                ("id", models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name="ID")),
                ("created_at", models.DateTimeField(auto_now_add=True)),
                ("updated_at", models.DateTimeField(auto_now=True)),
                ("date", models.DateField()),
                (
                    "embeddings",
                    models.ForeignKey(
                        on_delete=django.db.models.deletion.CASCADE,
                        related_name="embeddings_dates",
                        to="database.embeddings",
                    ),
                ),
            ],
            options={
                "indexes": [models.Index(fields=["date"], name="database_em_date_a1ba47_idx")],
            },
        ),
    ]
