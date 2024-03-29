# Generated by Django 4.2.5 on 2023-11-09 01:27

import django.db.models.deletion
from django.conf import settings
from django.db import migrations, models


class Migration(migrations.Migration):
    dependencies = [
        ("database", "0012_entry_file_source"),
    ]

    operations = [
        migrations.CreateModel(
            name="Subscription",
            fields=[
                ("id", models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name="ID")),
                ("created_at", models.DateTimeField(auto_now_add=True)),
                ("updated_at", models.DateTimeField(auto_now=True)),
                (
                    "type",
                    models.CharField(
                        choices=[("trial", "Trial"), ("standard", "Standard")], default="trial", max_length=20
                    ),
                ),
                ("is_recurring", models.BooleanField(default=False)),
                ("renewal_date", models.DateTimeField(default=None, null=True)),
                (
                    "user",
                    models.OneToOneField(on_delete=django.db.models.deletion.CASCADE, to=settings.AUTH_USER_MODEL),
                ),
            ],
            options={
                "abstract": False,
            },
        ),
    ]
