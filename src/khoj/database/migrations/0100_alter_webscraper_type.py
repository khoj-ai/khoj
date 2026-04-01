from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ("database", "0099_usermemory"),
    ]

    operations = [
        migrations.AlterField(
            model_name="webscraper",
            name="type",
            field=models.CharField(
                choices=[
                    ("Firecrawl", "Firecrawl"),
                    ("Olostep", "Olostep"),
                    ("Exa", "Exa"),
                    ("Tavily", "Tavily"),
                    ("Direct", "Direct"),
                ],
                default="Direct",
                max_length=20,
            ),
        ),
    ]
