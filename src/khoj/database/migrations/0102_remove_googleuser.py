# Remove legacy GoogleUser model after data migration to OAuthAccount

from django.db import migrations


class Migration(migrations.Migration):

    dependencies = [
        ("database", "0101_migrate_googleuser_to_oauthaccount"),
    ]

    operations = [
        migrations.DeleteModel(
            name="GoogleUser",
        ),
    ]
