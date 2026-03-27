# Data migration: copy GoogleUser records into OAuthAccount

from django.db import migrations


def migrate_google_users(apps, schema_editor):
    GoogleUser = apps.get_model("database", "GoogleUser")
    OAuthAccount = apps.get_model("database", "OAuthAccount")

    for gu in GoogleUser.objects.select_related("user").all():
        OAuthAccount.objects.update_or_create(
            provider="google",
            provider_user_id=gu.sub,
            defaults={
                "email": gu.email or "",
                "name": gu.name or "",
                "given_name": gu.given_name or "",
                "family_name": gu.family_name or "",
                "picture": gu.picture or "",
                "user": gu.user,
            },
        )


class Migration(migrations.Migration):

    dependencies = [
        ("database", "0100_oauthaccount"),
    ]

    operations = [
        migrations.RunPython(migrate_google_users, migrations.RunPython.noop),
    ]
