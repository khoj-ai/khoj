# Generated migration for OAuthAccount model

from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    dependencies = [
        ('database', '0099_usermemory'),
    ]

    operations = [
        migrations.CreateModel(
            name='OAuthAccount',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
                ('provider', models.CharField(max_length=50)),
                ('provider_user_id', models.CharField(max_length=255)),
                ('email', models.EmailField(max_length=254)),
                ('name', models.CharField(blank=True, max_length=255)),
                ('given_name', models.CharField(blank=True, max_length=255)),
                ('family_name', models.CharField(blank=True, max_length=255)),
                ('picture', models.CharField(blank=True, max_length=500)),
                ('raw_info', models.JSONField(blank=True, null=True)),
                ('user', models.OneToOneField(on_delete=django.db.models.deletion.CASCADE, related_name='oauth_account', to='database.khojuser')),
            ],
            options={
                'verbose_name': 'OAuth Account',
                'verbose_name_plural': 'OAuth Accounts',
                'unique_together': {('provider', 'provider_user_id')},
            },
        ),
    ]
