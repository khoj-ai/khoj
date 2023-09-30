import uuid

from django.db import models
from django.contrib.auth.models import AbstractUser


class KhojUser(AbstractUser):
    uuid = models.UUIDField(models.UUIDField(default=uuid.uuid4, editable=False))


class GoogleUser(models.Model):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)
    sub = models.CharField(max_length=200)
    azp = models.CharField(max_length=200)
    email = models.CharField(max_length=200)
    name = models.CharField(max_length=200)
    given_name = models.CharField(max_length=200)
    family_name = models.CharField(max_length=200)
    picture = models.CharField(max_length=200)
    locale = models.CharField(max_length=200)

    def __str__(self):
        return self.name


class Configuration(models.Model):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)


class NotionConfig(models.Model):
    token = models.CharField(max_length=200)
    compressed_jsonl = models.CharField(max_length=300)
    embeddings_file = models.CharField(max_length=300)
    config = models.OneToOneField(Configuration, on_delete=models.CASCADE)


class GithubConfig(models.Model):
    pat_token = models.CharField(max_length=200)
    compressed_jsonl = models.CharField(max_length=300)
    embeddings_file = models.CharField(max_length=300)
    config = models.OneToOneField(Configuration, on_delete=models.CASCADE)


class GithubRepoConfig(models.Model):
    name = models.CharField(max_length=200)
    owner = models.CharField(max_length=200)
    branch = models.CharField(max_length=200)
    github_config = models.ForeignKey(GithubConfig, on_delete=models.CASCADE)


class ConversationProcessorConfig(models.Model):
    conversation = models.JSONField()
    enable_offline_chat = models.BooleanField(default=False)
