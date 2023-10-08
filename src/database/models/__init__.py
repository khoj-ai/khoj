import uuid

from django.db import models
from django.contrib.auth.models import AbstractUser
from pgvector.django import VectorField, IvfflatIndex


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


class NotionConfig(models.Model):
    token = models.CharField(max_length=200)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class GithubConfig(models.Model):
    pat_token = models.CharField(max_length=200)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class GithubRepoConfig(models.Model):
    name = models.CharField(max_length=200)
    owner = models.CharField(max_length=200)
    branch = models.CharField(max_length=200)
    github_config = models.ForeignKey(GithubConfig, on_delete=models.CASCADE, related_name="githubrepoconfig")


class LocalOrgConfig(models.Model):
    input_files = models.JSONField(default=list, null=True)
    input_filter = models.JSONField(default=list, null=True)
    index_heading_entries = models.BooleanField(default=False)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class LocalMarkdownConfig(models.Model):
    input_files = models.JSONField(default=list, null=True)
    input_filter = models.JSONField(default=list, null=True)
    index_heading_entries = models.BooleanField(default=False)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class LocalPdfConfig(models.Model):
    input_files = models.JSONField(default=list)
    input_filter = models.JSONField(default=list)
    index_heading_entries = models.BooleanField(default=False)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class LocalPlaintextConfig(models.Model):
    input_files = models.JSONField(default=list)
    input_filter = models.JSONField(default=list)
    index_heading_entries = models.BooleanField(default=False)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class ConversationProcessorConfig(models.Model):
    conversation = models.JSONField()
    enable_offline_chat = models.BooleanField(default=False)


class Embeddings(models.Model):
    class EmbeddingsType(models.TextChoices):
        IMAGE = "image"
        PDF = "pdf"
        PLAINTEXT = "plaintext"
        MARKDOWN = "markdown"
        ORG = "org"
        NOTION = "notion"
        GITHUB = "github"
        CONVERSATION = "conversation"

    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE, default=None, null=True, blank=True)
    embeddings = VectorField(dimensions=384)
    raw = models.TextField()
    compiled = models.TextField()
    heading = models.CharField(max_length=400, default=None, null=True, blank=True)
    file_type = models.CharField(max_length=30, choices=EmbeddingsType.choices, default=EmbeddingsType.PLAINTEXT)
    file_path = models.CharField(max_length=400, default=None, null=True, blank=True)
    file_name = models.CharField(max_length=400, default=None, null=True, blank=True)
    url = models.URLField(max_length=400, default=None, null=True, blank=True)
    hashed_value = models.CharField(max_length=100)

    # class Meta:
    #     indexes = [
    #         IvfflatIndex(fields=["embeddings"], name="embeddings_idx", lists=512, opclasses=["vector_cosine_ops"])
    #     ]
