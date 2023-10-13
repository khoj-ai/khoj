import uuid

from django.db import models
from django.contrib.auth.models import AbstractUser
from pgvector.django import VectorField, IvfflatIndex


class BaseModel(models.Model):
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        abstract = True


class KhojUser(AbstractUser):
    uuid = models.UUIDField(models.UUIDField(default=uuid.uuid4, editable=False))

    def save(self, *args, **kwargs):
        if not self.uuid:
            self.uuid = uuid.uuid4()
        super().save(*args, **kwargs)


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


class NotionConfig(BaseModel):
    token = models.CharField(max_length=200)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class GithubConfig(BaseModel):
    pat_token = models.CharField(max_length=200)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class GithubRepoConfig(BaseModel):
    name = models.CharField(max_length=200)
    owner = models.CharField(max_length=200)
    branch = models.CharField(max_length=200)
    github_config = models.ForeignKey(GithubConfig, on_delete=models.CASCADE, related_name="githubrepoconfig")


class LocalOrgConfig(BaseModel):
    input_files = models.JSONField(default=list, null=True)
    input_filter = models.JSONField(default=list, null=True)
    index_heading_entries = models.BooleanField(default=False)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class LocalMarkdownConfig(BaseModel):
    input_files = models.JSONField(default=list, null=True)
    input_filter = models.JSONField(default=list, null=True)
    index_heading_entries = models.BooleanField(default=False)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class LocalPdfConfig(BaseModel):
    input_files = models.JSONField(default=list, null=True)
    input_filter = models.JSONField(default=list, null=True)
    index_heading_entries = models.BooleanField(default=False)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class LocalPlaintextConfig(BaseModel):
    input_files = models.JSONField(default=list, null=True)
    input_filter = models.JSONField(default=list, null=True)
    index_heading_entries = models.BooleanField(default=False)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class ConversationProcessorConfig(BaseModel):
    conversation = models.JSONField()
    enable_offline_chat = models.BooleanField(default=False)


class Embeddings(BaseModel):
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
    heading = models.CharField(max_length=1000, default=None, null=True, blank=True)
    file_type = models.CharField(max_length=30, choices=EmbeddingsType.choices, default=EmbeddingsType.PLAINTEXT)
    file_path = models.CharField(max_length=400, default=None, null=True, blank=True)
    file_name = models.CharField(max_length=400, default=None, null=True, blank=True)
    url = models.URLField(max_length=400, default=None, null=True, blank=True)
    hashed_value = models.CharField(max_length=100)
    corpus_id = models.UUIDField(default=uuid.uuid4, editable=False)


class EmbeddingsDates(BaseModel):
    date = models.DateField()
    embeddings = models.ForeignKey(Embeddings, on_delete=models.CASCADE, related_name="embeddings_dates")

    class Meta:
        indexes = [
            models.Index(fields=["date"]),
        ]
