import uuid
from random import choice

from django.contrib.auth.models import AbstractUser
from django.core.exceptions import ValidationError
from django.db import models
from django.db.models.signals import pre_save
from django.dispatch import receiver
from pgvector.django import VectorField
from phonenumber_field.modelfields import PhoneNumberField


class BaseModel(models.Model):
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        abstract = True


class ClientApplication(BaseModel):
    name = models.CharField(max_length=200)
    client_id = models.CharField(max_length=200)
    client_secret = models.CharField(max_length=200)

    def __str__(self):
        return self.name


class KhojUser(AbstractUser):
    uuid = models.UUIDField(models.UUIDField(default=uuid.uuid4, editable=False))
    phone_number = PhoneNumberField(null=True, default=None, blank=True)
    verified_phone_number = models.BooleanField(default=False)

    def save(self, *args, **kwargs):
        if not self.uuid:
            self.uuid = uuid.uuid4()
        super().save(*args, **kwargs)


class GoogleUser(models.Model):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)
    sub = models.CharField(max_length=200)
    azp = models.CharField(max_length=200)
    email = models.CharField(max_length=200)
    name = models.CharField(max_length=200, null=True, default=None, blank=True)
    given_name = models.CharField(max_length=200, null=True, default=None, blank=True)
    family_name = models.CharField(max_length=200, null=True, default=None, blank=True)
    picture = models.CharField(max_length=200, null=True, default=None)
    locale = models.CharField(max_length=200, null=True, default=None, blank=True)

    def __str__(self):
        return self.name


class KhojApiUser(models.Model):
    """User issued API tokens to authenticate Khoj clients"""

    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)
    token = models.CharField(max_length=50, unique=True)
    name = models.CharField(max_length=50)
    accessed_at = models.DateTimeField(null=True, default=None)


class Subscription(BaseModel):
    class Type(models.TextChoices):
        TRIAL = "trial"
        STANDARD = "standard"

    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE, related_name="subscription")
    type = models.CharField(max_length=20, choices=Type.choices, default=Type.TRIAL)
    is_recurring = models.BooleanField(default=False)
    renewal_date = models.DateTimeField(null=True, default=None, blank=True)


class ChatModelOptions(BaseModel):
    class ModelType(models.TextChoices):
        OPENAI = "openai"
        OFFLINE = "offline"

    max_prompt_size = models.IntegerField(default=None, null=True, blank=True)
    tokenizer = models.CharField(max_length=200, default=None, null=True, blank=True)
    chat_model = models.CharField(max_length=200, default="NousResearch/Hermes-2-Pro-Mistral-7B-GGUF")
    model_type = models.CharField(max_length=200, choices=ModelType.choices, default=ModelType.OFFLINE)


class Agent(BaseModel):
    creator = models.ForeignKey(
        KhojUser, on_delete=models.CASCADE, default=None, null=True, blank=True
    )  # Creator will only be null when the agents are managed by admin
    name = models.CharField(max_length=200)
    personality = models.TextField()
    avatar = models.URLField(max_length=400, default=None, null=True, blank=True)
    tools = models.JSONField(default=list)  # List of tools the agent has access to, like online search or notes search
    public = models.BooleanField(default=False)
    managed_by_admin = models.BooleanField(default=False)
    chat_model = models.ForeignKey(ChatModelOptions, on_delete=models.CASCADE)
    slug = models.CharField(max_length=200)


@receiver(pre_save, sender=Agent)
def verify_agent(sender, instance, **kwargs):
    # check if this is a new instance
    if instance._state.adding:
        if Agent.objects.filter(name=instance.name, public=True).exists():
            raise ValidationError(f"A public Agent with the name {instance.name} already exists.")
        if Agent.objects.filter(name=instance.name, creator=instance.creator).exists():
            raise ValidationError(f"A private Agent with the name {instance.name} already exists.")

        slug = instance.name.lower().replace(" ", "-")
        observed_random_numbers = set()
        while Agent.objects.filter(slug=slug).exists():
            try:
                random_number = choice([i for i in range(0, 1000) if i not in observed_random_numbers])
            except IndexError:
                raise ValidationError("Unable to generate a unique slug for the Agent. Please try again later.")
            observed_random_numbers.add(random_number)
            slug = f"{slug}-{random_number}"
        instance.slug = slug


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


class SearchModelConfig(BaseModel):
    class ModelType(models.TextChoices):
        TEXT = "text"

    name = models.CharField(max_length=200, default="default")
    model_type = models.CharField(max_length=200, choices=ModelType.choices, default=ModelType.TEXT)
    bi_encoder = models.CharField(max_length=200, default="thenlper/gte-small")
    cross_encoder = models.CharField(max_length=200, default="cross-encoder/ms-marco-MiniLM-L-6-v2")
    embeddings_inference_endpoint = models.CharField(max_length=200, default=None, null=True, blank=True)
    embeddings_inference_endpoint_api_key = models.CharField(max_length=200, default=None, null=True, blank=True)
    cross_encoder_inference_endpoint = models.CharField(max_length=200, default=None, null=True, blank=True)
    cross_encoder_inference_endpoint_api_key = models.CharField(max_length=200, default=None, null=True, blank=True)


class TextToImageModelConfig(BaseModel):
    class ModelType(models.TextChoices):
        OPENAI = "openai"

    model_name = models.CharField(max_length=200, default="dall-e-3")
    model_type = models.CharField(max_length=200, choices=ModelType.choices, default=ModelType.OPENAI)


class OpenAIProcessorConversationConfig(BaseModel):
    api_key = models.CharField(max_length=200)


class OfflineChatProcessorConversationConfig(BaseModel):
    enabled = models.BooleanField(default=False)


class SpeechToTextModelOptions(BaseModel):
    class ModelType(models.TextChoices):
        OPENAI = "openai"
        OFFLINE = "offline"

    model_name = models.CharField(max_length=200, default="base")
    model_type = models.CharField(max_length=200, choices=ModelType.choices, default=ModelType.OFFLINE)


class UserConversationConfig(BaseModel):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)
    setting = models.ForeignKey(ChatModelOptions, on_delete=models.CASCADE, default=None, null=True, blank=True)


class UserSearchModelConfig(BaseModel):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)
    setting = models.ForeignKey(SearchModelConfig, on_delete=models.CASCADE)


class Conversation(BaseModel):
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)
    conversation_log = models.JSONField(default=dict)
    client = models.ForeignKey(ClientApplication, on_delete=models.CASCADE, default=None, null=True, blank=True)
    slug = models.CharField(max_length=200, default=None, null=True, blank=True)
    title = models.CharField(max_length=200, default=None, null=True, blank=True)
    agent = models.ForeignKey(Agent, on_delete=models.SET_NULL, default=None, null=True, blank=True)


class ReflectiveQuestion(BaseModel):
    question = models.CharField(max_length=500)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE, default=None, null=True, blank=True)


class Entry(BaseModel):
    class EntryType(models.TextChoices):
        IMAGE = "image"
        PDF = "pdf"
        PLAINTEXT = "plaintext"
        MARKDOWN = "markdown"
        ORG = "org"
        NOTION = "notion"
        GITHUB = "github"
        CONVERSATION = "conversation"

    class EntrySource(models.TextChoices):
        COMPUTER = "computer"
        NOTION = "notion"
        GITHUB = "github"

    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE, default=None, null=True, blank=True)
    embeddings = VectorField(dimensions=None)
    raw = models.TextField()
    compiled = models.TextField()
    heading = models.CharField(max_length=1000, default=None, null=True, blank=True)
    file_source = models.CharField(max_length=30, choices=EntrySource.choices, default=EntrySource.COMPUTER)
    file_type = models.CharField(max_length=30, choices=EntryType.choices, default=EntryType.PLAINTEXT)
    file_path = models.CharField(max_length=400, default=None, null=True, blank=True)
    file_name = models.CharField(max_length=400, default=None, null=True, blank=True)
    url = models.URLField(max_length=400, default=None, null=True, blank=True)
    hashed_value = models.CharField(max_length=100)
    corpus_id = models.UUIDField(default=uuid.uuid4, editable=False)


class EntryDates(BaseModel):
    date = models.DateField()
    entry = models.ForeignKey(Entry, on_delete=models.CASCADE, related_name="embeddings_dates")

    class Meta:
        indexes = [
            models.Index(fields=["date"]),
        ]


class UserRequests(BaseModel):
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)
    slug = models.CharField(max_length=200)
