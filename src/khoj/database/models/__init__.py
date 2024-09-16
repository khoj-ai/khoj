import re
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
    verified_email = models.BooleanField(default=False)
    email_verification_code = models.CharField(max_length=200, null=True, default=None, blank=True)

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


class OpenAIProcessorConversationConfig(BaseModel):
    name = models.CharField(max_length=200)
    api_key = models.CharField(max_length=200)
    api_base_url = models.URLField(max_length=200, default=None, blank=True, null=True)


class ChatModelOptions(BaseModel):
    class ModelType(models.TextChoices):
        OPENAI = "openai"
        OFFLINE = "offline"
        ANTHROPIC = "anthropic"
        GOOGLE = "google"

    max_prompt_size = models.IntegerField(default=None, null=True, blank=True)
    subscribed_max_prompt_size = models.IntegerField(default=None, null=True, blank=True)
    tokenizer = models.CharField(max_length=200, default=None, null=True, blank=True)
    chat_model = models.CharField(max_length=200, default="bartowski/Meta-Llama-3.1-8B-Instruct-GGUF")
    model_type = models.CharField(max_length=200, choices=ModelType.choices, default=ModelType.OFFLINE)
    vision_enabled = models.BooleanField(default=False)
    openai_config = models.ForeignKey(
        OpenAIProcessorConversationConfig, on_delete=models.CASCADE, default=None, null=True, blank=True
    )


class VoiceModelOption(BaseModel):
    model_id = models.CharField(max_length=200)
    name = models.CharField(max_length=200)


class Agent(BaseModel):
    class StyleColorTypes(models.TextChoices):
        BLUE = "blue"
        GREEN = "green"
        RED = "red"
        YELLOW = "yellow"
        ORANGE = "orange"
        PURPLE = "purple"
        PINK = "pink"
        TEAL = "teal"
        CYAN = "cyan"
        LIME = "lime"
        INDIGO = "indigo"
        FUCHSIA = "fuchsia"
        ROSE = "rose"
        SKY = "sky"
        AMBER = "amber"
        EMERALD = "emerald"

    class StyleIconTypes(models.TextChoices):
        LIGHBULB = "Lightbulb"
        HEALTH = "Health"
        ROBOT = "Robot"
        APERTURE = "Aperture"
        GRADUATION_CAP = "GraduationCap"
        JEEP = "Jeep"
        ISLAND = "Island"
        MATH_OPERATIONS = "MathOperations"
        ASCLEPIUS = "Asclepius"
        COUCH = "Couch"
        CODE = "Code"
        ATOM = "Atom"
        CLOCK_COUNTER_CLOCKWISE = "ClockCounterClockwise"
        PENCIL_LINE = "PencilLine"
        CHALKBOARD = "Chalkboard"

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
    style_color = models.CharField(max_length=200, choices=StyleColorTypes.choices, default=StyleColorTypes.BLUE)
    style_icon = models.CharField(max_length=200, choices=StyleIconTypes.choices, default=StyleIconTypes.LIGHBULB)


class ProcessLock(BaseModel):
    class Operation(models.TextChoices):
        INDEX_CONTENT = "index_content"
        SCHEDULED_JOB = "scheduled_job"
        SCHEDULE_LEADER = "schedule_leader"

    # We need to make sure that some operations are thread-safe. To do so, add locks for potentially shared operations.
    # For example, we need to make sure that only one process is updating the embeddings at a time.
    name = models.CharField(max_length=200, choices=Operation.choices, unique=True)
    started_at = models.DateTimeField(auto_now_add=True)
    max_duration_in_seconds = models.IntegerField(default=60 * 60 * 12)  # 12 hours


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


class ServerChatSettings(BaseModel):
    chat_default = models.ForeignKey(
        ChatModelOptions, on_delete=models.CASCADE, default=None, null=True, blank=True, related_name="chat_default"
    )
    chat_advanced = models.ForeignKey(
        ChatModelOptions, on_delete=models.CASCADE, default=None, null=True, blank=True, related_name="chat_advanced"
    )


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

    # This is the model name exposed to users on their settings page
    name = models.CharField(max_length=200, default="default")
    # Type of content the model can generate embeddings for
    model_type = models.CharField(max_length=200, choices=ModelType.choices, default=ModelType.TEXT)
    # Bi-encoder model of sentence-transformer type to load from HuggingFace
    bi_encoder = models.CharField(max_length=200, default="thenlper/gte-small")
    # Config passed to the sentence-transformer model constructor. E.g. device="cuda:0", trust_remote_server=True etc.
    bi_encoder_model_config = models.JSONField(default=dict, blank=True)
    # Query encode configs like prompt, precision, normalize_embeddings, etc. for sentence-transformer models
    bi_encoder_query_encode_config = models.JSONField(default=dict, blank=True)
    # Docs encode configs like prompt, precision, normalize_embeddings, etc. for sentence-transformer models
    bi_encoder_docs_encode_config = models.JSONField(default=dict, blank=True)
    # Cross-encoder model of sentence-transformer type to load from HuggingFace
    cross_encoder = models.CharField(max_length=200, default="mixedbread-ai/mxbai-rerank-xsmall-v1")
    # Config passed to the cross-encoder model constructor. E.g. device="cuda:0", trust_remote_server=True etc.
    cross_encoder_model_config = models.JSONField(default=dict, blank=True)
    # Inference server API endpoint to use for embeddings inference. Bi-encoder model should be hosted on this server
    embeddings_inference_endpoint = models.CharField(max_length=200, default=None, null=True, blank=True)
    # Inference server API Key to use for embeddings inference. Bi-encoder model should be hosted on this server
    embeddings_inference_endpoint_api_key = models.CharField(max_length=200, default=None, null=True, blank=True)
    # Inference server API endpoint to use for embeddings inference. Cross-encoder model should be hosted on this server
    cross_encoder_inference_endpoint = models.CharField(max_length=200, default=None, null=True, blank=True)
    # Inference server API Key to use for embeddings inference. Cross-encoder model should be hosted on this server
    cross_encoder_inference_endpoint_api_key = models.CharField(max_length=200, default=None, null=True, blank=True)
    # The confidence threshold of the bi_encoder model to consider the embeddings as relevant
    bi_encoder_confidence_threshold = models.FloatField(default=0.18)


class TextToImageModelConfig(BaseModel):
    class ModelType(models.TextChoices):
        OPENAI = "openai"
        STABILITYAI = "stability-ai"
        REPLICATE = "replicate"

    model_name = models.CharField(max_length=200, default="dall-e-3")
    model_type = models.CharField(max_length=200, choices=ModelType.choices, default=ModelType.OPENAI)
    api_key = models.CharField(max_length=200, default=None, null=True, blank=True)
    openai_config = models.ForeignKey(
        OpenAIProcessorConversationConfig, on_delete=models.CASCADE, default=None, null=True, blank=True
    )

    def clean(self):
        # Custom validation logic
        error = {}
        if self.model_type == self.ModelType.OPENAI:
            if self.api_key and self.openai_config:
                error[
                    "api_key"
                ] = "Both API key and OpenAI config cannot be set for OpenAI models. Please set only one of them."
                error[
                    "openai_config"
                ] = "Both API key and OpenAI config cannot be set for OpenAI models. Please set only one of them."
        if self.model_type != self.ModelType.OPENAI:
            if not self.api_key:
                error["api_key"] = "The API key field must be set for non OpenAI models."
            if self.openai_config:
                error["openai_config"] = "OpenAI config cannot be set for non OpenAI models."
        if error:
            raise ValidationError(error)

    def save(self, *args, **kwargs):
        self.clean()
        super().save(*args, **kwargs)


class SpeechToTextModelOptions(BaseModel):
    class ModelType(models.TextChoices):
        OPENAI = "openai"
        OFFLINE = "offline"

    model_name = models.CharField(max_length=200, default="base")
    model_type = models.CharField(max_length=200, choices=ModelType.choices, default=ModelType.OFFLINE)


class UserConversationConfig(BaseModel):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)
    setting = models.ForeignKey(ChatModelOptions, on_delete=models.CASCADE, default=None, null=True, blank=True)


class UserVoiceModelConfig(BaseModel):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)
    setting = models.ForeignKey(VoiceModelOption, on_delete=models.CASCADE, default=None, null=True, blank=True)


class UserSearchModelConfig(BaseModel):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)
    setting = models.ForeignKey(SearchModelConfig, on_delete=models.CASCADE)


class UserTextToImageModelConfig(BaseModel):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)
    setting = models.ForeignKey(TextToImageModelConfig, on_delete=models.CASCADE)


class Conversation(BaseModel):
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)
    conversation_log = models.JSONField(default=dict)
    client = models.ForeignKey(ClientApplication, on_delete=models.CASCADE, default=None, null=True, blank=True)
    slug = models.CharField(max_length=200, default=None, null=True, blank=True)
    title = models.CharField(max_length=200, default=None, null=True, blank=True)
    agent = models.ForeignKey(Agent, on_delete=models.SET_NULL, default=None, null=True, blank=True)
    file_filters = models.JSONField(default=list)
    unique_id = models.UUIDField(default=uuid.uuid4, editable=False, unique=True)


class PublicConversation(BaseModel):
    source_owner = models.ForeignKey(KhojUser, on_delete=models.CASCADE)
    conversation_log = models.JSONField(default=dict)
    slug = models.CharField(max_length=200, default=None, null=True, blank=True)
    title = models.CharField(max_length=200, default=None, null=True, blank=True)
    agent = models.ForeignKey(Agent, on_delete=models.SET_NULL, default=None, null=True, blank=True)


@receiver(pre_save, sender=PublicConversation)
def verify_public_conversation(sender, instance, **kwargs):
    def generate_random_alphanumeric(length):
        characters = "0123456789abcdefghijklmnopqrstuvwxyz"
        return "".join(choice(characters) for _ in range(length))

    # check if this is a new instance
    if instance._state.adding:
        slug = re.sub(r"\W+", "-", instance.slug.lower())[:50]
        observed_random_id = set()
        while PublicConversation.objects.filter(slug=slug).exists():
            try:
                random_id = generate_random_alphanumeric(7)
            except IndexError:
                raise ValidationError(
                    "Unable to generate a unique slug for the Public Conversation. Please try again later."
                )
            observed_random_id.add(random_id)
            slug = f"{slug}-{random_id}"
        instance.slug = slug


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
        DOCX = "docx"

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


class FileObject(BaseModel):
    # Same as Entry but raw will be a much larger string
    file_name = models.CharField(max_length=400, default=None, null=True, blank=True)
    raw_text = models.TextField()
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE, default=None, null=True, blank=True)


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


class DataStore(BaseModel):
    key = models.CharField(max_length=200, unique=True)
    value = models.JSONField(default=dict)
    private = models.BooleanField(default=False)
    owner = models.ForeignKey(KhojUser, on_delete=models.CASCADE, default=None, null=True, blank=True)
