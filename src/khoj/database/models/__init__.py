import logging
import os
import re
import uuid
from random import choice
from typing import Dict, List, Optional, Union

from django.contrib.auth.models import AbstractUser
from django.contrib.postgres.fields import ArrayField
from django.core.exceptions import ValidationError
from django.db import models
from django.db.models.signals import pre_save
from django.dispatch import receiver
from pgvector.django import VectorField
from phonenumber_field.modelfields import PhoneNumberField
from pydantic import BaseModel as PydanticBaseModel
from pydantic import Field, model_validator

logger = logging.getLogger(__name__)


# Pydantic models for type Chat Message validation
class Context(PydanticBaseModel):
    compiled: str
    file: str
    uri: Optional[str] = None
    query: Optional[str] = None

    @model_validator(mode="after")
    def set_uri_fallback(self):
        """Set the URI to existing deeplink URI. Fallback to file based URI if unset."""
        if self.uri and self.uri.strip():
            self.uri = self.uri
        elif self.file and (self.file.startswith("http") or self.file.startswith("file://")):
            self.uri = self.file
        elif self.file:
            self.uri = f"file://{self.file}"
        else:
            self.uri = None
        return self


class CodeContextFile(PydanticBaseModel):
    filename: str
    b64_data: str


class CodeContextResult(PydanticBaseModel):
    success: bool
    output_files: List[CodeContextFile]
    std_out: Optional[str] = None
    std_err: str
    code_runtime: Optional[int] = None


class CodeContextData(PydanticBaseModel):
    code: str
    results: Optional[CodeContextResult] = None


class WebPage(PydanticBaseModel):
    link: str
    query: Optional[str] = None
    snippet: str


class AnswerBox(PydanticBaseModel):
    link: Optional[str] = None
    snippet: Optional[str] = None
    title: str
    snippetHighlighted: Optional[List[str]] = None


class PeopleAlsoAsk(PydanticBaseModel):
    link: Optional[str] = None
    question: Optional[str] = None
    snippet: Optional[str] = None
    title: Optional[str] = None


class KnowledgeGraph(PydanticBaseModel):
    attributes: Optional[Dict[str, str]] = None
    description: Optional[str] = None
    descriptionLink: Optional[str] = None
    descriptionSource: Optional[str] = None
    imageUrl: Optional[str] = None
    title: str
    type: Optional[str] = None


class OrganicContext(PydanticBaseModel):
    snippet: Optional[str] = None
    title: str
    link: str


class OnlineContext(PydanticBaseModel):
    webpages: Optional[Union[WebPage, List[WebPage]]] = None
    answerBox: Optional[AnswerBox] = None
    peopleAlsoAsk: Optional[List[PeopleAlsoAsk]] = None
    knowledgeGraph: Optional[KnowledgeGraph] = None
    organic: Optional[List[OrganicContext]] = None


class Intent(PydanticBaseModel):
    type: str
    query: Optional[str] = None
    memory_type: Optional[str] = Field(alias="memory-type", default=None)
    inferred_queries: Optional[List[str]] = Field(default=None, alias="inferred-queries")


class TrainOfThought(PydanticBaseModel):
    type: str
    data: str


class ChatMessageModel(PydanticBaseModel):
    by: str
    message: str | list[dict]
    trainOfThought: List[TrainOfThought] = []
    context: List[Context] = []
    onlineContext: Dict[str, OnlineContext] = {}
    codeContext: Dict[str, CodeContextData] = {}
    researchContext: Optional[List] = None
    operatorContext: Optional[List] = None
    created: Optional[str] = None
    images: Optional[List[str]] = None
    queryFiles: Optional[List[Dict]] = None
    excalidrawDiagram: Optional[List[Dict]] = None
    mermaidjsDiagram: Optional[str] = None
    turnId: Optional[str] = None
    intent: Optional[Intent] = None
    automationId: Optional[str] = None


class DbBaseModel(models.Model):
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        abstract = True


class ClientApplication(DbBaseModel):
    name = models.CharField(max_length=200)
    client_id = models.CharField(max_length=200)
    client_secret = models.CharField(max_length=200)

    def __str__(self):
        return self.name


class KhojUser(AbstractUser):
    uuid = models.UUIDField(default=uuid.uuid4, editable=False, unique=True)
    phone_number = PhoneNumberField(null=True, default=None, blank=True)
    verified_phone_number = models.BooleanField(default=False)
    verified_email = models.BooleanField(default=False)
    email_verification_code = models.CharField(max_length=200, null=True, default=None, blank=True)
    email_verification_code_expiry = models.DateTimeField(null=True, default=None, blank=True)

    def save(self, *args, **kwargs):
        if not self.uuid:
            self.uuid = uuid.uuid4()
        super().save(*args, **kwargs)

    def __str__(self):
        return f"{self.username} ({self.uuid})"


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


class Subscription(DbBaseModel):
    class Type(models.TextChoices):
        TRIAL = "trial"
        STANDARD = "standard"

    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE, related_name="subscription")
    type = models.CharField(max_length=20, choices=Type.choices, default=Type.STANDARD)
    is_recurring = models.BooleanField(default=False)
    renewal_date = models.DateTimeField(null=True, default=None, blank=True)
    enabled_trial_at = models.DateTimeField(null=True, default=None, blank=True)


class AiModelApi(DbBaseModel):
    name = models.CharField(max_length=200)
    api_key = models.CharField(max_length=4000)
    api_base_url = models.URLField(max_length=200, default=None, blank=True, null=True)

    def __str__(self):
        return self.name


class PriceTier(models.TextChoices):
    FREE = "free"
    STANDARD = "standard"


class ChatModel(DbBaseModel):
    class ModelType(models.TextChoices):
        OPENAI = "openai"
        ANTHROPIC = "anthropic"
        GOOGLE = "google"

    max_prompt_size = models.IntegerField(default=None, null=True, blank=True)
    subscribed_max_prompt_size = models.IntegerField(default=None, null=True, blank=True)
    tokenizer = models.CharField(max_length=200, default=None, null=True, blank=True)
    name = models.CharField(max_length=200, default="gemini-2.5-flash")
    friendly_name = models.CharField(max_length=200, default=None, null=True, blank=True)
    model_type = models.CharField(max_length=200, choices=ModelType.choices, default=ModelType.GOOGLE)
    price_tier = models.CharField(max_length=20, choices=PriceTier.choices, default=PriceTier.FREE)
    vision_enabled = models.BooleanField(default=False)
    ai_model_api = models.ForeignKey(AiModelApi, on_delete=models.CASCADE, default=None, null=True, blank=True)
    description = models.TextField(default=None, null=True, blank=True)
    strengths = models.TextField(default=None, null=True, blank=True)

    def __str__(self):
        return self.friendly_name


class VoiceModelOption(DbBaseModel):
    model_id = models.CharField(max_length=200)
    name = models.CharField(max_length=200)
    price_tier = models.CharField(max_length=20, choices=PriceTier.choices, default=PriceTier.STANDARD)


class Agent(DbBaseModel):
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
        LIGHTBULB = "Lightbulb"
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
        CIGARETTE = "Cigarette"
        CRANE_TOWER = "CraneTower"
        HEART = "Heart"
        LEAF = "Leaf"
        NEWSPAPER_CLIPPING = "NewspaperClipping"
        ORANGE_SLICE = "OrangeSlice"
        SMILEY_MELTING = "SmileyMelting"
        YIN_YANG = "YinYang"
        SNEAKER_MOVE = "SneakerMove"
        STUDENT = "Student"
        OVEN = "Oven"
        GAVEL = "Gavel"
        BROADCAST = "Broadcast"

    class PrivacyLevel(models.TextChoices):
        PUBLIC = "public"
        PRIVATE = "private"
        PROTECTED = "protected"

    class InputToolOptions(models.TextChoices):
        # These map to various ConversationCommand types
        GENERAL = "general"
        ONLINE = "online"
        NOTES = "notes"
        WEBPAGE = ("webpage",)
        CODE = "code"

    class OutputModeOptions(models.TextChoices):
        # These map to various ConversationCommand types
        IMAGE = "image"
        DIAGRAM = "diagram"

    creator = models.ForeignKey(
        KhojUser, on_delete=models.CASCADE, default=None, null=True, blank=True
    )  # Creator will only be null when the agents are managed by admin
    name = models.CharField(max_length=200)
    personality = models.TextField(default=None, null=True, blank=True)
    input_tools = ArrayField(
        models.CharField(max_length=200, choices=InputToolOptions.choices), default=list, null=True, blank=True
    )
    output_modes = ArrayField(
        models.CharField(max_length=200, choices=OutputModeOptions.choices), default=list, null=True, blank=True
    )
    managed_by_admin = models.BooleanField(default=False)
    chat_model = models.ForeignKey(ChatModel, on_delete=models.CASCADE)
    slug = models.CharField(max_length=200, unique=True)
    style_color = models.CharField(max_length=200, choices=StyleColorTypes.choices, default=StyleColorTypes.ORANGE)
    style_icon = models.CharField(max_length=200, choices=StyleIconTypes.choices, default=StyleIconTypes.LIGHTBULB)
    privacy_level = models.CharField(max_length=30, choices=PrivacyLevel.choices, default=PrivacyLevel.PRIVATE)
    is_hidden = models.BooleanField(default=False)

    def save(self, *args, **kwargs):
        is_new = self._state.adding

        if self.creator is None:
            self.managed_by_admin = True

        if is_new and not self.slug:
            random_sequence = "".join(choice("0123456789") for i in range(6))
            slug = f"{self.name.lower().replace(' ', '-')}-{random_sequence}"
            self.slug = slug

        super().save(*args, **kwargs)

    def __str__(self):
        return self.name


class ProcessLock(DbBaseModel):
    class Operation(models.TextChoices):
        INDEX_CONTENT = "index_content"
        SCHEDULED_JOB = "scheduled_job"
        SCHEDULE_LEADER = "schedule_leader"
        APPLY_MIGRATIONS = "apply_migrations"

    # We need to make sure that some operations are thread-safe. To do so, add locks for potentially shared operations.
    # For example, we need to make sure that only one process is updating the embeddings at a time.
    name = models.CharField(max_length=200, choices=Operation.choices, unique=True)
    started_at = models.DateTimeField(auto_now_add=True)
    max_duration_in_seconds = models.IntegerField(default=60 * 60 * 12)  # 12 hours


@receiver(pre_save, sender=Agent)
def verify_agent(sender, instance, **kwargs):
    # check if this is a new instance
    if instance._state.adding:
        if Agent.objects.filter(name=instance.name, privacy_level=Agent.PrivacyLevel.PUBLIC).exists():
            raise ValidationError(f"A public Agent with the name {instance.name} already exists.")
        if Agent.objects.filter(name=instance.name, creator=instance.creator).exists():
            raise ValidationError(f"A private Agent with the name {instance.name} already exists.")


class NotionConfig(DbBaseModel):
    token = models.CharField(max_length=200)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class GithubConfig(DbBaseModel):
    pat_token = models.CharField(max_length=200)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)


class GithubRepoConfig(DbBaseModel):
    name = models.CharField(max_length=200)
    owner = models.CharField(max_length=200)
    branch = models.CharField(max_length=200)
    github_config = models.ForeignKey(GithubConfig, on_delete=models.CASCADE, related_name="githubrepoconfig")


class WebScraper(DbBaseModel):
    class WebScraperType(models.TextChoices):
        FIRECRAWL = "Firecrawl"
        OLOSTEP = "Olostep"
        EXA = "Exa"
        DIRECT = "Direct"

    name = models.CharField(
        max_length=200,
        default=None,
        null=True,
        blank=True,
        unique=True,
        help_text="Friendly name. If not set, it will be set to the type of the scraper.",
    )
    type = models.CharField(max_length=20, choices=WebScraperType.choices, default=WebScraperType.DIRECT)
    api_key = models.CharField(
        max_length=200,
        default=None,
        null=True,
        blank=True,
        help_text="API key of the web scraper. Only set if scraper service requires an API key. Default is set from env var.",
    )
    api_url = models.URLField(
        max_length=200,
        default=None,
        null=True,
        blank=True,
        help_text="API URL of the web scraper. Only set if scraper service on non-default URL.",
    )
    priority = models.IntegerField(
        default=None,
        null=True,
        blank=True,
        unique=True,
        help_text="Priority of the web scraper. Lower numbers run first.",
    )

    def clean(self):
        error = {}
        if self.name is None:
            self.name = self.type.capitalize()
        if self.api_url is None:
            if self.type == self.WebScraperType.FIRECRAWL:
                self.api_url = os.getenv("FIRECRAWL_API_URL", "https://api.firecrawl.dev")
            elif self.type == self.WebScraperType.OLOSTEP:
                self.api_url = os.getenv("OLOSTEP_API_URL", "https://agent.olostep.com/olostep-p2p-incomingAPI")
            elif self.type == self.WebScraperType.EXA:
                self.api_url = os.getenv("EXA_API_URL", "https://api.exa.ai")
        if self.api_key is None:
            if self.type == self.WebScraperType.FIRECRAWL:
                self.api_key = os.getenv("FIRECRAWL_API_KEY")
                if not self.api_key and self.api_url == "https://api.firecrawl.dev":
                    error["api_key"] = "Set API key to use default Firecrawl. Get API key from https://firecrawl.dev."
            elif self.type == self.WebScraperType.OLOSTEP:
                self.api_key = os.getenv("OLOSTEP_API_KEY")
                if self.api_key is None:
                    error["api_key"] = "Set API key to use Olostep. Get API key from https://olostep.com/."
            elif self.type == self.WebScraperType.EXA:
                self.api_key = os.getenv("EXA_API_KEY")
                if self.api_key is None:
                    error["api_key"] = "Set API key to use Exa. Get API key from https://exa.ai/."
        if error:
            raise ValidationError(error)

    def save(self, *args, **kwargs):
        self.clean()

        if self.priority is None:
            max_priority = WebScraper.objects.aggregate(models.Max("priority"))["priority__max"]
            self.priority = max_priority + 1 if max_priority else 1

        super().save(*args, **kwargs)

    def __str__(self):
        return self.name


class ServerChatSettings(DbBaseModel):
    chat_default = models.ForeignKey(
        ChatModel, on_delete=models.CASCADE, default=None, null=True, blank=True, related_name="chat_default"
    )
    chat_advanced = models.ForeignKey(
        ChatModel, on_delete=models.CASCADE, default=None, null=True, blank=True, related_name="chat_advanced"
    )
    think_free_fast = models.ForeignKey(
        ChatModel, on_delete=models.CASCADE, default=None, null=True, blank=True, related_name="think_free_fast"
    )
    think_free_deep = models.ForeignKey(
        ChatModel, on_delete=models.CASCADE, default=None, null=True, blank=True, related_name="think_free_deep"
    )
    think_paid_fast = models.ForeignKey(
        ChatModel, on_delete=models.CASCADE, default=None, null=True, blank=True, related_name="think_paid_fast"
    )
    think_paid_deep = models.ForeignKey(
        ChatModel, on_delete=models.CASCADE, default=None, null=True, blank=True, related_name="think_paid_deep"
    )
    web_scraper = models.ForeignKey(
        WebScraper, on_delete=models.CASCADE, default=None, null=True, blank=True, related_name="web_scraper"
    )

    def clean(self):
        error = {}
        if self.chat_default and self.chat_default.price_tier != PriceTier.FREE:
            error["chat_default"] = "Set the price tier of this chat model to free or use a free tier chat model."
        if self.think_free_fast and self.think_free_fast.price_tier != PriceTier.FREE:
            error["think_free_fast"] = "Set the price tier of this chat model to free or use a free tier chat model."
        if self.think_free_deep and self.think_free_deep.price_tier != PriceTier.FREE:
            error["think_free_deep"] = "Set the price tier of this chat model to free or use a free tier chat model."
        if error:
            raise ValidationError(error)

    def save(self, *args, **kwargs):
        self.clean()
        super().save(*args, **kwargs)


class SearchModelConfig(DbBaseModel):
    class ModelType(models.TextChoices):
        TEXT = "text"

    class ApiType(models.TextChoices):
        HUGGINGFACE = "huggingface"
        OPENAI = "openai"
        LOCAL = "local"

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
    # Inference server API type to use for embeddings inference.
    embeddings_inference_endpoint_type = models.CharField(
        max_length=200, choices=ApiType.choices, default=ApiType.LOCAL
    )
    # Inference server API endpoint to use for embeddings inference. Cross-encoder model should be hosted on this server
    cross_encoder_inference_endpoint = models.CharField(max_length=200, default=None, null=True, blank=True)
    # Inference server API Key to use for embeddings inference. Cross-encoder model should be hosted on this server
    cross_encoder_inference_endpoint_api_key = models.CharField(max_length=200, default=None, null=True, blank=True)
    # The confidence threshold of the bi_encoder model to consider the embeddings as relevant
    bi_encoder_confidence_threshold = models.FloatField(default=0.18)

    def __str__(self):
        return self.name


class TextToImageModelConfig(DbBaseModel):
    class ModelType(models.TextChoices):
        OPENAI = "openai"
        STABILITYAI = "stability-ai"
        REPLICATE = "replicate"
        GOOGLE = "google"

    model_name = models.CharField(max_length=200, default="dall-e-3")
    friendly_name = models.CharField(max_length=200, default=None, null=True, blank=True)
    model_type = models.CharField(max_length=200, choices=ModelType.choices, default=ModelType.OPENAI)
    price_tier = models.CharField(max_length=20, choices=PriceTier.choices, default=PriceTier.FREE)
    api_key = models.CharField(max_length=200, default=None, null=True, blank=True)
    ai_model_api = models.ForeignKey(AiModelApi, on_delete=models.CASCADE, default=None, null=True, blank=True)

    def clean(self):
        # Custom validation logic
        error = {}
        if self.model_type == self.ModelType.OPENAI:
            if self.api_key and self.ai_model_api:
                error["api_key"] = (
                    "Both API key and AI Model API cannot be set for OpenAI models. Please set only one of them."
                )
                error["ai_model_api"] = (
                    "Both API key and OpenAI config cannot be set for OpenAI models. Please set only one of them."
                )
        if self.model_type != self.ModelType.OPENAI and self.model_type != self.ModelType.GOOGLE:
            if not self.api_key:
                error["api_key"] = "The API key field must be set for non OpenAI, non Google models."
            if self.ai_model_api:
                error["ai_model_api"] = "AI Model API cannot be set for non OpenAI, non Google models."
        if error:
            raise ValidationError(error)

    def save(self, *args, **kwargs):
        self.clean()
        super().save(*args, **kwargs)

    def __str__(self):
        return f"{self.model_name} - {self.model_type}"


class SpeechToTextModelOptions(DbBaseModel):
    class ModelType(models.TextChoices):
        OPENAI = "openai"

    model_name = models.CharField(max_length=200, default="whisper-1")
    friendly_name = models.CharField(max_length=200, default=None, null=True, blank=True)
    model_type = models.CharField(max_length=200, choices=ModelType.choices, default=ModelType.OPENAI)
    price_tier = models.CharField(max_length=20, choices=PriceTier.choices, default=PriceTier.FREE)
    ai_model_api = models.ForeignKey(AiModelApi, on_delete=models.CASCADE, default=None, null=True, blank=True)

    def __str__(self):
        return f"{self.model_name} - {self.model_type}"


class UserConversationConfig(DbBaseModel):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)
    setting = models.ForeignKey(ChatModel, on_delete=models.CASCADE, default=None, null=True, blank=True)


class UserVoiceModelConfig(DbBaseModel):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)
    setting = models.ForeignKey(VoiceModelOption, on_delete=models.CASCADE, default=None, null=True, blank=True)


class UserTextToImageModelConfig(DbBaseModel):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)
    setting = models.ForeignKey(TextToImageModelConfig, on_delete=models.CASCADE)


class Conversation(DbBaseModel):
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)
    conversation_log = models.JSONField(default=dict)
    client = models.ForeignKey(ClientApplication, on_delete=models.CASCADE, default=None, null=True, blank=True)

    # Slug is an app-generated conversation identifier. Need not be unique. Used as display title essentially.
    slug = models.CharField(max_length=200, default=None, null=True, blank=True)

    # The title field is explicitly set by the user.
    title = models.CharField(max_length=500, default=None, null=True, blank=True)
    agent = models.ForeignKey(Agent, on_delete=models.SET_NULL, default=None, null=True, blank=True)
    file_filters = models.JSONField(default=list)
    id = models.UUIDField(default=uuid.uuid4, editable=False, unique=True, primary_key=True, db_index=True)

    def clean(self):
        # Validate conversation_log structure
        try:
            messages = self.conversation_log.get("chat", [])
            for msg in messages:
                ChatMessageModel.model_validate(msg)
        except Exception as e:
            raise ValidationError(f"Invalid conversation_log format: {str(e)}")

    def save(self, *args, **kwargs):
        self.clean()
        super().save(*args, **kwargs)

    @property
    def messages(self) -> List[ChatMessageModel]:
        """Type-hinted accessor for conversation messages"""
        validated_messages = []
        for msg in self.conversation_log.get("chat", []):
            try:
                # Clean up inferred queries if they contain None
                if msg.get("intent") and msg["intent"].get("inferred_queries"):
                    msg["intent"]["inferred-queries"] = [
                        q for q in msg["intent"]["inferred_queries"] if q is not None and isinstance(q, str)
                    ]
                msg["message"] = str(msg.get("message", ""))
                validated_messages.append(ChatMessageModel.model_validate(msg))
            except ValidationError as e:
                logger.warning(f"Skipping invalid message in conversation: {e}")
                continue
        return validated_messages

    async def pop_message(self, interrupted: bool = False) -> Optional[ChatMessageModel]:
        """
        Remove and return the last message from the conversation log, persisting the change to the database.
        When interrupted is True, we only drop the last message if it was an interrupted message by khoj.
        """
        chat_log = self.conversation_log.get("chat", [])

        if not chat_log:
            return None

        last_message = chat_log[-1]
        is_interrupted_msg = last_message.get("by") == "khoj" and not last_message.get("message")
        # When handling an interruption, only pop if the last message is an empty one by khoj.
        if interrupted and not is_interrupted_msg:
            return None

        # Pop the last message, save the conversation, and then return the message.
        popped_message_dict = chat_log.pop()
        await self.asave()

        # Try to validate and return the popped message as a Pydantic model
        try:
            return ChatMessageModel.model_validate(popped_message_dict)
        except ValidationError as e:
            logger.warning(f"Popped an invalid message from conversation. The removal has been saved. Error: {e}")
            # The invalid message was removed and saved, but we can't return a valid model.
            return None


class PublicConversation(DbBaseModel):
    source_owner = models.ForeignKey(KhojUser, on_delete=models.CASCADE)
    conversation_log = models.JSONField(default=dict)
    slug = models.CharField(max_length=200, default=None, null=True, blank=True)
    title = models.CharField(max_length=200, default=None, null=True, blank=True)
    agent = models.ForeignKey(Agent, on_delete=models.SET_NULL, default=None, null=True, blank=True)


@receiver(pre_save, sender=PublicConversation)
def verify_public_conversation(sender, instance, **kwargs):
    # check if this is a new instance
    if instance._state.adding:
        base_length = 50  # Base slug length before adding random suffix
        base_slug = re.sub(r"\W+", "-", instance.slug.lower())[:base_length] if instance.slug else uuid.uuid4().hex
        suffix_length = 8  # Length of the random suffix to ensure uniqueness
        while True:
            random_id = uuid.uuid4().hex[:suffix_length]
            slug = f"{base_slug}-{random_id}"
            if not PublicConversation.objects.filter(slug=slug).exists():
                break
        instance.slug = slug


class ReflectiveQuestion(DbBaseModel):
    question = models.CharField(max_length=500)
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE, default=None, null=True, blank=True)


class FileObject(DbBaseModel):
    # Contains the full text of a file that has associated Entry objects
    file_name = models.CharField(max_length=400, default=None, null=True, blank=True)
    raw_text = models.TextField()
    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE, default=None, null=True, blank=True)
    agent = models.ForeignKey(Agent, on_delete=models.CASCADE, default=None, null=True, blank=True)


class Entry(DbBaseModel):
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
    agent = models.ForeignKey(Agent, on_delete=models.CASCADE, default=None, null=True, blank=True)
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
    search_model = models.ForeignKey(SearchModelConfig, on_delete=models.SET_NULL, default=None, null=True, blank=True)
    file_object = models.ForeignKey(FileObject, on_delete=models.CASCADE, default=None, null=True, blank=True)

    def save(self, *args, **kwargs):
        if self.user and self.agent:
            raise ValidationError("An Entry cannot be associated with both a user and an agent.")


class EntryDates(DbBaseModel):
    date = models.DateField()
    entry = models.ForeignKey(Entry, on_delete=models.CASCADE, related_name="embeddings_dates")

    class Meta:
        indexes = [
            models.Index(fields=["date"]),
        ]


class UserRequests(DbBaseModel):
    """Stores user requests to the server for rate limiting."""

    user = models.ForeignKey(KhojUser, on_delete=models.CASCADE)
    slug = models.CharField(max_length=200)


class RateLimitRecord(DbBaseModel):
    """Stores individual request timestamps for rate limiting."""

    identifier = models.CharField(max_length=255, db_index=True)  # IP address or email
    slug = models.CharField(max_length=255, db_index=True)  # Differentiates limit types

    class Meta:
        indexes = [
            models.Index(fields=["identifier", "slug", "created_at"]),
        ]
        ordering = ["-created_at"]

    def __str__(self):
        return f"{self.slug} - {self.identifier} at {self.created_at}"


class DataStore(DbBaseModel):
    key = models.CharField(max_length=200, unique=True)
    value = models.JSONField(default=dict)
    private = models.BooleanField(default=False)
    owner = models.ForeignKey(KhojUser, on_delete=models.CASCADE, default=None, null=True, blank=True)


class McpServer(DbBaseModel):
    name = models.CharField(max_length=50, unique=True)
    path = models.CharField(max_length=200, unique=True)
    api_key = models.CharField(max_length=4000, blank=True, null=True)

    def __str__(self):
        return self.name
