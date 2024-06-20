import csv
import json

from apscheduler.job import Job
from django.contrib import admin, messages
from django.contrib.auth.admin import UserAdmin
from django.http import HttpResponse
from django_apscheduler.admin import DjangoJobAdmin
from django_apscheduler.jobstores import DjangoJobStore
from django_apscheduler.models import DjangoJob

from khoj.database.models import (
    Agent,
    ChatModelOptions,
    ClientApplication,
    Conversation,
    Entry,
    GithubConfig,
    KhojUser,
    NotionConfig,
    OpenAIProcessorConversationConfig,
    ProcessLock,
    ReflectiveQuestion,
    SearchModelConfig,
    ServerChatSettings,
    SpeechToTextModelOptions,
    Subscription,
    TextToImageModelConfig,
    UserSearchModelConfig,
    VoiceModelOption,
)
from khoj.utils.helpers import ImageIntentType

admin.site.unregister(DjangoJob)


class KhojDjangoJobAdmin(DjangoJobAdmin):
    list_display = (
        "id",
        "next_run_time",
        "job_info",
    )
    search_fields = ("id", "next_run_time")
    ordering = ("-next_run_time",)
    job_store = DjangoJobStore()

    def job_info(self, obj):
        job: Job = self.job_store.lookup_job(obj.id)
        return f"{job.func_ref} {job.args} {job.kwargs}" if job else "None"

    job_info.short_description = "Job Info"  # type: ignore

    def get_search_results(self, request, queryset, search_term):
        queryset, use_distinct = super().get_search_results(request, queryset, search_term)
        if search_term:
            jobs = [job.id for job in self.job_store.get_all_jobs() if search_term in str(job)]
            queryset |= self.model.objects.filter(id__in=jobs)
        return queryset, use_distinct


admin.site.register(DjangoJob, KhojDjangoJobAdmin)


class KhojUserAdmin(UserAdmin):
    list_display = (
        "id",
        "email",
        "username",
        "is_active",
        "is_staff",
        "is_superuser",
        "phone_number",
    )
    search_fields = ("email", "username", "phone_number", "uuid")
    filter_horizontal = ("groups", "user_permissions")

    fieldsets = (("Personal info", {"fields": ("phone_number", "email_verification_code")}),) + UserAdmin.fieldsets

    actions = ["get_email_login_url"]

    def get_email_login_url(self, request, queryset):
        for user in queryset:
            if user.email:
                host = request.get_host()
                unique_id = user.email_verification_code
                login_url = f"{host}/auth/magic?code={unique_id}"
                messages.info(request, f"Email login URL for {user.email}: {login_url}")

    get_email_login_url.short_description = "Get email login URL"  # type: ignore


admin.site.register(KhojUser, KhojUserAdmin)

admin.site.register(ProcessLock)
admin.site.register(SpeechToTextModelOptions)
admin.site.register(SearchModelConfig)
admin.site.register(ReflectiveQuestion)
admin.site.register(UserSearchModelConfig)
admin.site.register(ClientApplication)
admin.site.register(GithubConfig)
admin.site.register(NotionConfig)
admin.site.register(VoiceModelOption)


@admin.register(Agent)
class AgentAdmin(admin.ModelAdmin):
    list_display = (
        "id",
        "name",
    )
    search_fields = ("id", "name")
    ordering = ("-created_at",)


@admin.register(Entry)
class EntryAdmin(admin.ModelAdmin):
    list_display = (
        "id",
        "created_at",
        "updated_at",
        "user",
        "file_source",
        "file_type",
        "file_name",
        "file_path",
    )
    search_fields = ("id", "user__email", "user__username", "file_path")
    list_filter = ("file_type",)
    ordering = ("-created_at",)


@admin.register(Subscription)
class KhojUserSubscription(admin.ModelAdmin):
    list_display = (
        "id",
        "user",
        "type",
    )

    search_fields = ("id", "user__email", "user__username", "type")
    list_filter = ("type",)


@admin.register(ChatModelOptions)
class ChatModelOptionsAdmin(admin.ModelAdmin):
    list_display = (
        "id",
        "chat_model",
        "model_type",
        "max_prompt_size",
    )
    search_fields = ("id", "chat_model", "model_type")


@admin.register(TextToImageModelConfig)
class TextToImageModelOptionsAdmin(admin.ModelAdmin):
    list_display = (
        "id",
        "model_name",
        "model_type",
    )
    search_fields = ("id", "model_name", "model_type")


@admin.register(OpenAIProcessorConversationConfig)
class OpenAIProcessorConversationConfigAdmin(admin.ModelAdmin):
    list_display = (
        "id",
        "name",
        "api_key",
        "api_base_url",
    )
    search_fields = ("id", "name", "api_key", "api_base_url")


@admin.register(ServerChatSettings)
class ServerChatSettingsAdmin(admin.ModelAdmin):
    list_display = (
        "default_model",
        "summarizer_model",
    )


@admin.register(Conversation)
class ConversationAdmin(admin.ModelAdmin):
    list_display = (
        "id",
        "user",
        "created_at",
        "updated_at",
        "client",
    )
    search_fields = ("id", "user__email", "user__username", "client__name")
    list_filter = ("agent",)
    ordering = ("-created_at",)

    actions = ["export_selected_objects", "export_selected_minimal_objects"]

    def export_selected_objects(self, request, queryset):
        response = HttpResponse(content_type="text/csv")
        response["Content-Disposition"] = 'attachment; filename="conversations.csv"'

        writer = csv.writer(response)
        writer.writerow(["id", "user", "created_at", "updated_at", "conversation_log"])

        for conversation in queryset:
            modified_log = conversation.conversation_log
            chat_log = modified_log.get("chat", [])
            for idx, log in enumerate(chat_log):
                if (
                    log["by"] == "khoj"
                    and log["intent"]
                    and log["intent"]["type"]
                    and (
                        log["intent"]["type"] == ImageIntentType.TEXT_TO_IMAGE.value
                        or log["intent"]["type"] == ImageIntentType.TEXT_TO_IMAGE_V3.value
                    )
                ):
                    log["message"] = "inline image redacted for space"
                    chat_log[idx] = log
            modified_log["chat"] = chat_log

            writer.writerow(
                [
                    conversation.id,
                    conversation.user,
                    conversation.created_at,
                    conversation.updated_at,
                    json.dumps(modified_log),
                ]
            )

        return response

    export_selected_objects.short_description = "Export selected conversations"  # type: ignore

    def export_selected_minimal_objects(self, request, queryset):
        response = HttpResponse(content_type="text/csv")
        response["Content-Disposition"] = 'attachment; filename="conversations.csv"'

        writer = csv.writer(response)
        writer.writerow(["id", "user", "created_at", "updated_at", "conversation_log"])

        fields_to_keep = set(["message", "by", "created"])

        for conversation in queryset:
            return_log = dict()
            chat_log = conversation.conversation_log.get("chat", [])
            for idx, log in enumerate(chat_log):
                updated_log = {}
                for key in fields_to_keep:
                    updated_log[key] = log[key]
                if (
                    log["by"] == "khoj"
                    and log["intent"]
                    and log["intent"]["type"]
                    and (
                        log["intent"]["type"] == ImageIntentType.TEXT_TO_IMAGE.value
                        or log["intent"]["type"] == ImageIntentType.TEXT_TO_IMAGE_V3.value
                    )
                ):
                    updated_log["message"] = "inline image redacted for space"
                chat_log[idx] = updated_log
            return_log["chat"] = chat_log

            writer.writerow(
                [
                    conversation.id,
                    conversation.user,
                    conversation.created_at,
                    conversation.updated_at,
                    json.dumps(return_log),
                ]
            )

        return response

    export_selected_minimal_objects.short_description = "Export selected conversations (minimal)"  # type: ignore

    def get_actions(self, request):
        actions = super().get_actions(request)
        if not request.user.is_superuser:
            if "export_selected_objects" in actions:
                del actions["export_selected_objects"]
            if "export_selected_minimal_objects" in actions:
                del actions["export_selected_minimal_objects"]
        return actions
