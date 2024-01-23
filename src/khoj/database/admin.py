import csv
import json

from django.contrib import admin
from django.contrib.auth.admin import UserAdmin
from django.http import HttpResponse

from khoj.database.models import (
    ChatModelOptions,
    ClientApplication,
    Conversation,
    KhojUser,
    OfflineChatProcessorConversationConfig,
    OpenAIProcessorConversationConfig,
    ReflectiveQuestion,
    SearchModelConfig,
    SpeechToTextModelOptions,
    Subscription,
    TextToImageModelConfig,
    UserSearchModelConfig,
)


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
    search_fields = ("email", "username", "phone_number")
    filter_horizontal = ("groups", "user_permissions")

    fieldsets = (("Personal info", {"fields": ("phone_number",)}),) + UserAdmin.fieldsets


admin.site.register(KhojUser, KhojUserAdmin)

admin.site.register(ChatModelOptions)
admin.site.register(SpeechToTextModelOptions)
admin.site.register(OpenAIProcessorConversationConfig)
admin.site.register(OfflineChatProcessorConversationConfig)
admin.site.register(SearchModelConfig)
admin.site.register(Subscription)
admin.site.register(ReflectiveQuestion)
admin.site.register(UserSearchModelConfig)
admin.site.register(TextToImageModelConfig)
admin.site.register(ClientApplication)


@admin.register(Conversation)
class ConversationAdmin(admin.ModelAdmin):
    list_display = (
        "id",
        "user",
        "created_at",
        "updated_at",
        "client",
    )
    search_fields = ("conversation_id",)
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
                    and log["intent"]["type"] == "text-to-image"
                ):
                    log["message"] = "image redacted for space"
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
                    and log["intent"]["type"] == "text-to-image"
                ):
                    updated_log["message"] = "image redacted for space"
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
