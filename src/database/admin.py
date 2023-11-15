from django.contrib import admin
from django.contrib.auth.admin import UserAdmin

# Register your models here.

from database.models import (
    KhojUser,
    ChatModelOptions,
    OpenAIProcessorConversationConfig,
    OfflineChatProcessorConversationConfig,
    SearchModel,
    Subscription,
)

admin.site.register(KhojUser, UserAdmin)

admin.site.register(ChatModelOptions)
admin.site.register(OpenAIProcessorConversationConfig)
admin.site.register(OfflineChatProcessorConversationConfig)
admin.site.register(SearchModel)
admin.site.register(Subscription)
