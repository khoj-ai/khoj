"""
ASGI config for app project.

It exposes the ASGI callable as a module-level variable named ``application``.

For more information on this file, see
https://docs.djangoproject.com/en/4.2/howto/deployment/asgi/
"""

import os
import uvicorn

from django.core.asgi import get_asgi_application

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "app.settings")

print("ASGI SETTINGS: ", os.environ.get("DJANGO_SETTINGS_MODULE"))
from django.conf import settings

application = get_asgi_application()

# Print application settings
print("ASGI APPLICATION: ", settings.SETTINGS_MODULE)
print(settings.INSTALLED_APPS)

"""
FastAPI settings
"""

from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles

fastapi_app = FastAPI()

from database.routers import question_router

# routers
fastapi_app.include_router(question_router, tags=["questions"], prefix="/question")

# to mount Django
fastapi_app.mount("/django", application, name="django")
fastapi_app.mount("/static", StaticFiles(directory="static"), name="static")
# fastapi_app.mount("/media", StaticFiles(directory="media"), name="media")
