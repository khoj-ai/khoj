from typing import Type, TypeVar

from django.db import models
from django.contrib.auth import authenticate, login
from django.middleware.csrf import rotate_token
from django.contrib.sessions.backends.db import SessionStore
from django.contrib.auth.models import User

# Import sync_to_async from Django Channels
from asgiref.sync import sync_to_async

from fastapi import HTTPException

from database.models import Question, Answer, KhojUser

ModelType = TypeVar("ModelType", bound=models.Model)


async def retrieve_object(model_class: Type[ModelType], id: int) -> ModelType:
    instance = await model_class.objects.filter(id=id).afirst()
    if not instance:
        raise HTTPException(status_code=404, detail=f"{model_class.__name__} not found")
    return instance


async def retrieve_question(id: int) -> Question:
    return await retrieve_object(Question, id)


async def create_question(question: str) -> Question:
    return await Question.objects.acreate(question=question)


async def retrieve_answer(id: int) -> Answer:
    return await retrieve_object(Answer, id)


async def retrieve_questions():
    return [q async for q in Question.objects.all()]


async def retrieve_answers():
    return [a async for a in Answer.objects.all()]


async def retrieve_user(session_id: str) -> KhojUser:
    session = SessionStore(session_key=session_id)
    if not await sync_to_async(session.exists)(session_key=session_id):
        raise HTTPException(status_code=401, detail="Invalid session")
    session_data = await sync_to_async(session.load)()
    user = await KhojUser.objects.filter(id=session_data.get("_auth_user_id")).afirst()
    if not user:
        raise HTTPException(status_code=401, detail="Invalid user")
    return user
