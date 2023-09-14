from typing import Type, TypeVar

from django.db import models

from fastapi import HTTPException

from database.models import Question, Answer

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
