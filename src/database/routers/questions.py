from typing import List

from fastapi import APIRouter, Depends, Request

from database import adapters
from database.models import Question

from database.schemas.question import QuestionSchema, QuestionsSchema, CreateQuestionSchema

question_router = APIRouter()


@question_router.get("/", response_model=List[QuestionSchema])
async def get(request: Request, questions: List[Question] = Depends(adapters.retrieve_questions)) -> QuestionsSchema:
    return questions


@question_router.get("/{question_id}", response_model=QuestionSchema)
async def get(request: Request, question: Question = Depends(adapters.retrieve_question)) -> QuestionSchema:
    return question


@question_router.post("/", response_model=QuestionSchema)
async def post(request: Request, question: CreateQuestionSchema) -> QuestionSchema:
    question = await adapters.create_question(question.question)
    return question
