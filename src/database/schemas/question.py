from typing import List

from pydantic import BaseModel


class CreateQuestionSchema(BaseModel):
    question: str


class QuestionSchema(BaseModel):
    question: str

    class Config:
        orm_mode = True


class QuestionsSchema(BaseModel):
    questions: List[QuestionSchema]

    class Config:
        orm_mode = True
