from django.db import models
from django.contrib.auth.models import AbstractUser


class Question(models.Model):
    question = models.CharField(max_length=200)

    def __str__(self):
        return self.question


class Answer(models.Model):
    answer = models.CharField(max_length=200)
    votes = models.IntegerField(default=0)
    question = models.ForeignKey(Question, on_delete=models.CASCADE)

    def __str__(self):
        return self.answer


class KhojUser(AbstractUser):
    pass
