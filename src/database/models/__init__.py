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


class GoogleUser(models.Model):
    user = models.OneToOneField(KhojUser, on_delete=models.CASCADE)
    sub = models.CharField(max_length=200)
    azp = models.CharField(max_length=200)
    email = models.CharField(max_length=200)
    name = models.CharField(max_length=200)
    given_name = models.CharField(max_length=200)
    family_name = models.CharField(max_length=200)
    picture = models.CharField(max_length=200)
    locale = models.CharField(max_length=200)

    def __str__(self):
        return self.name
