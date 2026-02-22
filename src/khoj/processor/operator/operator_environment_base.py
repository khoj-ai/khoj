from abc import ABC, abstractmethod
from enum import Enum
from typing import Literal, Optional

from pydantic import BaseModel

from khoj.processor.operator.operator_actions import OperatorAction


class EnvironmentType(Enum):
    """Type of environment to operate."""

    COMPUTER = "computer"
    BROWSER = "browser"


class EnvState(BaseModel):
    height: int
    width: int
    screenshot: Optional[str] = None
    url: Optional[str] = None


class EnvStepResult(BaseModel):
    type: Literal["text", "image"] = "text"
    output: Optional[str | dict] = None
    error: Optional[str] = None
    current_url: Optional[str] = None
    screenshot_base64: Optional[str] = None


class Environment(ABC):
    @abstractmethod
    async def start(self, width: int, height: int) -> None:
        pass

    @abstractmethod
    async def step(self, action: OperatorAction) -> EnvStepResult:
        pass

    @abstractmethod
    async def close(self) -> None:
        pass

    @abstractmethod
    async def get_state(self) -> EnvState:
        pass
