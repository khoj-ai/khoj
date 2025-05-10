import logging
from abc import ABC, abstractmethod
from typing import List, Literal, Optional, Union

from pydantic import BaseModel

from khoj.database.models import ChatModel
from khoj.processor.conversation.utils import commit_conversation_trace
from khoj.processor.operator.operator_actions import OperatorAction
from khoj.processor.operator.operator_environment_base import EnvState, EnvStepResult
from khoj.utils.helpers import get_chat_usage_metrics, is_promptrace_enabled

logger = logging.getLogger(__name__)


class AgentActResult(BaseModel):
    actions: List[OperatorAction] = []
    action_results: List[dict] = []  # Model-specific format
    rendered_response: Optional[dict] = None


class AgentMessage(BaseModel):
    role: Literal["user", "assistant", "system", "environment"]
    content: Union[str, List]


class OperatorAgent(ABC):
    def __init__(self, query: str, vision_model: ChatModel, max_iterations: int, tracer: dict):
        self.query = query
        self.vision_model = vision_model
        self.max_iterations = max_iterations
        self.tracer = tracer
        self.messages: List[AgentMessage] = []

    @abstractmethod
    async def act(self, current_state: EnvState) -> AgentActResult:
        pass

    @abstractmethod
    def add_action_results(self, env_steps: list[EnvStepResult], agent_action: AgentActResult) -> None:
        """Track results of agent actions on the environment."""
        pass

    async def summarize(self, summarize_prompt: str, current_state: EnvState) -> str:
        """Summarize the agent's actions and results."""
        self.messages.append(AgentMessage(role="user", content=summarize_prompt))
        await self.act(current_state)
        if not self.messages:
            return "No actions to summarize."
        return self.compile_response(self.messages[-1].content)

    @abstractmethod
    def compile_response(self, response: List | str) -> str:
        pass

    @abstractmethod
    def _render_response(self, response: List, screenshot: Optional[str]) -> dict:
        pass

    @abstractmethod
    def _format_message_for_api(self, message: AgentMessage) -> List:
        pass

    def _update_usage(self, input_tokens: int, output_tokens: int, cache_read: int = 0, cache_write: int = 0):
        self.tracer["usage"] = get_chat_usage_metrics(
            self.vision_model.name, input_tokens, output_tokens, cache_read, cache_write, usage=self.tracer.get("usage")
        )
        logger.debug(f"Operator usage by {self.vision_model.model_type}: {self.tracer['usage']}")

    def _commit_trace(self):
        self.tracer["chat_model"] = self.vision_model.name
        if is_promptrace_enabled() and len(self.messages) > 1:
            compiled_messages = [
                AgentMessage(role=msg.role, content=self.compile_response(msg.content)) for msg in self.messages
            ]
            commit_conversation_trace(compiled_messages[:-1], compiled_messages[-1].content, self.tracer)

    def reset(self):
        """Reset the agent state."""
        self.messages = []
