import logging
from abc import ABC, abstractmethod
from typing import List, Optional

from pydantic import BaseModel

from khoj.database.models import ChatModel
from khoj.processor.conversation.utils import (
    AgentMessage,
    OperatorRun,
    commit_conversation_trace,
)
from khoj.processor.operator.operator_actions import OperatorAction
from khoj.processor.operator.operator_environment_base import (
    EnvironmentType,
    EnvState,
    EnvStepResult,
)
from khoj.utils.helpers import get_chat_usage_metrics, is_promptrace_enabled

logger = logging.getLogger(__name__)


class AgentActResult(BaseModel):
    actions: List[OperatorAction] = []
    action_results: List[dict] = []  # Model-specific format
    rendered_response: Optional[dict] = None


class OperatorAgent(ABC):
    def __init__(
        self,
        query: str,
        vision_model: ChatModel,
        environment_type: EnvironmentType,
        max_iterations: int,
        max_context: int,
        chat_history: List[AgentMessage] = [],
        previous_trajectory: Optional[OperatorRun] = None,
        tracer: dict = {},
    ):
        self.query = query
        self.vision_model = vision_model
        self.environment_type = environment_type
        self.max_iterations = max_iterations
        self.tracer = tracer
        self.summarize_prompt = f"Use the results of our research to provide a comprehensive, self-contained answer for the target query:\n{query}."

        self.messages: List[AgentMessage] = chat_history
        if previous_trajectory:
            # Remove tool call from previous trajectory as tool call w/o result not supported
            if previous_trajectory.trajectory and previous_trajectory.trajectory[-1].role == "assistant":
                previous_trajectory.trajectory.pop()
            self.messages += previous_trajectory.trajectory
        self.messages += [AgentMessage(role="user", content=query)]

        # Context compression parameters
        self.context_compress_trigger = 2e3  # heuristic to determine compression trigger
        # turns after which compression triggered. scales with model max context size. Minimum 5 turns.
        self.message_limit = 2 * max(5, int(max_context / self.context_compress_trigger))
        # compression ratio determines how many messages to compress down to one
        # e.g. if 5 messages, a compress ratio of 4/5 means compress 5 messages into 1 + keep 1 uncompressed
        self.message_compress_ratio = 4 / 5
        self.compress_length = int(self.message_limit * self.message_compress_ratio)

    @abstractmethod
    async def act(self, current_state: EnvState) -> AgentActResult:
        pass

    @abstractmethod
    def add_action_results(self, env_steps: list[EnvStepResult], agent_action: AgentActResult) -> None:
        """Track results of agent actions on the environment."""
        pass

    async def summarize(self, current_state: EnvState, summarize_prompt: str = None) -> str:
        """Summarize the agent's actions and results."""
        summarize_prompt = summarize_prompt or self.summarize_prompt
        self.messages.append(AgentMessage(role="user", content=summarize_prompt))
        await self.act(current_state)
        if not self.messages:
            return "No actions to summarize."
        return self._compile_response(self.messages[-1].content)

    @abstractmethod
    def _compile_response(self, response: List | str) -> str:
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

    def _commit_trace(self):
        self.tracer["chat_model"] = self.vision_model.name
        if is_promptrace_enabled() and len(self.messages) > 1:
            compiled_messages = [
                AgentMessage(role=msg.role, content=self._compile_response(msg.content)) for msg in self.messages
            ]
            commit_conversation_trace(compiled_messages[:-1], compiled_messages[-1].content, self.tracer)

    def reset(self):
        """Reset the agent state."""
        self.messages = []
