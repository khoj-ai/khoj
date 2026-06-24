from khoj.processor.conversation.openai.utils import is_instream_thinking_model
from khoj.utils import constants


class TestIsInstreamThinkingModel:
    """MiniMax M2/M3 and Kimi K2 Thinking emit <think>...</think> reasoning inline
    in the content stream over the OpenAI compatible API. They must be routed
    through the in-stream thought processor so reasoning is parsed out instead of
    leaking into the visible chat response."""

    def test_minimax_thinking_models_detected(self):
        assert is_instream_thinking_model("MiniMax-M2") is True
        assert is_instream_thinking_model("MiniMax-M3") is True

    def test_detection_is_case_insensitive(self):
        assert is_instream_thinking_model("MINIMAX-M3") is True
        assert is_instream_thinking_model("minimax-m2") is True

    def test_kimi_thinking_model_still_detected(self):
        assert is_instream_thinking_model("moonshotai/kimi-k2-thinking") is True

    def test_non_thinking_models_not_detected(self):
        assert is_instream_thinking_model("gpt-4o") is False
        assert is_instream_thinking_model("claude-sonnet-4-0") is False
        assert is_instream_thinking_model("gemini-2.5-flash") is False


class TestMiniMaxModelCost:
    """MiniMax-M3 standard pay-as-you-go pricing ($/M tokens) must be registered
    so usage cost is tracked for the model."""

    def test_minimax_m3_cost_registered(self):
        assert "MiniMax-M3" in constants.model_to_cost
        cost = constants.model_to_cost["MiniMax-M3"]
        assert cost["input"] == 0.6
        assert cost["output"] == 2.4
        assert cost["cache_read"] == 0.12
