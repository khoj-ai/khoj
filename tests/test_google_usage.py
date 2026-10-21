from google.genai import types as gtypes

from khoj.processor.conversation.google.utils import get_usage_token_counts


def test_usage_token_counts_of_a_response_with_usage():
    response = gtypes.GenerateContentResponse(
        usage_metadata=gtypes.GenerateContentResponseUsageMetadata(
            prompt_token_count=10,
            candidates_token_count=5,
            thoughts_token_count=3,
            cached_content_token_count=2,
        )
    )

    assert get_usage_token_counts(response) == (10, 5, 3, 2)


def test_usage_token_counts_default_to_zero_for_missing_counts():
    response = gtypes.GenerateContentResponse(
        usage_metadata=gtypes.GenerateContentResponseUsageMetadata(prompt_token_count=10)
    )

    assert get_usage_token_counts(response) == (10, 0, 0, 0)


def test_usage_token_counts_of_a_response_without_usage_metadata():
    # A blocked prompt can come back as a chunk with prompt feedback but no usage metadata
    blocked = gtypes.GenerateContentResponse(
        prompt_feedback=gtypes.GenerateContentResponsePromptFeedback(block_reason=gtypes.BlockedReason.SAFETY)
    )

    assert blocked.usage_metadata is None
    assert get_usage_token_counts(blocked) == (0, 0, 0, 0)
    assert get_usage_token_counts(None) == (0, 0, 0, 0)
