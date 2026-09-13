import pytest

def sanitize_search_query(query: str, max_chars: int = 500) -> str:
    """Sanitizes user search query string by collapsing whitespace and clamping length."""
    if not query:
        return ""
    # Collapse multiple whitespaces/newlines
    cleaned = " ".join(query.split())
    if len(cleaned) > max_chars:
        return cleaned[:max_chars].rstrip()
    return cleaned

def test_search_query_collapses_whitespace():
    raw = "   machine   learning \n\n algorithms   "
    assert sanitize_search_query(raw) == "machine learning algorithms"

def test_search_query_length_clamping():
    long_query = "a" * 600
    clamped = sanitize_search_query(long_query, max_chars=100)
    assert len(clamped) == 100
    assert clamped == "a" * 100

def test_search_query_empty_handling():
    assert sanitize_search_query("") == ""
    assert sanitize_search_query("   ") == ""
