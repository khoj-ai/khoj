# System Packages
import pytest
import logging

from khoj.database.adapters import FileObjectAdapters
from khoj.database.models import KhojUser
from khoj.routers.helpers import grep_files

logger = logging.getLogger(__name__)


@pytest.fixture
@pytest.mark.django_db
@pytest.mark.asyncio
async def default_user():
    user, _ = await KhojUser.objects.aget_or_create(
        username="test_user",
        password="test_password",
        email="test@example.com",
    )
    return user


@pytest.mark.django_db
@pytest.mark.asyncio
async def test_grep_files_simple_match(default_user: KhojUser):
    user = await default_user
    await FileObjectAdapters.adelete_all_file_objects(user=user)
    # Arrange
    await FileObjectAdapters.acreate_file_object(
        user=user,
        file_name="test.txt",
        raw_text="hello world\nthis is a test\nhello again",
    )

    # Act
    results = [
        result
        async for result in grep_files(
            regex_pattern="hello",
            user=user,
        )
    ]

    # Assert
    assert len(results) == 1
    result = results[0]
    assert "Found 2 matches for 'hello' in 1 documents" in result["query"]
    assert "test.txt:1:> hello world" in result["compiled"]
    assert "test.txt:3:> hello again" in result["compiled"]


@pytest.mark.django_db
@pytest.mark.asyncio
async def test_grep_files_no_match(default_user: KhojUser):
    user = await default_user
    await FileObjectAdapters.adelete_all_file_objects(user=user)
    # Arrange
    await FileObjectAdapters.acreate_file_object(
        user=user,
        file_name="test.txt",
        raw_text="this is a test",
    )

    # Act
    results = [
        result
        async for result in grep_files(
            regex_pattern="nonexistent",
            user=user,
        )
    ]

    # Assert
    assert len(results) == 1
    result = results[0]
    assert "Found 0 matches for 'nonexistent' in 0 documents" in result["query"]
    assert "No matches found." in result["compiled"]


@pytest.mark.django_db
@pytest.mark.asyncio
async def test_grep_files_with_path_prefix(default_user: KhojUser):
    user = await default_user
    await FileObjectAdapters.adelete_all_file_objects(user=user)
    # Arrange
    await FileObjectAdapters.acreate_file_object(
        user=user,
        file_name="dir1/test1.txt",
        raw_text="hello from dir1",
    )
    await FileObjectAdapters.acreate_file_object(
        user=user,
        file_name="dir2/test2.txt",
        raw_text="hello from dir2",
    )

    # Act
    results = [
        result
        async for result in grep_files(
            regex_pattern="hello",
            path_prefix="dir1/",
            user=user,
        )
    ]

    # Assert
    assert len(results) == 1
    result = results[0]
    assert "Found 1 matches for 'hello' in 1 documents" in result["query"]
    assert "in dir1/" in result["query"]
    assert "dir1/test1.txt:1:> hello from dir1" in result["compiled"]
    assert "dir2/test2.txt" not in result["compiled"]


@pytest.mark.django_db
@pytest.mark.asyncio
async def test_grep_files_with_context(default_user: KhojUser):
    user = await default_user
    await FileObjectAdapters.adelete_all_file_objects(user=user)
    # Arrange
    await FileObjectAdapters.acreate_file_object(
        user=user,
        file_name="test.txt",
        raw_text="line 1\nline 2\nline 3 (match)\nline 4\nline 5",
    )

    # Act
    results = [
        result
        async for result in grep_files(
            regex_pattern="match",
            lines_before=1,
            lines_after=1,
            user=user,
        )
    ]

    # Assert
    assert len(results) == 1
    result = results[0]
    assert "Found 1 matches for 'match' in 1 documents" in result["query"]
    assert "Showing 1 lines before and 1 lines after" in result["query"]
    assert "test.txt:2:  line 2" in result["compiled"]
    assert "test.txt:3:> line 3 (match)" in result["compiled"]
    assert "test.txt:4:  line 4" in result["compiled"]
    assert "line 1" not in result["compiled"]
    assert "line 5" not in result["compiled"]


@pytest.mark.django_db
@pytest.mark.asyncio
async def test_grep_files_invalid_regex(default_user: KhojUser):
    user = await default_user
    await FileObjectAdapters.adelete_all_file_objects(user=user)
    # Act
    results = [
        result
        async for result in grep_files(
            regex_pattern="[",
            user=user,
        )
    ]

    # Assert
    assert len(results) == 1
    result = results[0]
    assert "Invalid regex pattern" in result["compiled"]


@pytest.mark.django_db
@pytest.mark.asyncio
async def test_grep_files_multiple_files(default_user: KhojUser):
    user = await default_user
    await FileObjectAdapters.adelete_all_file_objects(user=user)
    # Arrange
    await FileObjectAdapters.acreate_file_object(
        user=user,
        file_name="file1.txt",
        raw_text="hello from file1",
    )
    await FileObjectAdapters.acreate_file_object(
        user=user,
        file_name="file2.txt",
        raw_text="hello from file2",
    )

    # Act
    results = [
        result
        async for result in grep_files(
            regex_pattern="hello",
            user=user,
        )
    ]

    # Assert
    assert len(results) == 1
    result = results[0]
    assert "Found 2 matches for 'hello' in 2 documents" in result["query"]
    assert "file1.txt:1:> hello from file1" in result["compiled"]
    assert "file2.txt:1:> hello from file2" in result["compiled"]


@pytest.mark.parametrize(
    "regex_pattern,expected_matches,test_description",
    [
        # Test with (?im) inline flags and ^ anchor
        (r"(?im)^\d{4}-\d{2}-\d{2}.*(sailing|sail|Center for Boats|Captain Sailor)", 1, "inline flags with anchor"),
        # Test with (?i) flag and ^ anchor
        (r"(?i)^\d{4}-\d{2}-\d{2}.*(sailing|sail|Center for Boats|Captain Sailor)", 1, "case insensitive with anchor"),
        # Test without any anchors
        (
            r"(?i)\d{4}-\d{2}-\d{2}.*(sailing|sail|Center for Boats|Captain Sailor)",
            1,
            "case insensitive without anchor",
        ),
        # Test with just the ^ anchor (no inline flags)
        (r"^\d{4}-\d{2}-\d{2}.*(sailing|sail|Center for Boats|Captain Sailor)", 1, "anchor only"),
        # Test without anchors or flags (should still work due to re.IGNORECASE in function)
        (r"\d{4}-\d{2}-\d{2}.*(sailing|sail|center for boats|captain sailor)", 1, "no flags or anchors"),
    ],
)
@pytest.mark.django_db
@pytest.mark.asyncio
async def test_grep_files_financial_entries_regex_patterns(
    default_user: KhojUser, regex_pattern: str, expected_matches: int, test_description: str
):
    user = await default_user
    await FileObjectAdapters.adelete_all_file_objects(user=user)

    # Arrange - Create file with financial ledger content that has prefix text
    ledger_content = """This is a financial ledger file

1984-06-23 * "Al Zaheer, Mediteranean" "Chicken Gyro Plate, Falafel Sandwhich for Bob" #bob
  Expenses:Food:Dining                                             11.55 USD
  Liabilities:People:Bob                                          11.55 USD
  Liabilities:CreditCard:Chase                            -23.10 USD

1984-06-24 * "Center for Boats" "Sailing" #bob
  Expenses:Sports                                                     30 USD
  Liabilities:People:Bob                                           30.0 USD
  Liabilities:CreditCard:Chase                             -60 USD

1984-06-24 * "Safeway" "Groceries" #bob
  Expenses:Food:Groceries                                          11.20 USD
  Liabilities:People:Bob                                          11.20 USD
  Liabilities:CreditCard:Chase                          -22.40 USD"""

    await FileObjectAdapters.acreate_file_object(
        user=user,
        file_name="ledger.txt",
        raw_text=ledger_content,
    )

    # Act - Test the regex pattern
    results = [
        result
        async for result in grep_files(
            regex_pattern=regex_pattern,
            user=user,
        )
    ]

    # Assert
    assert len(results) == 1
    result = results[0]
    logger.info(f"Testing {test_description}: {regex_pattern}")
    logger.info(f"Query: {result['query']}")
    logger.info(f"Compiled: {result['compiled']}")

    # All patterns should find the sailing entry
    assert f"Found {expected_matches} matches" in result["query"]
    assert 'ledger.txt:8:> 1984-06-24 * "Center for Boats" "Sailing" #bob' in result["compiled"]
