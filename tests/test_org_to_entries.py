import os
import re
import time

from khoj.processor.content.org_mode.org_to_entries import OrgToEntries
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.utils.fs_syncer import get_org_files
from khoj.utils.helpers import is_none_or_empty
from khoj.utils.rawconfig import Entry, TextContentConfig


def test_configure_indexing_heading_only_entries(tmp_path):
    """Ensure entries with empty body are ignored, unless explicitly configured to index heading entries.
    Property drawers not considered Body. Ignore control characters for evaluating if Body empty."""
    # Arrange
    entry = f"""*** Heading
    :PROPERTIES:
    :ID:       42-42-42
    :END:
    \t \r
    """

    data = {
        f"{tmp_path}": entry,
    }

    for index_heading_entries in [True, False]:
        # Act
        # Extract entries into jsonl from specified Org files
        entries = OrgToEntries.extract_org_entries(
            org_files=data, index_heading_entries=index_heading_entries, max_tokens=3
        )

        # Assert
        if index_heading_entries:
            # Entry with empty body indexed when index_heading_entries set to True
            assert len(entries) == 2
            assert len(entries[1]) == 1
        else:
            # Entry with empty body ignored when index_heading_entries set to False
            assert len(entries) == 2
            assert is_none_or_empty(entries[1])


def test_extract_entries_when_child_headings_have_same_prefix():
    """Extract org entries from entries having child headings with same prefix.
    Prevents regressions like the one fixed in PR #840.
    """
    # Arrange
    tmp_path = "tests/data/org/same_prefix_headings.org"
    entry: str = """
** 1
*** 1.1
**** 1.1.2
""".strip()
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Org files
    start = time.time()
    entries = OrgToEntries.extract_org_entries(org_files=data, max_tokens=2)
    end = time.time()
    indexing_time = end - start

    # Assert
    explanation_msg = (
        "It should not take more than 6 seconds to index. Entry extraction may have gone into an infinite loop."
    )
    assert indexing_time < 6 * len(entries), explanation_msg


def test_entry_split_when_exceeds_max_tokens():
    "Ensure entries with compiled words exceeding max_tokens are split."
    # Arrange
    tmp_path = "/tmp/test.org"
    entry = f"""*** Heading
    \t\r
    Body Line
    """
    data = {
        f"{tmp_path}": entry,
    }
    expected_heading = f"* {tmp_path}\n** Heading"

    # Act
    # Extract Entries from specified Org files
    entries = OrgToEntries.extract_org_entries(org_files=data)
    assert len(entries) == 2
    # Split each entry from specified Org files by max tokens
    entries = TextToEntries.split_entries_by_max_tokens(entries[1], max_tokens=5)

    # Assert
    assert len(entries) == 2
    # Ensure compiled entries split by max tokens start with entry heading (for search context)
    assert all([entry.compiled.startswith(expected_heading) for entry in entries])


def test_entry_split_drops_large_words():
    "Ensure entries drops words larger than specified max word length from compiled version."
    # Arrange
    entry_text = f"""First Line
dog=1\n\r\t
cat=10
car=4
book=2
"""
    entry = Entry(raw=entry_text, compiled=entry_text)

    # Act
    # Split entry by max words and drop words larger than max word length
    processed_entry = TextToEntries.split_entries_by_max_tokens([entry], max_word_length=5)[0]

    # Assert
    # Ensure words larger than max word length are dropped
    # Ensure newline characters are considered as word boundaries for splitting words. See #620
    words_to_keep = ["First", "Line", "dog=1", "car=4"]
    words_to_drop = ["cat=10", "book=2"]
    assert all([word for word in words_to_keep if word in processed_entry.compiled])
    assert not any([word for word in words_to_drop if word in processed_entry.compiled])
    assert len(processed_entry.compiled.split()) == len(entry_text.split()) - 2


def test_parse_org_file_into_single_entry_if_small(tmp_path):
    "Parse org file into single entry if it fits within the token limits."
    # Arrange
    original_entry = f"""
* Heading 1
body line 1
** Subheading 1.1
body line 1.1
"""
    data = {
        f"{tmp_path}": original_entry,
    }
    expected_entry = f"""
* Heading 1
body line 1

** Subheading 1.1
body line 1.1

""".lstrip()

    # Act
    # Extract Entries from specified Org files
    extracted_entries = OrgToEntries.extract_org_entries(org_files=data, max_tokens=12)
    assert len(extracted_entries) == 2
    for entry in extracted_entries[1]:
        entry.raw = clean(entry.raw)

    # Assert
    assert len(extracted_entries[1]) == 1
    assert entry.raw == expected_entry


def test_parse_org_entry_with_children_as_single_entry_if_small(tmp_path):
    "Parse org entry with child headings as single entry only if it fits within the tokens limits."
    # Arrange
    entry = f"""
* Heading 1
body line 1
** Subheading 1.1
body line 1.1
* Heading 2
body line 2
** Subheading 2.1
longer body line 2.1
"""
    data = {
        f"{tmp_path}": entry,
    }
    first_expected_entry = f"""
* {tmp_path}
** Heading 1.
 body line 1

*** Subheading 1.1.
 body line 1.1

""".lstrip()
    second_expected_entry = f"""
* {tmp_path}
** Heading 2.
 body line 2

""".lstrip()
    third_expected_entry = f"""
* {tmp_path} / Heading 2
** Subheading 2.1.
 longer body line 2.1

""".lstrip()

    # Act
    # Extract Entries from specified Org files
    extracted_entries = OrgToEntries.extract_org_entries(org_files=data, max_tokens=12)

    # Assert
    assert len(extracted_entries) == 2
    assert len(extracted_entries[1]) == 3
    assert extracted_entries[1][0].compiled == first_expected_entry, "First entry includes children headings"
    assert extracted_entries[1][1].compiled == second_expected_entry, "Second entry does not include children headings"
    assert extracted_entries[1][2].compiled == third_expected_entry, "Third entry is second entries child heading"


def test_separate_sibling_org_entries_if_all_cannot_fit_in_token_limit(tmp_path):
    "Parse org sibling entries as separate entries only if it fits within the tokens limits."
    # Arrange
    entry = f"""
* Heading 1
body line 1
** Subheading 1.1
body line 1.1
* Heading 2
body line 2
** Subheading 2.1
body line 2.1
* Heading 3
body line 3
** Subheading 3.1
body line 3.1
"""
    data = {
        f"{tmp_path}": entry,
    }
    first_expected_entry = f"""
* {tmp_path}
** Heading 1.
 body line 1

*** Subheading 1.1.
 body line 1.1

""".lstrip()
    second_expected_entry = f"""
* {tmp_path}
** Heading 2.
 body line 2

*** Subheading 2.1.
 body line 2.1

""".lstrip()
    third_expected_entry = f"""
* {tmp_path}
** Heading 3.
 body line 3

*** Subheading 3.1.
 body line 3.1

""".lstrip()

    # Act
    # Extract Entries from specified Org files
    # Max tokens = 30 is in the middle of 2 entry (24 tokens) and 3 entry (36 tokens) tokens boundary
    # Where each sibling entry contains 12 tokens per sibling entry * 3 entries = 36 tokens
    extracted_entries = OrgToEntries.extract_org_entries(org_files=data, max_tokens=30)

    # Assert
    assert len(extracted_entries) == 2
    assert len(extracted_entries[1]) == 3
    assert extracted_entries[1][0].compiled == first_expected_entry, "First entry includes children headings"
    assert extracted_entries[1][1].compiled == second_expected_entry, "Second entry includes children headings"
    assert extracted_entries[1][2].compiled == third_expected_entry, "Third entry includes children headings"


def test_entry_with_body_to_entry(tmp_path):
    "Ensure entries with valid body text are loaded."
    # Arrange
    entry = f"""*** Heading
    :PROPERTIES:
    :ID:       42-42-42
    :END:
    \t\r
    Body Line 1
    """
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Org files
    entries = OrgToEntries.extract_org_entries(org_files=data, max_tokens=3)

    # Assert
    assert len(entries) == 2
    assert len(entries[1]) == 1


def test_file_with_entry_after_intro_text_to_entry(tmp_path):
    "Ensure intro text before any headings is indexed."
    # Arrange
    entry = f"""
Intro text

* Entry Heading
  entry body
"""
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Org files
    entries = OrgToEntries.extract_org_entries(org_files=data, max_tokens=3)

    # Assert
    assert len(entries) == 2
    assert len(entries[1]) == 2


def test_file_with_no_headings_to_entry(tmp_path):
    "Ensure files with no heading, only body text are loaded."
    # Arrange
    entry = f"""
    - Bullet point 1
    - Bullet point 2
    """
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Org files
    entries = OrgToEntries.extract_org_entries(org_files=data, max_tokens=3)

    # Assert
    assert len(entries) == 2
    assert len(entries[1]) == 1


def test_get_org_files(tmp_path):
    "Ensure Org files specified via input-filter, input-files extracted"
    # Arrange
    # Include via input-filter globs
    group1_file1 = create_file(tmp_path, filename="group1-file1.org")
    group1_file2 = create_file(tmp_path, filename="group1-file2.org")
    group2_file1 = create_file(tmp_path, filename="group2-file1.org")
    group2_file2 = create_file(tmp_path, filename="group2-file2.org")
    # Include via input-file field
    orgfile1 = create_file(tmp_path, filename="orgfile1.org")
    # Not included by any filter
    create_file(tmp_path, filename="orgfile2.org")
    create_file(tmp_path, filename="text1.txt")

    expected_files = set(
        [
            os.path.join(tmp_path, file.name)
            for file in [group1_file1, group1_file2, group2_file1, group2_file2, orgfile1]
        ]
    )

    # Setup input-files, input-filters
    input_files = [tmp_path / "orgfile1.org"]
    input_filter = [tmp_path / "group1*.org", tmp_path / "group2*.org"]

    org_config = TextContentConfig(
        input_files=input_files,
        input_filter=[str(filter) for filter in input_filter],
        compressed_jsonl=tmp_path / "test.jsonl",
        embeddings_file=tmp_path / "test_embeddings.jsonl",
    )

    # Act
    extracted_org_files = get_org_files(org_config)

    # Assert
    assert len(extracted_org_files) == 5
    assert set(extracted_org_files.keys()) == expected_files


def test_extract_entries_with_different_level_headings(tmp_path):
    "Extract org entries with different level headings."
    # Arrange
    entry = f"""
* Heading 1
** Sub-Heading 1.1
* Heading 2
"""
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Org files
    entries = OrgToEntries.extract_org_entries(org_files=data, index_heading_entries=True, max_tokens=3)
    assert len(entries) == 2
    for entry in entries[1]:
        entry.raw = clean(f"{entry.raw}")

    # Assert
    assert len(entries[1]) == 2
    assert entries[1][0].raw == "* Heading 1\n** Sub-Heading 1.1\n", "Ensure entry includes heading ancestory"
    assert entries[1][1].raw == "* Heading 2\n"


# Helper Functions
def create_file(tmp_path, entry=None, filename="test.org"):
    org_file = tmp_path / filename
    org_file.touch()
    if entry:
        org_file.write_text(entry)
    return org_file


def clean(entry):
    "Remove properties from entry for easier comparison."
    return re.sub(r"\n:PROPERTIES:(.*?):END:", "", entry, flags=re.DOTALL)
