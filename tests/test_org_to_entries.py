import os
import re
import time

from khoj.processor.content.org_mode.org_to_entries import OrgToEntries
from khoj.processor.content.text_to_entries import TextToEntries
from khoj.utils.helpers import is_none_or_empty
from khoj.utils.rawconfig import Entry


def test_configure_indexing_heading_only_entries(tmp_path):
    """Ensure entries with empty body are ignored, unless explicitly configured to index heading entries.
    Property drawers not considered Body. Ignore control characters for evaluating if Body empty."""
    # Arrange
    entry = """*** Heading
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
    entry = """*** Heading
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
    entry_text = """First Line
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
    original_entry = """
* Heading 1
body line 1
** Subheading 1.1
body line 1.1
"""
    data = {
        f"{tmp_path}": original_entry,
    }
    expected_entry = """
* Heading 1
body line 1

** Subheading 1.1
body line 1.1

""".lstrip()

    # Act
    # Extract Entries from specified Org files
    extracted_entries = OrgToEntries.extract_org_entries(org_files=data, max_tokens=12)
    assert len(extracted_entries) == 2

    # Assert
    assert len(extracted_entries[1]) == 1
    assert extracted_entries[1][-1].raw == expected_entry


def test_parse_org_entry_with_children_as_single_entry_if_small(tmp_path):
    "Parse org entry with child headings as single entry only if it fits within the tokens limits."
    # Arrange
    entry = """
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
    entry = """
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
    entry = """*** Heading
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
    entry = """
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
    entry = """
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


def test_extract_entries_with_different_level_headings(tmp_path):
    "Extract org entries with different level headings."
    # Arrange
    entry = """
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

    # Assert
    assert len(entries[1]) == 2
    assert entries[1][0].raw == "* Heading 1\n** Sub-Heading 1.1\n", "Ensure entry includes heading ancestory"
    assert entries[1][1].raw == "* Heading 2\n"


def test_line_number_tracking_in_recursive_split():
    "Ensure line numbers in URIs are correct after recursive splitting by checking against the actual file."
    # Arrange
    org_file_path = os.path.abspath("tests/data/org/main_readme.org")

    with open(org_file_path, "r") as f:
        org_content = f.read()
    lines = org_content.splitlines()
    data = {org_file_path: org_content}

    # Act
    # Using a small max_tokens to force recursive splitting
    _, entries = OrgToEntries.extract_org_entries(org_files=data, max_tokens=10, index_heading_entries=True)

    # Assert
    assert len(entries) > 0, "No entries were extracted."

    for entry in entries:
        # Extract file path and line number from the entry URI
        # for files uri is expected in format: file:///path/to/file.org#line=5
        match = re.search(r"file://(.*?)#line=(\d+)", entry.uri)
        if not match:
            continue
        filepath_from_uri = match.group(1)
        line_number_from_uri = int(match.group(2))

        # line_number is 1-based, list index is 0-based
        line_in_file = clean(lines[line_number_from_uri - 1])
        next_line_in_file = clean(lines[line_number_from_uri]) if line_number_from_uri < len(lines) else ""

        # Remove ancestor heading lines inserted during post-processing
        first_entry_line = ""
        for line in entry.raw.splitlines():
            if line.startswith("*"):
                first_entry_line = line
            else:
                break  # Stop at the first non-heading line
        # Remove heading prefix from entry.compiled as level changed during post-processing
        cleaned_first_entry_line = first_entry_line.strip()
        # Remove multiple consecutive spaces
        cleaned_first_entry_line = clean(cleaned_first_entry_line)

        assert entry.uri is not None, f"Entry '{entry}' has a None URI."
        assert match is not None, f"URI format is incorrect: {entry.uri}"
        assert filepath_from_uri == org_file_path, (
            f"File path in URI '{filepath_from_uri}' does not match expected '{org_file_path}'"
        )

        # Ensure the first non-heading line in the compiled entry matches the line in the file
        assert (
            cleaned_first_entry_line in line_in_file.strip() or cleaned_first_entry_line in next_line_in_file.strip()
        ), (
            f"First non-heading line '{cleaned_first_entry_line}' in {entry.raw} does not match line {line_number_from_uri} in file: '{line_in_file}' or next line '{next_line_in_file}'"
        )


# Helper Functions
def create_file(tmp_path, entry=None, filename="test.org"):
    org_file = tmp_path / filename
    org_file.touch()
    if entry:
        org_file.write_text(entry)
    return org_file


def clean(text):
    "Normalize spaces in text for easier comparison."
    return re.sub(r"\s+", " ", text)
