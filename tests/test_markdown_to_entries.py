import os
from pathlib import Path

from khoj.processor.content.markdown.markdown_to_entries import MarkdownToEntries
from khoj.utils.fs_syncer import get_markdown_files
from khoj.utils.rawconfig import TextContentConfig


def test_extract_markdown_with_no_headings(tmp_path):
    "Convert markdown file with no heading to entry format."
    # Arrange
    entry = f"""
    - Bullet point 1
    - Bullet point 2
    """
    data = {
        f"{tmp_path}": entry,
    }
    expected_heading = f"# {tmp_path}"

    # Act
    # Extract Entries from specified Markdown files
    entries = MarkdownToEntries.extract_markdown_entries(markdown_files=data, max_tokens=3)

    # Assert
    assert len(entries) == 1
    # Ensure raw entry with no headings do not get heading prefix prepended
    assert not entries[0].raw.startswith("#")
    # Ensure compiled entry has filename prepended as top level heading
    assert entries[0].compiled.startswith(expected_heading)
    # Ensure compiled entry also includes the file name
    assert str(tmp_path) in entries[0].compiled


def test_extract_single_markdown_entry(tmp_path):
    "Convert markdown from single file to entry format."
    # Arrange
    entry = f"""### Heading
    \t\r
    Body Line 1
    """
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Markdown files
    entries = MarkdownToEntries.extract_markdown_entries(markdown_files=data, max_tokens=3)

    # Assert
    assert len(entries) == 1


def test_extract_multiple_markdown_entries(tmp_path):
    "Convert multiple markdown from single file to entry format."
    # Arrange
    entry = f"""
### Heading 1
    \t\r
    Heading 1 Body Line 1
### Heading 2
    \t\r
    Heading 2 Body Line 2
    """
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Markdown files
    entries = MarkdownToEntries.extract_markdown_entries(markdown_files=data, max_tokens=3)

    # Assert
    assert len(entries) == 2
    # Ensure entry compiled strings include the markdown files they originate from
    assert all([tmp_path.stem in entry.compiled for entry in entries])


def test_extract_entries_with_different_level_headings(tmp_path):
    "Extract markdown entries with different level headings."
    # Arrange
    entry = f"""
# Heading 1
## Sub-Heading 1.1
# Heading 2
"""
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Markdown files
    entries = MarkdownToEntries.extract_markdown_entries(markdown_files=data, max_tokens=3)

    # Assert
    assert len(entries) == 2
    assert entries[0].raw == "# Heading 1\n## Sub-Heading 1.1", "Ensure entry includes heading ancestory"
    assert entries[1].raw == "# Heading 2\n"


def test_extract_entries_with_non_incremental_heading_levels(tmp_path):
    "Extract markdown entries when deeper child level before shallower child level."
    # Arrange
    entry = f"""
# Heading 1
#### Sub-Heading 1.1
## Sub-Heading 1.2
# Heading 2
"""
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Markdown files
    entries = MarkdownToEntries.extract_markdown_entries(markdown_files=data, max_tokens=3)

    # Assert
    assert len(entries) == 3
    assert entries[0].raw == "# Heading 1\n#### Sub-Heading 1.1", "Ensure entry includes heading ancestory"
    assert entries[1].raw == "# Heading 1\n## Sub-Heading 1.2", "Ensure entry includes heading ancestory"
    assert entries[2].raw == "# Heading 2\n"


def test_extract_entries_with_text_before_headings(tmp_path):
    "Extract markdown entries with some text before any headings."
    # Arrange
    entry = f"""
Text before headings
# Heading 1
body line 1
## Heading 2
body line 2
"""
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Markdown files
    entries = MarkdownToEntries.extract_markdown_entries(markdown_files=data, max_tokens=3)

    # Assert
    assert len(entries) == 3
    assert entries[0].raw == "\nText before headings"
    assert entries[1].raw == "# Heading 1\nbody line 1"
    assert entries[2].raw == "# Heading 1\n## Heading 2\nbody line 2\n", "Ensure raw entry includes heading ancestory"


def test_parse_markdown_file_into_single_entry_if_small(tmp_path):
    "Parse markdown file into single entry if it fits within the token limits."
    # Arrange
    entry = f"""
# Heading 1
body line 1
## Subheading 1.1
body line 1.1
"""
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Markdown files
    entries = MarkdownToEntries.extract_markdown_entries(markdown_files=data, max_tokens=12)

    # Assert
    assert len(entries) == 1
    assert entries[0].raw == entry


def test_parse_markdown_entry_with_children_as_single_entry_if_small(tmp_path):
    "Parse markdown entry with child headings as single entry if it fits within the tokens limits."
    # Arrange
    entry = f"""
# Heading 1
body line 1
## Subheading 1.1
body line 1.1
# Heading 2
body line 2
## Subheading 2.1
longer body line 2.1
"""
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Markdown files
    entries = MarkdownToEntries.extract_markdown_entries(markdown_files=data, max_tokens=12)

    # Assert
    assert len(entries) == 3
    assert (
        entries[0].raw == "# Heading 1\nbody line 1\n## Subheading 1.1\nbody line 1.1"
    ), "First entry includes children headings"
    assert entries[1].raw == "# Heading 2\nbody line 2", "Second entry does not include children headings"
    assert (
        entries[2].raw == "# Heading 2\n## Subheading 2.1\nlonger body line 2.1\n"
    ), "Third entry is second entries child heading"


def test_get_markdown_files(tmp_path):
    "Ensure Markdown files specified via input-filter, input-files extracted"
    # Arrange
    # Include via input-filter globs
    group1_file1 = create_file(tmp_path, filename="group1-file1.md")
    group1_file2 = create_file(tmp_path, filename="group1-file2.md")
    group2_file1 = create_file(tmp_path, filename="group2-file1.markdown")
    group2_file2 = create_file(tmp_path, filename="group2-file2.markdown")
    # Include via input-file field
    file1 = create_file(tmp_path, filename="notes.md")
    # Not included by any filter
    create_file(tmp_path, filename="not-included-markdown.md")
    create_file(tmp_path, filename="not-included-text.txt")

    expected_files = set(
        [os.path.join(tmp_path, file.name) for file in [group1_file1, group1_file2, group2_file1, group2_file2, file1]]
    )

    # Setup input-files, input-filters
    input_files = [tmp_path / "notes.md"]
    input_filter = [tmp_path / "group1*.md", tmp_path / "group2*.markdown"]

    markdown_config = TextContentConfig(
        input_files=input_files,
        input_filter=[str(filter) for filter in input_filter],
        compressed_jsonl=tmp_path / "test.jsonl",
        embeddings_file=tmp_path / "test_embeddings.jsonl",
    )

    # Act
    extracted_org_files = get_markdown_files(markdown_config)

    # Assert
    assert len(extracted_org_files) == 5
    assert set(extracted_org_files.keys()) == expected_files


# Helper Functions
def create_file(tmp_path: Path, entry=None, filename="test.md"):
    markdown_file = tmp_path / filename
    markdown_file.touch()
    if entry:
        markdown_file.write_text(entry)
    return markdown_file
