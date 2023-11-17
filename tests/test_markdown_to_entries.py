# Standard Packages
import json
from pathlib import Path
import os

# Internal Packages
from khoj.processor.data_sources.markdown.markdown_to_entries import MarkdownToEntries
from khoj.utils.fs_syncer import get_markdown_files
from khoj.utils.rawconfig import TextContentConfig


def test_markdown_file_with_no_headings_to_jsonl(tmp_path):
    "Convert files with no heading to jsonl."
    # Arrange
    entry = f"""
    - Bullet point 1
    - Bullet point 2
    """
    data = {
        f"{tmp_path}": entry,
    }
    expected_heading = f"# {tmp_path.stem}"

    # Act
    # Extract Entries from specified Markdown files
    entry_nodes, file_to_entries = MarkdownToEntries.extract_markdown_entries(markdown_files=data)

    # Process Each Entry from All Notes Files
    jsonl_string = MarkdownToEntries.convert_markdown_maps_to_jsonl(
        MarkdownToEntries.convert_markdown_entries_to_maps(entry_nodes, file_to_entries)
    )
    jsonl_data = [json.loads(json_string) for json_string in jsonl_string.splitlines()]

    # Assert
    assert len(jsonl_data) == 1
    # Ensure raw entry with no headings do not get heading prefix prepended
    assert not jsonl_data[0]["raw"].startswith("#")
    # Ensure compiled entry has filename prepended as top level heading
    assert jsonl_data[0]["compiled"].startswith(expected_heading)


def test_single_markdown_entry_to_jsonl(tmp_path):
    "Convert markdown entry from single file to jsonl."
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
    entries, entry_to_file_map = MarkdownToEntries.extract_markdown_entries(markdown_files=data)

    # Process Each Entry from All Notes Files
    jsonl_string = MarkdownToEntries.convert_markdown_maps_to_jsonl(
        MarkdownToEntries.convert_markdown_entries_to_maps(entries, entry_to_file_map)
    )
    jsonl_data = [json.loads(json_string) for json_string in jsonl_string.splitlines()]

    # Assert
    assert len(jsonl_data) == 1


def test_multiple_markdown_entries_to_jsonl(tmp_path):
    "Convert multiple markdown entries from single file to jsonl."
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
    entry_strings, entry_to_file_map = MarkdownToEntries.extract_markdown_entries(markdown_files=data)
    entries = MarkdownToEntries.convert_markdown_entries_to_maps(entry_strings, entry_to_file_map)

    # Process Each Entry from All Notes Files
    jsonl_string = MarkdownToEntries.convert_markdown_maps_to_jsonl(entries)
    jsonl_data = [json.loads(json_string) for json_string in jsonl_string.splitlines()]

    # Assert
    assert len(jsonl_data) == 2
    # Ensure entry compiled strings include the markdown files they originate from
    assert all([tmp_path.stem in entry.compiled for entry in entries])


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


def test_extract_entries_with_different_level_headings(tmp_path):
    "Extract markdown entries with different level headings."
    # Arrange
    entry = f"""
# Heading 1
## Heading 2
"""
    data = {
        f"{tmp_path}": entry,
    }

    # Act
    # Extract Entries from specified Markdown files
    entries, _ = MarkdownToEntries.extract_markdown_entries(markdown_files=data)

    # Assert
    assert len(entries) == 2
    assert entries[0] == "# Heading 1"
    assert entries[1] == "## Heading 2"


# Helper Functions
def create_file(tmp_path: Path, entry=None, filename="test.md"):
    markdown_file = tmp_path / filename
    markdown_file.touch()
    if entry:
        markdown_file.write_text(entry)
    return markdown_file
