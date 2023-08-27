# Standard Packages
import json
from pathlib import Path

# Internal Packages
from khoj.processor.plaintext.plaintext_to_jsonl import PlaintextToJsonl


def test_plaintext_file(tmp_path):
    "Convert files with no heading to jsonl."
    # Arrange
    entry = f"""
    Hi, I am a plaintext file and I have some plaintext words.
    """
    plaintextfile = create_file(tmp_path, entry)

    filename = plaintextfile.stem

    # Act
    # Extract Entries from specified plaintext files
    file_to_entries = PlaintextToJsonl.extract_plaintext_entries(plaintext_files=[str(plaintextfile)])

    maps = PlaintextToJsonl.convert_plaintext_entries_to_maps(file_to_entries)

    # Convert each entry.file to absolute path to make them JSON serializable
    for map in maps:
        map.file = str(Path(map.file).absolute())

    # Process Each Entry from All Notes Files
    jsonl_string = PlaintextToJsonl.convert_entries_to_jsonl(maps)
    jsonl_data = [json.loads(json_string) for json_string in jsonl_string.splitlines()]

    # Assert
    assert len(jsonl_data) == 1
    # Ensure raw entry with no headings do not get heading prefix prepended
    assert not jsonl_data[0]["raw"].startswith("#")
    # Ensure compiled entry has filename prepended as top level heading
    assert jsonl_data[0]["compiled"] == f"{filename}\n{entry}"


def test_get_plaintext_files(tmp_path):
    "Ensure Plaintext files specified via input-filter, input-files extracted"
    # Arrange
    # Include via input-filter globs
    group1_file1 = create_file(tmp_path, filename="group1-file1.md")
    group1_file2 = create_file(tmp_path, filename="group1-file2.md")

    group2_file1 = create_file(tmp_path, filename="group2-file1.markdown")
    group2_file2 = create_file(tmp_path, filename="group2-file2.markdown")
    group2_file3 = create_file(tmp_path, filename="group2-file3.mbox")
    group2_file4 = create_file(tmp_path, filename="group2-file4.html")
    # Include via input-file field
    file1 = create_file(tmp_path, filename="notes.txt")
    # Include unsupported file types
    create_file(tmp_path, filename="group2-unincluded.py")
    create_file(tmp_path, filename="group2-unincluded.csv")
    create_file(tmp_path, filename="group2-unincluded.csv")
    # Not included by any filter
    create_file(tmp_path, filename="not-included-markdown.md")
    create_file(tmp_path, filename="not-included-text.txt")

    expected_files = sorted(
        map(str, [group1_file1, group1_file2, group2_file1, group2_file2, file1, group2_file3, group2_file4])
    )

    # Setup input-files, input-filters
    input_files = [tmp_path / "notes.txt"]
    input_filter = [tmp_path / "group1*.md", tmp_path / "group2*.*"]

    # Act
    extracted_plaintext_files = PlaintextToJsonl.get_plaintext_files(input_files, input_filter)

    # Assert
    assert len(extracted_plaintext_files) == 7
    assert set(extracted_plaintext_files) == set(expected_files)


def test_parse_html_plaintext_file(content_config):
    "Ensure HTML files are parsed correctly"
    # Arrange
    # Setup input-files, input-filters
    input_files = content_config.plaintext.input_files
    input_filter = content_config.plaintext.input_filter

    # Act
    extracted_plaintext_files = PlaintextToJsonl.get_plaintext_files(input_files, input_filter)
    file_to_entries = PlaintextToJsonl.extract_plaintext_entries(extracted_plaintext_files)
    maps = PlaintextToJsonl.convert_plaintext_entries_to_maps(file_to_entries)

    # Assert
    assert len(maps) == 1
    assert "<div>" not in maps[0].raw


# Helper Functions
def create_file(tmp_path: Path, entry=None, filename="test.md"):
    file_ = tmp_path / filename
    file_.touch()
    if entry:
        file_.write_text(entry)
    return file_
