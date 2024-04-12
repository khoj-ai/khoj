import os
from pathlib import Path

from khoj.database.models import KhojUser, LocalPlaintextConfig
from khoj.processor.content.plaintext.plaintext_to_entries import PlaintextToEntries
from khoj.utils.fs_syncer import get_plaintext_files
from khoj.utils.rawconfig import TextContentConfig


def test_plaintext_file(tmp_path):
    "Convert files with no heading to jsonl."
    # Arrange
    raw_entry = f"""
    Hi, I am a plaintext file and I have some plaintext words.
    """
    plaintextfile = create_file(tmp_path, raw_entry)

    # Act
    # Extract Entries from specified plaintext files

    data = {
        f"{plaintextfile}": raw_entry,
    }

    entries = PlaintextToEntries.extract_plaintext_entries(data)

    # Convert each entry.file to absolute path to make them JSON serializable
    for entry in entries:
        entry.file = str(Path(entry.file).absolute())

    # Assert
    assert len(entries) == 1
    # Ensure raw entry with no headings do not get heading prefix prepended
    assert not entries[0].raw.startswith("#")
    # Ensure compiled entry has filename prepended as top level heading
    assert entries[0].compiled == f"{plaintextfile}\n{raw_entry}"


def test_get_plaintext_files(tmp_path):
    "Ensure Plaintext files specified via input-filter, input-files extracted"
    # Arrange
    # Include via input-filter globs
    group1_file1 = create_file(tmp_path, filename="group1-file1.md")
    group1_file2 = create_file(tmp_path, filename="group1-file2.md")

    group2_file1 = create_file(tmp_path, filename="group2-file1.markdown")
    group2_file2 = create_file(tmp_path, filename="group2-file2.markdown")
    group2_file4 = create_file(tmp_path, filename="group2-file4.html")
    # Include via input-file field
    file1 = create_file(tmp_path, filename="notes.txt")
    # Include unsupported file types
    create_file(tmp_path, filename="group2-unincluded.py")
    create_file(tmp_path, filename="group2-unincluded.csv")
    create_file(tmp_path, filename="group2-unincluded.csv")
    create_file(tmp_path, filename="group2-file3.mbox")
    # Not included by any filter
    create_file(tmp_path, filename="not-included-markdown.md")
    create_file(tmp_path, filename="not-included-text.txt")

    expected_files = set(
        [
            os.path.join(tmp_path, file.name)
            for file in [group1_file1, group1_file2, group2_file1, group2_file2, group2_file4, file1]
        ]
    )

    # Setup input-files, input-filters
    input_files = [tmp_path / "notes.txt"]
    input_filter = [tmp_path / "group1*.md", tmp_path / "group2*.*"]

    plaintext_config = TextContentConfig(
        input_files=input_files,
        input_filter=[str(filter) for filter in input_filter],
        compressed_jsonl=tmp_path / "test.jsonl",
        embeddings_file=tmp_path / "test_embeddings.jsonl",
    )

    # Act
    extracted_plaintext_files = get_plaintext_files(plaintext_config)

    # Assert
    assert len(extracted_plaintext_files) == len(expected_files)
    assert set(extracted_plaintext_files.keys()) == set(expected_files)


def test_parse_html_plaintext_file(content_config, default_user: KhojUser):
    "Ensure HTML files are parsed correctly"
    # Arrange
    # Setup input-files, input-filters
    config = LocalPlaintextConfig.objects.filter(user=default_user).first()
    extracted_plaintext_files = get_plaintext_files(config=config)

    # Act
    entries = PlaintextToEntries.extract_plaintext_entries(extracted_plaintext_files)

    # Assert
    assert len(entries) == 1
    assert "<div>" not in entries[0].raw


def test_large_plaintext_file_split_into_multiple_entries(tmp_path):
    "Convert files with no heading to jsonl."
    # Arrange
    max_tokens = 256
    normal_entry = " ".join([f"{number}" for number in range(max_tokens - 1)])
    large_entry = " ".join([f"{number}" for number in range(max_tokens)])

    normal_plaintextfile = create_file(tmp_path, normal_entry)
    large_plaintextfile = create_file(tmp_path, large_entry)

    normal_data = {f"{normal_plaintextfile}": normal_entry}
    large_data = {f"{large_plaintextfile}": large_entry}

    # Act
    # Extract Entries from specified plaintext files
    normal_entries = PlaintextToEntries.split_entries_by_max_tokens(
        PlaintextToEntries.extract_plaintext_entries(normal_data),
        max_tokens=max_tokens,
        raw_is_compiled=True,
    )
    large_entries = PlaintextToEntries.split_entries_by_max_tokens(
        PlaintextToEntries.extract_plaintext_entries(large_data), max_tokens=max_tokens, raw_is_compiled=True
    )

    # Assert
    assert len(normal_entries) == 1
    assert len(large_entries) == 2


# Helper Functions
def create_file(tmp_path: Path, entry=None, filename="test.md"):
    file_ = tmp_path / filename
    file_.touch()
    if entry:
        file_.write_text(entry)
    return file_
