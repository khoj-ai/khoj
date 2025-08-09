from pathlib import Path
from textwrap import dedent

from khoj.processor.content.plaintext.plaintext_to_entries import PlaintextToEntries


def test_plaintext_file():
    "Convert files with no heading to jsonl."
    # Arrange
    raw_entry = """
    Hi, I am a plaintext file and I have some plaintext words.
    """
    plaintextfile = "test.txt"
    data = {plaintextfile: raw_entry}

    # Act
    # Extract Entries from specified plaintext files
    entries = PlaintextToEntries.extract_plaintext_entries(data)

    # Convert each entry.file to absolute path to make them JSON serializable
    for entry in entries[1]:
        entry.file = str(Path(entry.file).absolute())

    # Assert
    assert len(entries) == 2
    assert len(entries[1]) == 1
    # Ensure raw entry with no headings do not get heading prefix prepended
    assert not entries[1][0].raw.startswith("#")
    # Ensure compiled entry has filename prepended as top level heading
    assert entries[1][0].compiled == f"{plaintextfile}\n{raw_entry}"


def test_parse_html_plaintext_file(tmp_path):
    "Ensure HTML files are parsed correctly"
    # Arrange
    raw_entry = dedent(
        f"""
        <html>
        <head><title>Test HTML</title></head>
        <body>
        <div>Test content</div>
        </body>
        </html>
        """
    )
    extracted_plaintext_files = {"test.html": raw_entry}

    # Act
    entries = PlaintextToEntries.extract_plaintext_entries(extracted_plaintext_files)

    # Assert
    assert len(entries) == 2
    assert len(entries[1]) == 1
    assert "<div>" not in entries[1][0].raw


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
    normal_entries = PlaintextToEntries.extract_plaintext_entries(normal_data)
    large_entries = PlaintextToEntries.extract_plaintext_entries(large_data)

    # assert
    assert len(normal_entries) == 2
    assert len(large_entries) == 2

    normal_entries = PlaintextToEntries.split_entries_by_max_tokens(
        normal_entries[1],
        max_tokens=max_tokens,
        raw_is_compiled=True,
    )
    large_entries = PlaintextToEntries.split_entries_by_max_tokens(
        large_entries[1], max_tokens=max_tokens, raw_is_compiled=True
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
