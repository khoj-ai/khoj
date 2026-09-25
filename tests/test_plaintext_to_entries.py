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


def test_html_inline_elements_stay_in_their_line():
    "Ensure links, emphasis and other inline tags do not split the sentence around them"
    # Arrange
    markup = dedent(
        """
        <html><body>
        <h1>Setup</h1>
        <p>Read the <a href="/docs">setup guide</a> before you start. It is un<b>believ</b>able.</p>
        <p>Do <em>not</em> delete the <code>data</code> folder.</p>
        <p>Wrapped
           source line</p>
        <ul><li>Alpha <b>one</b></li><li>Beta</li></ul>
        <table><tr><td>Apple <i>red</i></td><td>3</td></tr></table>
        <p>Line one<br>Line two</p>
        <div><button>Save</button><button>Cancel</button></div>
        </body></html>
        """
    )

    # Act
    text = PlaintextToEntries.extract_html_content(markup, "html")

    # Assert
    assert text.split("\n") == [
        "Setup",
        "Read the setup guide before you start. It is unbelievable.",
        "Do not delete the data folder.",
        "Wrapped source line",
        "Alpha one",
        "Beta",
        "Apple red",
        "3",
        "Line one",
        "Line two",
        "Save Cancel",
    ]


def test_html_preformatted_text_keeps_its_layout():
    "Ensure whitespace inside pre blocks is kept, since it is content there"
    # Arrange
    markup = "<p>Example:</p><pre>def f():\n    return 1</pre><p>Done.</p>"

    # Act
    text = PlaintextToEntries.extract_html_content(markup, "html")

    # Assert
    assert text == "Example:\ndef f():\n    return 1\nDone."


def test_html_non_text_content_is_skipped():
    "Ensure script, style and comment content does not reach the indexed text"
    # Arrange
    markup = "<p>Visible</p><script>var hidden = 1;</script><style>p { color: red; }</style><!-- note -->"

    # Act
    text = PlaintextToEntries.extract_html_content(markup, "html")

    # Assert
    assert text == "Visible"


def test_deeply_nested_html_is_extracted():
    "Ensure deeply nested markup does not exhaust the recursion limit"
    # Arrange
    markup = "<div>" * 5000 + "deep" + "</div>" * 5000

    # Act
    text = PlaintextToEntries.extract_html_content(markup, "html")

    # Assert
    assert text == "deep"


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
