import datetime

from khoj.processor.content.org_mode import orgnode


# Test
# ----------------------------------------------------------------------------------------------------
def test_parse_entry_with_no_headings(tmp_path):
    "Test parsing of entry with minimal fields"
    # Arrange
    entry = """Body Line 1"""
    orgfile = create_file(tmp_path, entry)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert len(entries) == 1
    assert entries[0].heading == f"{orgfile}"
    assert entries[0].tags == list()
    assert entries[0].body == "Body Line 1"
    assert entries[0].priority == ""
    assert entries[0].Property("ID") == ""
    assert entries[0].closed == ""
    assert entries[0].scheduled == ""
    assert entries[0].deadline == ""


# ----------------------------------------------------------------------------------------------------
def test_parse_minimal_entry(tmp_path):
    "Test parsing of entry with minimal fields"
    # Arrange
    entry = """
* Heading
Body Line 1"""
    orgfile = create_file(tmp_path, entry)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert len(entries) == 1
    assert entries[0].heading == "Heading"
    assert entries[0].tags == list()
    assert entries[0].body == "Body Line 1\n\n"
    assert entries[0].priority == ""
    assert entries[0].Property("ID") == ""
    assert entries[0].closed == ""
    assert entries[0].scheduled == ""
    assert entries[0].deadline == ""


# ----------------------------------------------------------------------------------------------------
def test_parse_complete_entry(tmp_path):
    "Test parsing of entry with all important fields"
    # Arrange
    entry = """
*** DONE [#A] Heading   :Tag1:TAG2:tag3:
CLOSED: [1984-04-01 Sun 12:00] SCHEDULED: <1984-04-01 Sun 09:00> DEADLINE: <1984-04-01 Sun>
:PROPERTIES:
:ID: 123-456-789-4234-1231
:END:
:LOGBOOK:
CLOCK: [1984-04-01 Sun 09:00]--[1984-04-01 Sun 12:00] => 3:00
- Clocked Log 1
:END:
Body Line 1
Body Line 2"""
    orgfile = create_file(tmp_path, entry)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert len(entries) == 1
    assert entries[0].heading == "Heading"
    assert entries[0].todo == "DONE"
    assert entries[0].tags == ["Tag1", "TAG2", "tag3"]
    assert entries[0].body == "- Clocked Log 1\n\nBody Line 1\n\nBody Line 2\n\n"
    assert entries[0].priority == "A"
    assert entries[0].Property("ID") == "id:123-456-789-4234-1231"
    assert entries[0].closed == datetime.date(1984, 4, 1)
    assert entries[0].scheduled == datetime.date(1984, 4, 1)
    assert entries[0].deadline == datetime.date(1984, 4, 1)
    assert entries[0].logbook == [(datetime.datetime(1984, 4, 1, 9, 0, 0), datetime.datetime(1984, 4, 1, 12, 0, 0))]


# ----------------------------------------------------------------------------------------------------
def test_render_entry_with_property_drawer_and_empty_body(tmp_path):
    "Render heading entry with property drawer"
    # Arrange
    entry_to_render = """
*** [#A] Heading1   :tag1:
    :PROPERTIES:
    :ID: 111-111-111-1111-1111
    :END:
\t\r  \n
"""
    orgfile = create_file(tmp_path, entry_to_render)

    expected_entry = f"""*** [#A] Heading1                                            :tag1:
:PROPERTIES:
:LINE: file://{orgfile}#line=2
:ID: id:111-111-111-1111-1111
:END:
"""

    # Act
    parsed_entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert f"{parsed_entries[0]}" == expected_entry


# ----------------------------------------------------------------------------------------------------
def test_all_links_to_entry_rendered(tmp_path):
    "Ensure all links to entry rendered in property drawer from entry"
    # Arrange
    entry = """
*** [#A] Heading   :tag1:
:PROPERTIES:
:ID: 123-456-789-4234-1231
:END:
Body Line 1
*** Heading2
Body Line 2
"""
    orgfile = create_file(tmp_path, entry)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    # SOURCE link rendered with Heading
    # ID link rendered with ID
    assert ":ID: id:123-456-789-4234-1231" in f"{entries[0]}"
    # LINE link rendered with line number
    assert f":LINE: file://{orgfile}#line=2" in f"{entries[0]}"
    # LINE link rendered with line number
    assert f":LINE: file://{orgfile}#line=7" in f"{entries[1]}"


# ----------------------------------------------------------------------------------------------------
def test_parse_multiple_entries(tmp_path):
    "Test parsing of multiple entries"
    # Arrange
    content = """
*** FAILED [#A] Heading1   :tag1:
CLOSED: [1984-04-01 Sun 12:00] SCHEDULED: <1984-04-01 Sun 09:00> DEADLINE: <1984-04-01 Sun>
:PROPERTIES:
:ID: 123-456-789-4234-0001
:END:
:LOGBOOK:
CLOCK: [1984-04-01 Sun 09:00]--[1984-04-01 Sun 12:00] => 3:00
- Clocked Log 1
:END:
Body 1

*** CANCELLED [#A] Heading2   :tag2:
CLOSED: [1984-04-02 Sun 12:00] SCHEDULED: <1984-04-02 Sun 09:00> DEADLINE: <1984-04-02 Sun>
:PROPERTIES:
:ID: 123-456-789-4234-0002
:END:
:LOGBOOK:
CLOCK: [1984-04-02 Mon 09:00]--[1984-04-02 Mon 12:00] => 3:00
- Clocked Log 2
:END:
Body 2

"""
    orgfile = create_file(tmp_path, content)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert len(entries) == 2
    for index, entry in enumerate(entries):
        assert entry.heading == f"Heading{index + 1}"
        assert entry.todo == "FAILED" if index == 0 else "CANCELLED"
        assert entry.tags == [f"tag{index + 1}"]
        assert entry.body == f"- Clocked Log {index + 1}\n\nBody {index + 1}\n\n"
        assert entry.priority == "A"
        assert entry.Property("ID") == f"id:123-456-789-4234-000{index + 1}"
        assert entry.closed == datetime.date(1984, 4, index + 1)
        assert entry.scheduled == datetime.date(1984, 4, index + 1)
        assert entry.deadline == datetime.date(1984, 4, index + 1)
        assert entry.logbook == [
            (datetime.datetime(1984, 4, index + 1, 9, 0, 0), datetime.datetime(1984, 4, index + 1, 12, 0, 0))
        ]


# ----------------------------------------------------------------------------------------------------
def test_parse_entry_with_empty_title(tmp_path):
    "Test parsing of entry with minimal fields"
    # Arrange
    entry = """#+TITLE:
Body Line 1"""
    orgfile = create_file(tmp_path, entry)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert len(entries) == 1
    assert entries[0].heading == f"{orgfile}"
    assert entries[0].tags == list()
    assert entries[0].body == "Body Line 1"
    assert entries[0].priority == ""
    assert entries[0].Property("ID") == ""
    assert entries[0].closed == ""
    assert entries[0].scheduled == ""
    assert entries[0].deadline == ""


# ----------------------------------------------------------------------------------------------------
def test_parse_entry_with_title_and_no_headings(tmp_path):
    "Test parsing of entry with minimal fields"
    # Arrange
    entry = """#+TITLE: test
Body Line 1"""
    orgfile = create_file(tmp_path, entry)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert len(entries) == 1
    assert entries[0].heading == "test"
    assert entries[0].tags == list()
    assert entries[0].body == "Body Line 1"
    assert entries[0].priority == ""
    assert entries[0].Property("ID") == ""
    assert entries[0].closed == ""
    assert entries[0].scheduled == ""
    assert entries[0].deadline == ""
    assert entries[0].ancestors == ["test"]


# ----------------------------------------------------------------------------------------------------
def test_parse_entry_with_multiple_titles_and_no_headings(tmp_path):
    "Test parsing of entry with minimal fields"
    # Arrange
    entry = """#+TITLE: title1
Body Line 1
#+TITLE:  title2 """
    orgfile = create_file(tmp_path, entry)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert len(entries) == 1
    assert entries[0].heading == "title1 title2"
    assert entries[0].tags == list()
    assert entries[0].body == "Body Line 1\n"
    assert entries[0].priority == ""
    assert entries[0].Property("ID") == ""
    assert entries[0].closed == ""
    assert entries[0].scheduled == ""
    assert entries[0].deadline == ""
    assert entries[0].ancestors == ["title1 title2"]


# ----------------------------------------------------------------------------------------------------
def test_parse_org_with_intro_text_before_heading(tmp_path):
    "Test parsing of org file with intro text before heading"
    # Arrange
    body = """#+TITLE: Title
intro body
* Entry Heading
entry body
"""
    orgfile = create_file(tmp_path, body)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert len(entries) == 2
    assert entries[0].heading == "Title"
    assert entries[0].body == "intro body\n"
    assert entries[0].ancestors == ["Title"]
    assert entries[1].heading == "Entry Heading"
    assert entries[1].body == "entry body\n\n"
    assert entries[1].ancestors == ["Title"]


# ----------------------------------------------------------------------------------------------------
def test_parse_org_with_intro_text_multiple_titles_and_heading(tmp_path):
    "Test parsing of org file with intro text, multiple titles and heading entry"
    # Arrange
    body = """#+TITLE: Title1
intro body
* Entry Heading
entry body
#+TITLE: Title2 """
    orgfile = create_file(tmp_path, body)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert len(entries) == 2
    assert entries[0].heading == "Title1 Title2"
    assert entries[0].body == "intro body\n"
    assert entries[0].ancestors == ["Title1 Title2"]
    assert entries[1].heading == "Entry Heading"
    assert entries[1].body == "entry body\n\n"
    assert entries[0].ancestors == ["Title1 Title2"]


# ----------------------------------------------------------------------------------------------------
def test_parse_org_with_single_ancestor_heading(tmp_path):
    "Parse org entries with parent headings context"
    # Arrange
    body = """
* Heading 1
body 1
** Sub Heading 1
"""
    orgfile = create_file(tmp_path, body)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert len(entries) == 2
    assert entries[0].heading == "Heading 1"
    assert entries[0].ancestors == [f"{orgfile}"]
    assert entries[1].heading == "Sub Heading 1"
    assert entries[1].ancestors == [f"{orgfile}", "Heading 1"]


# ----------------------------------------------------------------------------------------------------
def test_parse_org_with_multiple_ancestor_headings(tmp_path):
    "Parse org entries with parent headings context"
    # Arrange
    body = """
* Heading 1
body 1
** Sub Heading 1
*** Sub Sub Heading 1
sub sub body 1
"""
    orgfile = create_file(tmp_path, body)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert len(entries) == 3
    assert entries[0].heading == "Heading 1"
    assert entries[0].ancestors == [f"{orgfile}"]
    assert entries[1].heading == "Sub Heading 1"
    assert entries[1].ancestors == [f"{orgfile}", "Heading 1"]
    assert entries[2].heading == "Sub Sub Heading 1"
    assert entries[2].ancestors == [f"{orgfile}", "Heading 1", "Sub Heading 1"]


# ----------------------------------------------------------------------------------------------------
def test_parse_org_with_multiple_ancestor_headings_of_siblings(tmp_path):
    "Parse org entries with parent headings context"
    # Arrange
    body = """
* Heading 1
body 1
** Sub Heading 1
*** Sub Sub Heading 1
sub sub body 1
*** Sub Sub Heading 2
** Sub Heading 2
*** Sub Sub Heading 3
"""
    orgfile = create_file(tmp_path, body)

    # Act
    entries = orgnode.makelist_with_filepath(orgfile)

    # Assert
    assert len(entries) == 6
    assert entries[0].heading == "Heading 1"
    assert entries[0].ancestors == [f"{orgfile}"]
    assert entries[1].heading == "Sub Heading 1"
    assert entries[1].ancestors == [f"{orgfile}", "Heading 1"]
    assert entries[2].heading == "Sub Sub Heading 1"
    assert entries[2].ancestors == [f"{orgfile}", "Heading 1", "Sub Heading 1"]
    assert entries[3].heading == "Sub Sub Heading 2"
    assert entries[3].ancestors == [f"{orgfile}", "Heading 1", "Sub Heading 1"]
    assert entries[4].heading == "Sub Heading 2"
    assert entries[4].ancestors == [f"{orgfile}", "Heading 1"]
    assert entries[5].heading == "Sub Sub Heading 3"
    assert entries[5].ancestors == [f"{orgfile}", "Heading 1", "Sub Heading 2"]


# Helper Functions
def create_file(tmp_path, entry, filename="test.org"):
    org_file = tmp_path / f"notes/{filename}"
    org_file.parent.mkdir()
    org_file.touch()
    org_file.write_text(entry)
    return org_file
