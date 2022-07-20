# Standard Packages
import datetime
from os.path import relpath
from pathlib import Path

# Internal Packages
from src.processor.org_mode import orgnode


# Test
# ----------------------------------------------------------------------------------------------------
def test_parse_minimal_entry(tmp_path):
    "Test parsing of entry with minimal fields"
    # Arrange
    entry = f'''
* Heading
Body Line 1'''
    orgfile = create_file(tmp_path, entry)

    # Act
    entries = orgnode.makelist(orgfile)

    # Assert
    assert len(entries) == 1
    assert entries[0].Heading() == "Heading"
    assert entries[0].Tags() == set()
    assert entries[0].Body() == "Body Line 1"
    assert entries[0].Priority() == ""
    assert entries[0].Property("ID") == ""
    assert entries[0].Closed() == ""
    assert entries[0].Scheduled() == ""
    assert entries[0].Deadline() == ""


# ----------------------------------------------------------------------------------------------------
def test_parse_complete_entry(tmp_path):
    "Test parsing of entry with all important fields"
    # Arrange
    entry = f'''
*** [#A] Heading   :Tag1:TAG2:tag3:
CLOSED: [1984-04-01 Sun 12:00] SCHEDULED: <1984-04-01 Sun 09:00> DEADLINE: <1984-04-01 Sun>
:PROPERTIES:
:ID: 123-456-789-4234-1231
:END:
:LOGBOOK:
CLOCK: [1984-04-01 Sun 09:00]--[1984-04-01 Sun 12:00] => 3:00
- Clocked Log 1
:END:
Body Line 1
Body Line 2'''
    orgfile = create_file(tmp_path, entry)

    # Act
    entries = orgnode.makelist(orgfile)

    # Assert
    assert len(entries) == 1
    assert entries[0].Heading() == "Heading"
    assert entries[0].Tags() == {"Tag1", "TAG2", "tag3"}
    assert entries[0].Body() == "- Clocked Log 1\nBody Line 1\nBody Line 2"
    assert entries[0].Priority() == "A"
    assert entries[0].Property("ID") == "id:123-456-789-4234-1231"
    assert entries[0].Closed() == datetime.date(1984,4,1)
    assert entries[0].Scheduled() == datetime.date(1984,4,1)
    assert entries[0].Deadline() == datetime.date(1984,4,1)
    assert entries[0].Logbook() == [(datetime.datetime(1984,4,1,9,0,0), datetime.datetime(1984,4,1,12,0,0))]


# ----------------------------------------------------------------------------------------------------
def test_all_links_to_entry_rendered(tmp_path):
    "Ensure all links to entry rendered in property drawer from entry"
    # Arrange
    entry = f'''
*** [#A] Heading   :tag1:
:PROPERTIES:
:ID: 123-456-789-4234-1231
:END:
Body Line 1
*** Heading2
Body Line 2
'''
    orgfile = create_file(tmp_path, entry)
    normalized_orgfile = f'~/{relpath(orgfile, start=Path.home())}'

    # Act
    entries = orgnode.makelist(orgfile)

    # Assert
    # SOURCE link rendered with Heading
    assert f':SOURCE: [[file:{normalized_orgfile}::*{entries[0].Heading()}]]' in f'{entries[0]}'
    # ID link rendered with ID
    assert f':ID: id:123-456-789-4234-1231' in f'{entries[0]}'
    # LINE link rendered with line number
    assert f':LINE: file:{normalized_orgfile}::2' in f'{entries[0]}'


# ----------------------------------------------------------------------------------------------------
def test_source_link_to_entry_escaped_for_rendering(tmp_path):
    "Test SOURCE link renders with square brackets in filename, heading escaped for org-mode rendering"
    # Arrange
    entry = f'''
*** [#A] Heading[1]   :tag1:
:PROPERTIES:
:ID: 123-456-789-4234-1231
:END:
Body Line 1'''
    orgfile = create_file(tmp_path, entry, filename="test[1].org")

    # Act
    entries = orgnode.makelist(orgfile)

    # Assert
    assert len(entries) == 1
    # parsed heading from entry
    assert entries[0].Heading() == "Heading[1]"
    # ensure SOURCE link has square brackets in filename, heading escaped in rendered entries
    normalized_orgfile = f'~/{relpath(orgfile, start=Path.home())}'
    escaped_orgfile = f'{normalized_orgfile}'.replace("[1]", "\\[1\\]")
    assert f':SOURCE: [[file:{escaped_orgfile}::*Heading\[1\]' in f'{entries[0]}'


# ----------------------------------------------------------------------------------------------------
def test_parse_multiple_entries(tmp_path):
    "Test parsing of multiple entries"
    # Arrange
    content = f'''
*** [#A] Heading1   :tag1:
CLOSED: [1984-04-01 Sun 12:00] SCHEDULED: <1984-04-01 Sun 09:00> DEADLINE: <1984-04-01 Sun>
:PROPERTIES:
:ID: 123-456-789-4234-0001
:END:
:LOGBOOK:
CLOCK: [1984-04-01 Sun 09:00]--[1984-04-01 Sun 12:00] => 3:00
- Clocked Log 1
:END:
Body 1

*** [#A] Heading2   :tag2:
CLOSED: [1984-04-02 Sun 12:00] SCHEDULED: <1984-04-02 Sun 09:00> DEADLINE: <1984-04-02 Sun>
:PROPERTIES:
:ID: 123-456-789-4234-0002
:END:
:LOGBOOK:
CLOCK: [1984-04-02 Mon 09:00]--[1984-04-02 Mon 12:00] => 3:00
- Clocked Log 2
:END:
Body 2

'''
    orgfile = create_file(tmp_path, content)

    # Act
    entries = orgnode.makelist(orgfile)

    # Assert
    assert len(entries) == 2
    for index, entry in enumerate(entries):
        assert entry.Heading() == f"Heading{index+1}"
        assert entry.Tags() == {f"tag{index+1}"}
        assert entry.Body() == f"- Clocked Log {index+1}\nBody {index+1}\n\n"
        assert entry.Priority() == "A"
        assert entry.Property("ID") == f"id:123-456-789-4234-000{index+1}"
        assert entry.Closed() == datetime.date(1984,4,index+1)
        assert entry.Scheduled() == datetime.date(1984,4,index+1)
        assert entry.Deadline() == datetime.date(1984,4,index+1)
        assert entry.Logbook() == [(datetime.datetime(1984,4,index+1,9,0,0), datetime.datetime(1984,4,index+1,12,0,0))]


# Helper Functions
def create_file(tmp_path, entry, filename="test.org"):
    org_file = tmp_path / f"notes/{filename}"
    org_file.parent.mkdir()
    org_file.touch()
    org_file.write_text(entry)
    return org_file