# Internal Packages
from khoj.processor.jsonl.jsonl_to_jsonl import JsonlToJsonl
from khoj.utils.rawconfig import Entry


def test_process_entries_from_single_input_jsonl(tmp_path):
    "Convert multiple jsonl entries from single file to entries."
    # Arrange
    input_jsonl = """{"raw": "raw input data 1", "compiled": "compiled input data 1", "heading": null, "file": "source/file/path1"}
{"raw": "raw input data 2", "compiled": "compiled input data 2", "heading": null, "file": "source/file/path2"}
"""
    input_jsonl_file = create_file(tmp_path, input_jsonl)

    # Act
    # Process Each Entry from All Notes Files
    input_jsons = JsonlToJsonl.extract_jsonl_entries([input_jsonl_file])
    entries = list(map(Entry.from_dict, input_jsons))
    output_jsonl = JsonlToJsonl.convert_entries_to_jsonl(entries)

    # Assert
    assert len(entries) == 2
    assert output_jsonl == input_jsonl


def test_process_entries_from_multiple_input_jsonls(tmp_path):
    "Convert multiple jsonl entries from single file to entries."
    # Arrange
    input_jsonl_1 = """{"raw": "raw input data 1", "compiled": "compiled input data 1", "heading": null, "file": "source/file/path1"}"""
    input_jsonl_2 = """{"raw": "raw input data 2", "compiled": "compiled input data 2", "heading": null, "file": "source/file/path2"}"""
    input_jsonl_file_1 = create_file(tmp_path, input_jsonl_1, filename="input1.jsonl")
    input_jsonl_file_2 = create_file(tmp_path, input_jsonl_2, filename="input2.jsonl")

    # Act
    # Process Each Entry from All Notes Files
    input_jsons = JsonlToJsonl.extract_jsonl_entries([input_jsonl_file_1, input_jsonl_file_2])
    entries = list(map(Entry.from_dict, input_jsons))
    output_jsonl = JsonlToJsonl.convert_entries_to_jsonl(entries)

    # Assert
    assert len(entries) == 2
    assert output_jsonl == f"{input_jsonl_1}\n{input_jsonl_2}\n"


def test_get_jsonl_files(tmp_path):
    "Ensure JSONL files specified via input-filter, input-files extracted"
    # Arrange
    # Include via input-filter globs
    group1_file1 = create_file(tmp_path, filename="group1-file1.jsonl")
    group1_file2 = create_file(tmp_path, filename="group1-file2.jsonl")
    group2_file1 = create_file(tmp_path, filename="group2-file1.jsonl")
    group2_file2 = create_file(tmp_path, filename="group2-file2.jsonl")
    # Include via input-file field
    file1 = create_file(tmp_path, filename="notes.jsonl")
    # Not included by any filter
    create_file(tmp_path, filename="not-included-jsonl.jsonl")
    create_file(tmp_path, filename="not-included-text.txt")

    expected_files = sorted(map(str, [group1_file1, group1_file2, group2_file1, group2_file2, file1]))

    # Setup input-files, input-filters
    input_files = [tmp_path / "notes.jsonl"]
    input_filter = [tmp_path / "group1*.jsonl", tmp_path / "group2*.jsonl"]

    # Act
    extracted_org_files = JsonlToJsonl.get_jsonl_files(input_files, input_filter)

    # Assert
    assert len(extracted_org_files) == 5
    assert extracted_org_files == expected_files


# Helper Functions
def create_file(tmp_path, entry=None, filename="test.jsonl"):
    jsonl_file = tmp_path / filename
    jsonl_file.touch()
    if entry:
        jsonl_file.write_text(entry)
    return jsonl_file
