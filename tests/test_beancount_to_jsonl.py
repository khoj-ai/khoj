# Standard Packages
import json

# Internal Packages
from src.processor.ledger.beancount_to_jsonl import BeancountToJsonl


def test_no_transactions_in_file(tmp_path):
    "Handle file with no transactions."
    # Arrange
    entry = f'''
    - Bullet point 1
    - Bullet point 2
    '''
    beancount_file = create_file(tmp_path, entry)

    # Act
    # Extract Entries from specified Beancount files
    entry_nodes, file_to_entries = BeancountToJsonl.extract_beancount_transactions(beancount_files=[beancount_file])

    # Process Each Entry from All Beancount Files
    jsonl_string = BeancountToJsonl.convert_transaction_maps_to_jsonl(
        BeancountToJsonl.convert_transactions_to_maps(entry_nodes, file_to_entries))
    jsonl_data = [json.loads(json_string) for json_string in jsonl_string.splitlines()]

    # Assert
    assert len(jsonl_data) == 0


def test_single_beancount_transaction_to_jsonl(tmp_path):
    "Convert transaction from single file to jsonl."
    # Arrange
    entry = f'''
1984-04-01 * "Payee" "Narration"
Expenses:Test:Test  1.00 KES
Assets:Test:Test  -1.00 KES
    '''
    beancount_file = create_file(tmp_path, entry)

    # Act
    # Extract Entries from specified Beancount files
    entries, entry_to_file_map = BeancountToJsonl.extract_beancount_transactions(beancount_files=[beancount_file])

    # Process Each Entry from All Beancount Files
    jsonl_string = BeancountToJsonl.convert_transaction_maps_to_jsonl(
        BeancountToJsonl.convert_transactions_to_maps(entries, entry_to_file_map))
    jsonl_data = [json.loads(json_string) for json_string in jsonl_string.splitlines()]

    # Assert
    assert len(jsonl_data) == 1


def test_multiple_transactions_to_jsonl(tmp_path):
    "Convert multiple transactions from single file to jsonl."
    # Arrange
    entry = f'''
1984-04-01 * "Payee" "Narration"
Expenses:Test:Test  1.00 KES
Assets:Test:Test  -1.00 KES
\t\r
1984-04-01 * "Payee" "Narration"
Expenses:Test:Test  1.00 KES
Assets:Test:Test  -1.00 KES
'''

    beancount_file = create_file(tmp_path, entry)

    # Act
    # Extract Entries from specified Beancount files
    entries, entry_to_file_map = BeancountToJsonl.extract_beancount_transactions(beancount_files=[beancount_file])

    # Process Each Entry from All Beancount Files
    jsonl_string = BeancountToJsonl.convert_transaction_maps_to_jsonl(
        BeancountToJsonl.convert_transactions_to_maps(entries, entry_to_file_map))
    jsonl_data = [json.loads(json_string) for json_string in jsonl_string.splitlines()]

    # Assert
    assert len(jsonl_data) == 2


def test_get_beancount_files(tmp_path):
    "Ensure Beancount files specified via input-filter, input-files extracted"
    # Arrange
    # Include via input-filter globs
    group1_file1 = create_file(tmp_path, filename="group1-file1.bean")
    group1_file2 = create_file(tmp_path, filename="group1-file2.bean")
    group2_file1 = create_file(tmp_path, filename="group2-file1.beancount")
    group2_file2 = create_file(tmp_path, filename="group2-file2.beancount")
    # Include via input-file field
    file1 = create_file(tmp_path, filename="ledger.bean")
    # Not included by any filter
    create_file(tmp_path, filename="not-included-ledger.bean")
    create_file(tmp_path, filename="not-included-text.txt")

    expected_files = sorted(map(str, [group1_file1, group1_file2, group2_file1, group2_file2, file1]))

    # Setup input-files, input-filters
    input_files = [tmp_path / 'ledger.bean']
    input_filter = [tmp_path / 'group1*.bean', tmp_path / 'group2*.beancount']

    # Act
    extracted_org_files = BeancountToJsonl.get_beancount_files(input_files, input_filter)

    # Assert
    assert len(extracted_org_files) == 5
    assert extracted_org_files == expected_files


# Helper Functions
def create_file(tmp_path, entry=None, filename="ledger.beancount"):
    beancount_file = tmp_path / filename
    beancount_file.touch()
    if entry:
        beancount_file.write_text(entry)
    return beancount_file
