# Standard Packages
import json

# Internal Packages
from src.processor.ledger.beancount_to_jsonl import extract_beancount_transactions, convert_transactions_to_maps, convert_transaction_maps_to_jsonl


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
    entry_nodes, file_to_entries = extract_beancount_transactions(beancount_files=[beancount_file])

    # Process Each Entry from All Beancount Files
    jsonl_string = convert_transaction_maps_to_jsonl(convert_transactions_to_maps(entry_nodes, file_to_entries))
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
    entries, entry_to_file_map = extract_beancount_transactions(beancount_files=[beancount_file])

    # Process Each Entry from All Beancount Files
    jsonl_string = convert_transaction_maps_to_jsonl(convert_transactions_to_maps(entries, entry_to_file_map))
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
    entries, entry_to_file_map = extract_beancount_transactions(beancount_files=[beancount_file])

    # Process Each Entry from All Beancount Files
    jsonl_string = convert_transaction_maps_to_jsonl(convert_transactions_to_maps(entries, entry_to_file_map))
    jsonl_data = [json.loads(json_string) for json_string in jsonl_string.splitlines()]

    # Assert
    assert len(jsonl_data) == 2


# Helper Functions
def create_file(tmp_path, entry, filename="ledger.beancount"):
    beancount_file = tmp_path / f"notes/{filename}"
    beancount_file.parent.mkdir()
    beancount_file.touch()
    beancount_file.write_text(entry)
    return beancount_file
