from pathlib import Path

app_root_directory = Path(__file__).parent.parent.parent
web_directory = app_root_directory / 'src/interface/web/'
empty_escape_sequences = r'\n|\r\t '
