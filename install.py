#!/usr/bin/env python3
import pathlib
import argparse

if __name__ == '__main__':
    # Setup Argument Parser
    parser = argparse.ArgumentParser(description="Setup the semantic search program")
    parser.add_argument('--script-dir', '-s', default="./", type=pathlib.Path, help="The project directory. Default: Current Directory")
    parser.add_argument('--install-path', '-i', default="/usr/bin/semantic-search", type=pathlib.Path, help="The directory to install the script. Default: ./embeddings.pt")
    parser.add_argument('--model-dir', '-m', default="./", type=pathlib.Path, help="The directory to store the model in. Default: Current Directory")
    args = parser.parse_args()

    run_script_content = f'''#!/bin/bash

# Arrange
eval "$(conda shell.bash hook)"
conda activate samvayati
cd {args.script_dir.expanduser().absolute()}

# Act
python3 asymmetric.py -j {args.model_dir.expanduser().absolute()}/notes.jsonl.gz -e {args.model_dir.expanduser().absolute()}/notes_embeddings.pt -n 5 --interactive
'''

    with args.install_path.open(mode='w') as run_script:
        run_script.write(run_script_content)
