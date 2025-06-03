# Initialize the development environment for the project
# ---
PROJECT_ROOT=$(git rev-parse --show-toplevel)

# Default to minimal installation unless --full flag passed
INSTALL_FULL=false
DEVCONTAINER=false
for arg in "$@"
do
    if [ "$arg" == "--full" ]
    then
        INSTALL_FULL=true
    fi
    if [ "$arg" == "--devcontainer" ]
    then
        DEVCONTAINER=true
    fi
done

if [ "$DEVCONTAINER" = true ]; then
    echo "Dev container setup - using pre-installed dependencies..."
    cd "$PROJECT_ROOT"

    # Use devcontainer launch.json
    mkdir -p .vscode && cp .devcontainer/launch.json .vscode/launch.json

    # Activate the pre-installed venv (no need to create new one)
    echo "Using Python environment at /opt/venv"
    # PATH should already include /opt/venv/bin from Dockerfile

    # Install khoj in editable mode (dependencies already installed)
    python3 -m pip install -e '.[dev]'

    # Install Web App using cached dependencies
    echo "Installing Web App using cached dependencies..."
    cd "$PROJECT_ROOT/src/interface/web"
    yarn install --cache-folder /opt/yarn-cache && yarn export
else
    # Standard setup
    echo "Installing Server App..."
    cd "$PROJECT_ROOT"
    python3 -m venv .venv && . .venv/bin/activate && python3 -m pip install -e '.[dev]'

    echo "Installing Web App..."
    cd "$PROJECT_ROOT/src/interface/web"
    yarn install && yarn export
fi

# Install Obsidian App
# ---
if [ "$INSTALL_FULL" = true ] ; then
    echo "Installing Obsidian App..."
    cd $PROJECT_ROOT/src/interface/obsidian
    yarn install
fi

# Install Desktop App
# ---
if [ "$INSTALL_FULL" = true ] ; then
    echo "Installing Desktop App..."
    cd $PROJECT_ROOT/src/interface/desktop
    yarn install
fi

# Install pre-commit hooks
# ----
echo "Installing pre-commit hooks..."

# Setup pre-commit hooks using the pre-commit package
pre-commit install -t pre-push -t pre-commit

# Run Prettier on web app
cat << 'EOF' > temp_pre_commit
# Run Prettier for Web App
# -------------------------

# Function to check if color output is possible
can_use_color() {
    if [ -t 1 ] && command -v tput >/dev/null 2>&1 && tput colors >/dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

# Function to print colored text if possible
print_color() {
    if can_use_color; then
        tput setab "$1"
        printf "%s" "$2"
        tput sgr0
    else
        printf "%s" "$2"
    fi
}

print_status() {
    local status="$1"
    local color="$2"
    printf "prettier%-64s" "..."
    print_color "$color" "$status"
    printf "\n"
}

PROJECT_ROOT=$(git rev-parse --show-toplevel)
# Get the list of staged files
FILES=$(git diff --cached --name-only --diff-filter=ACMR | grep '^src/interface/web/' | sed 's| |\\ |g')
if [ -z "$FILES" ]; then
    if [ -t 1 ]; then
        print_status "Skipped" 6
    else
        echo "prettier.....................................................Skipped"
    fi
else
    # Run prettier on staged files
    echo "$FILES" | xargs $PROJECT_ROOT/src/interface/web/node_modules/.bin/prettier --ignore-unknown --write

    # Check if any files were modified by prettier
    MODIFIED=$(git diff --name-only -- $FILES)
    if [ -n "$MODIFIED" ]; then
        if [ -t 1 ]; then
            print_status "Modified" 1
        else
            echo "prettier.....................................................Modified"
        fi
        exit 1
    fi

    # Add back the modified/prettified files to staging
    # echo "$FILES" | xargs git add

    # Show the user if changes were made
    if [ -t 1 ]; then
        print_status "Passed" 2
    else
        echo "prettier.....................................................Passed"
    fi
fi
EOF

# Prepend the new content to the existing pre-commit file
cat temp_pre_commit "$(git rev-parse --git-dir)/hooks/pre-commit" > temp_combined_pre_commit

# Replace the old pre-commit file with the new combined one
mv temp_combined_pre_commit "$(git rev-parse --git-dir)/hooks/pre-commit"

# Clean up
# ---

# Remove the temporary pre-commit file
rm temp_pre_commit

# Make sure the pre-commit hook is executable
chmod +x "$(git rev-parse --git-dir)/hooks/pre-commit"
