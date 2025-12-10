# Khoj - AI Assistant Development Guide

This document provides a comprehensive guide for AI assistants working with the Khoj codebase. Khoj is a personal AI application that serves as "Your AI second brain," enabling users to extend their capabilities through AI-powered chat, search, and knowledge management.

## Table of Contents

1. [Project Overview](#project-overview)
2. [Repository Structure](#repository-structure)
3. [Technology Stack](#technology-stack)
4. [Architecture](#architecture)
5. [Development Setup](#development-setup)
6. [Key Conventions](#key-conventions)
7. [Testing](#testing)
8. [Common Development Workflows](#common-development-workflows)
9. [Important Files and Modules](#important-files-and-modules)
10. [Deployment](#deployment)

---

## Project Overview

**Khoj** is a personal AI app that smoothly scales from an on-device personal AI to a cloud-scale enterprise AI.

### Key Features
- Chat with local or online LLMs (llama3, qwen, gemma, mistral, GPT, Claude, Gemini, Deepseek)
- Get answers from the internet and personal documents (images, PDFs, Markdown, Org-mode, Word, Notion files)
- Multi-platform access: Browser, Obsidian, Emacs, Desktop, Phone, WhatsApp
- Create custom agents with knowledge, persona, chat models, and tools
- Automated research with personal newsletters and smart notifications
- Advanced semantic search across documents
- Generate images, text-to-speech, and voice input
- Open-source and self-hostable

### License
AGPL-3.0-or-later

---

## Repository Structure

```
khoj/
├── src/
│   ├── khoj/                      # Main Python application
│   │   ├── app/                   # Django application configuration
│   │   ├── database/              # Database models, migrations, adapters
│   │   │   ├── models/            # Django models (all in __init__.py)
│   │   │   ├── migrations/        # Django migrations
│   │   │   └── adapters/          # Database access layer (all in __init__.py)
│   │   ├── interface/             # UI components
│   │   │   ├── web/               # Web interface templates
│   │   │   └── email/             # Email templates
│   │   ├── processor/             # Data processing modules
│   │   │   ├── content/           # Content processors (PDF, markdown, etc.)
│   │   │   ├── conversation/      # Chat/conversation logic
│   │   │   ├── image/             # Image generation
│   │   │   ├── speech/            # Speech-to-text, text-to-speech
│   │   │   ├── tools/             # AI tool implementations
│   │   │   └── operator/          # Computer automation tools
│   │   ├── routers/               # FastAPI route handlers
│   │   ├── search_filter/         # Search filtering logic
│   │   ├── search_type/           # Search implementations
│   │   └── utils/                 # Utility modules
│   ├── interface/                 # Client applications
│   │   ├── web/                   # Next.js web application
│   │   ├── desktop/               # Electron desktop app
│   │   ├── obsidian/              # Obsidian plugin
│   │   └── emacs/                 # Emacs package
│   └── telemetry/                 # Telemetry server
├── tests/                         # Test suite
│   ├── conftest.py                # Pytest configuration and fixtures
│   ├── data/                      # Test data files
│   └── evals/                     # Quality evaluation tests
├── documentation/                 # Docusaurus documentation site
├── scripts/                       # Development and deployment scripts
├── .github/workflows/             # CI/CD workflows
├── pyproject.toml                 # Python project configuration
├── docker-compose.yml             # Docker services configuration
└── versions.json                  # Version tracking
```

### Key Directories Explained

- **`src/khoj/routers/`**: FastAPI endpoint handlers for API routes (chat, search, auth, content, etc.)
- **`src/khoj/database/`**: Django ORM models and database adapters. All models are in `models/__init__.py`
- **`src/khoj/processor/`**: Business logic for processing different content types and AI operations
- **`src/interface/web/`**: Next.js 14 web application with React components
- **`tests/`**: Pytest-based test suite with Django integration

---

## Technology Stack

### Backend
- **Python**: 3.10, 3.11, 3.12
- **Framework**: FastAPI + Django (hybrid architecture)
- **Database**: PostgreSQL with pgvector for embeddings
- **ORM**: Django ORM
- **ASGI Server**: Uvicorn
- **Task Scheduling**: APScheduler, schedule
- **AI/ML Libraries**:
  - `sentence-transformers` (3.4.1) - Embeddings
  - `torch` (2.6.0) - Deep learning
  - `transformers` (>=4.51.0) - Model inference
  - `openai`, `anthropic`, `google-genai` - LLM APIs
  - `langchain-community`, `langchain-text-splitters` - RAG utilities
  - `openai-whisper` - Speech-to-text

### Frontend
- **Framework**: Next.js 14.2.32
- **Language**: TypeScript
- **UI Libraries**:
  - React 18.3.1
  - Radix UI components
  - Tailwind CSS
  - Framer Motion
- **State Management**: SWR for data fetching
- **Build Tool**: Bun (preferred) or npm
- **Markdown**: markdown-it with syntax highlighting
- **Diagrams**: Mermaid, Excalidraw

### Desktop Clients
- **Obsidian Plugin**: TypeScript, yarn
- **Emacs Package**: Emacs Lisp
- **Desktop App**: Electron (in `src/interface/desktop/`)

### Infrastructure
- **Containerization**: Docker, Docker Compose
- **Code Sandbox**: Terrarium (self-hosted) or E2B
- **Web Search**: SearxNG
- **Package Management**: uv (for Python dependencies)
- **CI/CD**: GitHub Actions

---

## Architecture

### Hybrid FastAPI + Django Architecture

Khoj uses a unique hybrid architecture combining FastAPI and Django:

1. **FastAPI** handles API routes, WebSocket connections, and async operations
2. **Django** manages database models, migrations, admin interface, and authentication
3. **Integration**: Django is initialized before FastAPI, and Django's ASGI app is mounted for admin routes

#### Application Flow

```
main.py
  ├── Initialize Django (setup, migrate, collectstatic)
  ├── Create FastAPI app
  ├── Add CORS middleware
  ├── Import and configure routes (from configure.py)
  ├── Initialize server (background tasks, schedulers)
  └── Run with Uvicorn
```

### Key Architectural Patterns

1. **Adapter Pattern**: Database access is abstracted through adapter classes in `database/adapters/`
2. **Router Pattern**: API endpoints organized by domain (chat, content, agents, etc.) in `routers/`
3. **Processor Pattern**: Content processing logic separated into `processor/` modules
4. **Middleware**: Authentication, CORS, sessions handled via Starlette middleware

### Data Flow

```
User Request
  → FastAPI Router (routers/)
  → Database Adapter (database/adapters/)
  → Django Model (database/models/)
  → Processor (processor/)
  → LLM API / Search / Tool
  → Response
```

---

## Development Setup

### Prerequisites

- **Python**: 3.10, 3.11, or 3.12
- **PostgreSQL**: With pgvector extension
- **Bun**: For frontend development (or npm/yarn)
- **uv**: Python package installer (recommended)
- **Git**: Version control

### Quick Start (Local)

```bash
# Clone repository
git clone https://github.com/khoj-ai/khoj && cd khoj

# Create and activate virtual environment
python3 -m venv .venv
source .venv/bin/activate  # On Windows: .venv\Scripts\activate

# Install dependencies (using uv)
uv sync --all-extras

# Create PostgreSQL database
createdb khoj -U postgres --password

# Build frontend assets
cd src/interface/web/
bun install
bun export
cd ../../..

# Run Khoj
khoj -vv
```

### Quick Start (Docker)

```bash
# Clone repository
git clone https://github.com/khoj-ai/khoj && cd khoj

# Update docker-compose.yml environment variables
# Comment out 'image' line and uncomment 'build' line for local development

# Start services
docker-compose up -d

# Rebuild after code changes
docker-compose build --no-cache
```

### Environment Variables

Key environment variables for local development:

```bash
POSTGRES_HOST=localhost
POSTGRES_PORT=5432
POSTGRES_USER=postgres
POSTGRES_PASSWORD=postgres
POSTGRES_DB=khoj
KHOJ_DJANGO_SECRET_KEY=your-secret-key
KHOJ_DEBUG=True
KHOJ_ADMIN_EMAIL=admin@example.com
KHOJ_ADMIN_PASSWORD=admin
KHOJ_DOMAIN=localhost
KHOJ_NO_HTTPS=True
```

---

## Key Conventions

### Python Code Style

1. **Formatting**: Uses `ruff format` (configured in pyproject.toml)
   - Line length: 120 characters
   - Quote style: Double quotes
   - Indent style: Spaces

2. **Linting**: Uses `ruff check`
   - Enabled: Error (E), Warning (F), Import (I) checks
   - Ignored: E501 (line length), F405 (undefined names), E402 (module import position)

3. **Type Checking**: Uses `mypy`
   - Run with: `mypy` (configured in pyproject.toml)
   - `strict_optional = false`
   - `ignore_missing_imports = true`

4. **Import Order** (via ruff isort):
   - Standard library imports
   - Third-party imports
   - First-party imports (khoj)

### Git Workflow

1. **Pre-commit Hooks**: Automatically run formatters and linters
   - Install: `./scripts/dev_setup.sh`
   - Manual run: `pre-commit run --hook-stage manual --all`

2. **Hooks**:
   - `ruff-check --fix`: Auto-fix linting issues
   - `ruff-format`: Format Python code
   - `mypy`: Type checking (on pre-push)
   - End-of-file fixer
   - Trailing whitespace removal
   - JSON/TOML/YAML validation

### Database Conventions

1. **Migrations**: Always create migrations for model changes
   ```bash
   python src/khoj/manage.py makemigrations
   ```

2. **Model Location**: All models in `src/khoj/database/models/__init__.py`

3. **Adapter Pattern**: Database access through adapters in `database/adapters/__init__.py`
   - Never query models directly from routers
   - Use adapter methods for all DB operations

### Frontend Conventions

1. **TypeScript**: Strict type checking enabled
2. **Formatting**: Prettier with:
   - Tab width: 4
   - Print width: 100
3. **Linting**: ESLint with Next.js config
4. **Build**: Always run `bun export` before creating PRs to test on port 42110
5. **Dev Server**: `bun dev` runs on port 3000 (note: streaming doesn't work in dev mode)

### API Design

1. **Route Organization**: Group related endpoints in router files
2. **Async by Default**: Use async functions for I/O operations
3. **Error Handling**: Use FastAPI's HTTPException for errors
4. **Authentication**: Required for most endpoints (handled via middleware)

### Naming Conventions

- **Python**: `snake_case` for functions, variables, modules
- **Classes**: `PascalCase`
- **Constants**: `UPPER_SNAKE_CASE`
- **TypeScript/React**: `camelCase` for variables/functions, `PascalCase` for components
- **Files**: `kebab-case` for frontend, `snake_case` for backend

---

## Testing

### Test Framework

- **Backend**: pytest with pytest-django
- **Configuration**: `pytest.ini` and `tests/conftest.py`
- **Database**: Uses `--reuse-db` flag for faster tests

### Running Tests

```bash
# Run all tests
pytest

# Run specific test file
pytest tests/test_client.py

# Run with verbose output
pytest -v

# Run tests in parallel
pytest -n auto

# Run specific marker
pytest -m chatquality

# Skip specific marker
pytest -m "not chatquality"
```

### Test Structure

```
tests/
├── conftest.py              # Shared fixtures and configuration
├── data/                    # Test data files
├── evals/                   # Quality evaluation tests
├── helpers.py               # Test helper functions
├── test_*.py                # Unit and integration tests
└── factories (in conftest)  # factory-boy for test data generation
```

### Key Test Files

- `test_client.py`: API endpoint tests
- `test_agents.py`: Agent functionality tests
- `test_conversation_utils.py`: Chat logic tests
- `test_text_search.py`: Search functionality tests
- `test_*_to_entries.py`: Content processor tests

### CI/CD Testing

Tests run automatically on:
- Pull requests to `master` or `release/1.x`
- Pushes to `master` or `release/1.x`
- Python versions: 3.10, 3.11, 3.12
- Database: PostgreSQL with pgvector (via Docker)

### Writing Tests

1. **Use Fixtures**: Leverage pytest fixtures in `conftest.py`
2. **Factory Pattern**: Use factory-boy for creating test data
3. **Async Tests**: Mark with `@pytest.mark.asyncio` for async functions
4. **Database Tests**: Marked with `@pytest.mark.django_db`
5. **Cleanup**: Tests should clean up after themselves (fixtures handle this)

---

## Common Development Workflows

### Adding a New API Endpoint

1. Create or update router file in `src/khoj/routers/`
2. Define FastAPI route with appropriate decorators
3. Add database adapters if needed in `database/adapters/`
4. Create/update models if needed (+ migrations)
5. Add tests in `tests/`
6. Update API documentation in `documentation/docs/`

### Adding a New Content Type Processor

1. Create processor in `src/khoj/processor/content/`
2. Implement entry extraction logic
3. Add to content type registry
4. Create tests with sample data in `tests/data/`
5. Update documentation

### Modifying the Frontend

1. Make changes in `src/interface/web/`
2. Test with dev server: `bun dev` (localhost:3000)
3. Build and test production: `bun export` (localhost:42110)
4. Ensure TypeScript types are correct: `bun lint`
5. Test with actual backend integration

### Creating Database Migrations

```bash
# After modifying models in database/models/__init__.py
python src/khoj/manage.py makemigrations

# Apply migrations
python src/khoj/manage.py migrate

# Check migration status
python src/khoj/manage.py showmigrations
```

### Debugging

1. **Enable Debug Mode**: Set `KHOJ_DEBUG=True` environment variable
2. **Verbose Logging**: Run with `khoj -vv` (double verbose)
3. **Django Admin**: Access at `/admin` with admin credentials
4. **API Docs**: Available at `/docs` when debug mode is on
5. **Database**: Use Django shell: `python src/khoj/manage.py shell`

---

## Important Files and Modules

### Core Application Files

- **`src/khoj/main.py`**: Application entry point, FastAPI and Django initialization
- **`src/khoj/configure.py`**: Route configuration and middleware setup
- **`src/khoj/app/settings.py`**: Django settings
- **`pyproject.toml`**: Python dependencies and tool configuration

### Router Files (API Endpoints)

- **`routers/api_chat.py`**: Chat endpoints (69KB, main chat logic)
- **`routers/api_content.py`**: Content management endpoints
- **`routers/api_agents.py`**: Agent management endpoints
- **`routers/api_automation.py`**: Automation and scheduling
- **`routers/helpers.py`**: Shared router utilities (127KB)
- **`routers/auth.py`**: Authentication endpoints
- **`routers/research.py`**: Research mode endpoints

### Database Files

- **`database/models/__init__.py`**: All Django models (34KB)
- **`database/adapters/__init__.py`**: All database adapters (88KB)
- **`database/migrations/`**: Django migration files

### Processor Files

- **`processor/conversation/`**: Chat and conversation processing
- **`processor/content/`**: Document processing (PDF, markdown, etc.)
- **`processor/tools/`**: AI tool implementations
- **`processor/image/`**: Image generation
- **`processor/speech/`**: Speech processing

### Utility Files

- **`utils/helpers.py`**: General helper functions (47KB)
- **`utils/initialization.py`**: App initialization logic
- **`utils/state.py`**: Application state management
- **`utils/constants.py`**: Application constants

### Frontend Files

- **`src/interface/web/app/`**: Next.js app directory (pages and layouts)
- **`src/interface/web/components/`**: React components
- **`src/interface/web/package.json`**: Frontend dependencies

### Configuration Files

- **`.pre-commit-config.yaml`**: Pre-commit hook configuration
- **`pytest.ini`**: Pytest configuration
- **`docker-compose.yml`**: Docker services configuration
- **`gunicorn-config.py`**: Gunicorn server configuration

---

## Deployment

### Docker Deployment

Services in `docker-compose.yml`:
- **database**: PostgreSQL with pgvector
- **sandbox**: Terrarium code execution environment
- **search**: SearxNG web search engine
- **computer**: Optional computer automation container
- **server**: Khoj application server

### Production Considerations

1. **Image**: Use `ghcr.io/khoj-ai/khoj:latest` or `ghcr.io/khoj-ai/khoj-cloud:latest`
2. **Security**:
   - Set strong `KHOJ_DJANGO_SECRET_KEY`
   - Configure proper `KHOJ_ADMIN_PASSWORD`
   - Set `KHOJ_DEBUG=False`
3. **HTTPS**: Remove `KHOJ_NO_HTTPS` for production
4. **Domain**: Set `KHOJ_DOMAIN` to your domain
5. **Volumes**: Persist data with named volumes (khoj_config, khoj_db, khoj_models)

### Release Process (Maintainers)

1. Run version bump script:
   ```bash
   ./scripts/bump_version.sh -c "<release_version>"
   ```

2. Push commit and tag:
   ```bash
   git push origin master
   git push origin <release_version>
   ```

3. Automated workflows will:
   - Build and publish to PyPI
   - Build and push Docker images
   - Create GitHub release with desktop apps
   - Update documentation

### CI/CD Workflows

Located in `.github/workflows/`:
- **test.yml**: Run tests on PR/push
- **dockerize.yml**: Build and push Docker images
- **pypi.yml**: Publish to PyPI
- **desktop.yml**: Build desktop apps
- **run_evals.yml**: Run quality evaluations
- **pre-commit.yml**: Lint and format checks

---

## Additional Resources

- **Documentation**: https://docs.khoj.dev
- **Website**: https://khoj.dev
- **Discord**: https://discord.gg/BDgyabRM6e
- **Blog**: https://blog.khoj.dev
- **GitHub Issues**: https://github.com/khoj-ai/khoj/issues
- **Contributing Guide**: documentation/docs/contributing/development.mdx

---

## Tips for AI Assistants

1. **Always check** `pyproject.toml` for current dependencies and versions
2. **Use adapters** for database operations, never query models directly from routers
3. **Run tests** after making changes: `pytest`
4. **Format code** before committing: pre-commit hooks handle this automatically
5. **Check CI/CD**: Ensure all workflows pass before merging
6. **Update documentation**: Keep docs in sync with code changes
7. **Ask for clarification**: Check Discord #contributors channel or GitHub issues
8. **Consider scale**: Code should work for both self-hosted and cloud deployments
9. **Security first**: Be mindful of authentication, authorization, and data privacy
10. **Test integrations**: Khoj supports many clients—test multi-platform when relevant

---

**Version**: 2.0.0-beta.22 (as of documentation creation)
**Last Updated**: 2025-12-10
