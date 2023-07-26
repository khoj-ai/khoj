from pathlib import Path

app_root_directory = Path(__file__).parent.parent.parent
web_directory = app_root_directory / "khoj/interface/web/"
empty_escape_sequences = "\n|\r|\t| "
app_env_filepath = "~/.khoj/env"
telemetry_server = "https://khoj.beta.haletic.com/v1/telemetry"

# default app config to use
default_config = {
    "content-type": {
        "org": {
            "input-files": None,
            "input-filter": None,
            "compressed-jsonl": "~/.khoj/content/org/org.jsonl.gz",
            "embeddings-file": "~/.khoj/content/org/org_embeddings.pt",
            "index-heading-entries": False,
        },
        "markdown": {
            "input-files": None,
            "input-filter": None,
            "compressed-jsonl": "~/.khoj/content/markdown/markdown.jsonl.gz",
            "embeddings-file": "~/.khoj/content/markdown/markdown_embeddings.pt",
        },
        "pdf": {
            "input-files": None,
            "input-filter": None,
            "compressed-jsonl": "~/.khoj/content/pdf/pdf.jsonl.gz",
            "embeddings-file": "~/.khoj/content/pdf/pdf_embeddings.pt",
        },
        "image": {
            "input-directories": None,
            "input-filter": None,
            "embeddings-file": "~/.khoj/content/image/image_embeddings.pt",
            "batch-size": 50,
            "use-xmp-metadata": False,
        },
        "github": {
            "pat-token": None,
            "repos": [],
            "compressed-jsonl": "~/.khoj/content/github/github.jsonl.gz",
            "embeddings-file": "~/.khoj/content/github/github_embeddings.pt",
        },
        "notion": {
            "token": None,
            "compressed-jsonl": "~/.khoj/content/notion/notion.jsonl.gz",
            "embeddings-file": "~/.khoj/content/notion/notion_embeddings.pt",
        },
    },
    "search-type": {
        "symmetric": {
            "encoder": "sentence-transformers/all-MiniLM-L6-v2",
            "cross-encoder": "cross-encoder/ms-marco-MiniLM-L-6-v2",
            "model_directory": "~/.khoj/search/symmetric/",
        },
        "asymmetric": {
            "encoder": "sentence-transformers/multi-qa-MiniLM-L6-cos-v1",
            "cross-encoder": "cross-encoder/ms-marco-MiniLM-L-6-v2",
            "model_directory": "~/.khoj/search/asymmetric/",
        },
        "image": {"encoder": "sentence-transformers/clip-ViT-B-32", "model_directory": "~/.khoj/search/image/"},
    },
    "processor": {
        "conversation": {
            "openai": {
                "api-key": None,
                "chat-model": "gpt-3.5-turbo",
            },
            "enable-offline-chat": False,
            "conversation-logfile": "~/.khoj/processor/conversation/conversation_logs.json",
        }
    },
}
