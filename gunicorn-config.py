import os

bind = "0.0.0.0:42110"

# Worker Configuration
workers = int(os.environ.get("GUNICORN_WORKERS", 6))
worker_class = "uvicorn.workers.UvicornWorker"

# Worker Timeout Configuration
timeout = int(os.environ.get("GUNICORN_TIMEOUT", 180))
graceful_timeout = int(os.environ.get("GUNICORN_GRACEFUL_TIMEOUT", 90))
keep_alive = int(os.environ.get("GUNICORN_KEEP_ALIVE", 60))

# Logging Configuration
accesslog = "-"
errorlog = "-"
loglevel = "debug"
