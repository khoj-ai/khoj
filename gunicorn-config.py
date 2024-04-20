import multiprocessing

bind = "0.0.0.0:42110"
workers = 2
worker_class = "uvicorn.workers.UvicornWorker"
timeout = 120
keep_alive = 60
accesslog = "-"
errorlog = "-"
loglevel = "debug"
