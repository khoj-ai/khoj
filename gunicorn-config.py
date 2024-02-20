import multiprocessing

bind = "0.0.0.0:42110"
workers = 1
worker_class = "uvicorn.workers.UvicornWorker"
timeout = 120
keep_alive = 60
accesslog = "access.log"
errorlog = "error.log"
loglevel = "debug"
