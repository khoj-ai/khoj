# Django App

Khoj uses Django as the backend framework primarily for its powerful ORM and the admin interface. The Django app is located in the `src/app` directory. We have one installed app, under the `/database/` directory. This app is responsible for all the database related operations and holds all of our models. You can find the extensive Django documentation [here](https://docs.djangoproject.com/en/4.2/) 🌈.

## Setup (Docker)

### Prerequisites
1. Ensure you have [Docker](https://docs.docker.com/get-docker/) installed.
2. Ensure you have [Docker Compose](https://docs.docker.com/compose/install/) installed.

### Run

Using the `docker-compose.yml` file in the root directory, you can run the Khoj app using the following command:
```bash
docker-compose up
```

## Setup (Local)

### Install dependencies

```bash
pip install -e '.[dev]'
```

### Setup the database

1. Ensure you have Postgres installed. For MacOS, you can use [Postgres.app](https://postgresapp.com/).
2. If you're not using Postgres.app, you may have to install the pgvector extension manually. You can find the instructions [here](https://github.com/pgvector/pgvector#installation). If you're using Postgres.app, you can skip this step. Reproduced instructions below for convenience.

```bash
cd /tmp
git clone --branch v0.5.1 https://github.com/pgvector/pgvector.git
cd pgvector
make
make install # may need sudo
```
3. Create a database

### Make migrations

This command will create the migrations for the database app. This command should be run whenever a new model is added to the database app or an existing model is modified (updated or deleted).

```bash
python3 src/manage.py makemigrations
```

### Run migrations

This command will run any pending migrations in your application.
```bash
python3 src/manage.py migrate
```

### Run the server

While we're using Django for the ORM, we're still using the FastAPI server for the API. This command automatically scaffolds the Django application in the backend.
```bash
python3 src/khoj/main.py
```
