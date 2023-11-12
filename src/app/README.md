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

### Install Postgres (with PgVector)

#### MacOS
- Install the [Postgres.app](https://postgresapp.com/).

#### Debian, Ubuntu
From [official instructions](https://wiki.postgresql.org/wiki/Apt)

```bash
sudo apt install -y postgresql-common
sudo /usr/share/postgresql-common/pgdg/apt.postgresql.org.sh
sudo apt install postgres-16 postgresql-16-pgvector
```

#### Windows
- Use the [recommended installer](https://www.postgresql.org/download/windows/)

#### From Source
1. Follow instructions to [Install Postgres](https://www.postgresql.org/download/)
2. Follow instructions to [Install PgVector](https://github.com/pgvector/pgvector#installation) in case you need to manually install it. Reproduced instructions below for convenience.

```bash
cd /tmp
git clone --branch v0.5.1 https://github.com/pgvector/pgvector.git
cd pgvector
make
make install # may need sudo
```

### Create the Khoj database

#### MacOS
```bash
createdb khoj -U postgres
```

#### Debian, Ubuntu
```bash
sudo -u postgres createdb khoj
```

- [Optional] To set default postgres user's password
  - Execute `ALTER USER postgres PASSWORD 'my_secure_password';` using `psql`
  - Run `export $POSTGRES_PASSWORD=my_secure_password` in your terminal for Khoj to use it later

### Install Khoj

```bash
pip install -e '.[dev]'
```

### Make Khoj DB migrations

This command will create the migrations for the database app. This command should be run whenever a new db model is added to the database app or an existing db model is modified (updated or deleted).

```bash
python3 src/manage.py makemigrations
```

### Run Khoj DB migrations

This command will run any pending migrations in your application.
```bash
python3 src/manage.py migrate
```

### Start Khoj Server

While we're using Django for the ORM, we're still using the FastAPI server for the API. This command automatically scaffolds the Django application in the backend.

*Note: Anonymous mode bypasses authentication for local, single-user usage.*

```bash
python3 src/khoj/main.py --anonymous-mode
```
