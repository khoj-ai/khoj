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


## 🚀 HPU Support
### 🛠️ Setup for HPU

To run Khoj on a Habana Gaudi device, follow these steps:

1. **Build the HPU Docker Image**:
   Use the provided `Dockerfile.hpu` to build a Docker image optimized for HPU:
   ```bash
   docker build -t khoj-hpu -f Dockerfile.hpu .
   ```

2. **Run the Docker Container**:
   Start the container with the appropriate environment variables for HPU:
   ```bash
   docker run --runtime=habana -e HABANA_VISIBLE_DEVICES=all -p <PORT>:<PORT> khoj-hpu
   ```
   Replace `<PORT>` with the port number you want to expose.

3. **Verify HPU Support**:
   Ensure that the application detects the HPU device by checking the logs. The application will automatically use the HPU if available.

### 📦 New Dependencies

To support HPU and other advanced features, we've added the following dependencies:

- **`optimum-habana`**: Optimizes models for Habana Gaudi accelerators.
- **`torch-geometric`**: Enables deep learning on graph-based data structures.
- **`numba`**: Accelerates Python code by compiling it to machine code at runtime.

These dependencies are automatically installed when you build the Docker image or install the project locally.

### 🧠 Device Selection

The application now supports multiple device types, including **CUDA**, **HPU**, **MPS** (Apple Silicon), and **CPU**. You can specify your preferred device by passing the `preferred_device` argument to the `get_device()` function in `helpers.py`. For example:

```python
device = get_device(preferred_device="hpu")  # Use HPU if available
```

If no preferred device is specified, the application will automatically select the best available device.

### 📝 Notes

- Ensure that your system has the necessary Habana drivers and software stack installed to use HPUs.
- For more information on Habana Gaudi accelerators, visit the [Habana Labs documentation](https://docs.habana.ai/).

