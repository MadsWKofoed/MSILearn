# MSI Clustering & Prediction

R/Shiny application for MSI processing, clustering, prediction, and database management.

## Configuration

Database settings are read from environment variables:

| Variable | Default for local R runs | Docker Compose value |
| --- | --- | --- |
| `APP_PLATFORM` | not used | `linux/amd64` |
| `APP_PORT` | not used | `3838` |
| `MONGO_DB` | `MSI_DB` | `MSI_DB` |
| `MONGO_URL` | `mongodb://localhost:27018` | `mongodb://mongo:27017` |
| `MONGO_HOST_PORT` | not used | `27018` |
| `MONGO_VERSION` | not used | `8.2.7` |
| `APP_WORKERS` | unset, auto-sized | unset, auto-sized |
| `APP_WORKER_MAX` | `8` | `8` |
| `APP_WORKER_RESERVE_CORES` | `1` | `1` |
| `APP_WORKER_CPU_FRACTION` | `0.75` | `0.75` |
| `APP_MEMORY_PER_WORKER_GB` | `2` | `2` |
| `APP_RESERVE_MEMORY_GB` | `2` | `2` |
| `APP_BIOCPARALLEL_BACKEND` | `snow` | `snow` |

The local defaults preserve the existing development workflow. When the app runs inside Docker, Compose sets `MONGO_URL` to the MongoDB service name (`mongo`) so the app does not depend on `localhost` inside the container.

`APP_PLATFORM` defaults to `linux/amd64` to match the remote server audit. Docker Desktop runs this platform through emulation on Apple Silicon Macs.

The worker settings control how many parallel workers the app uses for shared BiocParallel/Cardinal work and auto-sized training/clustering tasks. Leave `APP_WORKERS` empty for automatic sizing. The auto-sizer uses the CPU cores and memory visible inside Docker, reserves some headroom for the laptop, and caps workers at `APP_WORKER_MAX`. Set `APP_WORKERS` to a positive integer only when you want to force a specific worker count.

`APP_BIOCPARALLEL_BACKEND=snow` uses socket workers, which is safer for Shiny/Docker than forked multicore workers. Set it to `multicore` only if you specifically want forked workers on a Linux host and have tested that processing is stable.

Check the worker count seen by the running app container:

```sh
docker compose exec app Rscript -e 'source("R/config.R"); cat("cores=", app_available_cores(), " memory_gb=", round(app_available_memory_gb(), 1), " workers=", app_worker_count(), "\n", sep = "")'
```

## Runtime Versions

The Docker image mirrors the remote server audit as closely as practical:

- Ubuntu 22.04 Jammy
- R `4.4.3` from Posit's Ubuntu 22.04 binary package
- Bioconductor `3.20`
- Python `3.10` from Ubuntu 22.04, with Flask/OpenSlide pip packages pinned in the Dockerfile
- MongoDB `8.2.7`

R package versions captured from the server are listed in `docker/r-packages.csv`. CRAN dependencies are installed from Posit's Ubuntu Jammy binary repository at the pinned `2026-03-10` snapshot date, then direct CRAN packages are checked against the manifest and reinstalled from CRAN source archives when needed. Bioconductor packages are installed through `BiocManager` using Bioconductor `3.20`.

For strict long-term reproducibility, add an `renv.lock` file from a known-working local setup and update the Dockerfile to run `renv::restore()` instead of installing package names directly.

## Docker Setup

The Docker setup runs two services:

- `app`: the R/Shiny application, exposed on `http://localhost:3838`
- `mongo`: MongoDB, exposed to the host on `localhost:27018`

MongoDB data is stored in the named Docker volume `mongo-data`, so database contents persist across normal container restarts.

### Build and Start

Optionally copy the example environment file before starting:

```sh
cp .env.example .env
```

Build the app image and start both services:

```sh
docker compose up --build
```

Run in the background:

```sh
docker compose up --build -d
```

Open the app at:

```text
http://localhost:3838
```

### Stop

Stop and remove the running containers while keeping the MongoDB volume:

```sh
docker compose down
```

### Reset the Database

Stop the services and delete the persistent MongoDB volume:

```sh
docker compose down -v
```

Start again with a fresh database:

```sh
docker compose up --build
```

### Check MongoDB Connectivity from the App Container

With the services running, verify that the app can reach MongoDB through the Compose network:

```sh
docker compose exec app Rscript -e 'source("R/config.R"); con <- mongolite::mongo("studies", db = DB_NAME, url = MONGO_URL); print(con$count())'
```

The command should print a count and exit without a connection error.

## Local Development

You can still run the Shiny app directly from R as before. For local development against the Compose MongoDB container, start only MongoDB:

```sh
docker compose up -d mongo
```

Then run the app locally. By default it connects to:

```text
mongodb://localhost:27018
```

To use a different MongoDB instance locally, set the environment variables before starting R:

```sh
export MONGO_DB=MSI_DB
export MONGO_URL=mongodb://localhost:27018
```
