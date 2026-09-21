# MSI Clustering & Prediction

R/Shiny application for MSI processing, clustering, prediction, and database management.

## Prerequisites

- [Docker Desktop](https://www.docker.com/products/docker-desktop/) (or Docker Engine + the Compose v2 plugin), installed and running before any `docker compose` command
- `git`
- On Apple Silicon Macs: Rosetta emulation enabled in Docker Desktop (Settings → General → "Use Rosetta for x86_64/amd64 emulation on Apple Silicon"), since the image targets `linux/amd64`

## Getting the Code

```sh
git clone https://github.com/MadsWKofoed/MSILearn.git
cd MSILearn
```

## Configuration

Database settings are read from environment variables:

| Variable | Default for local R runs | Docker Compose value |
| --- | --- | --- |
| `APP_PLATFORM` | not used | `linux/amd64` |
| `APP_PORT` | not used | `3838` |
| `APP_CPUS` | not used | `2` |
| `APP_MEM_LIMIT` | not used | `8g` |
| `MONGO_DB` | `MSI_DB` | `MSI_DB` |
| `MONGO_URL` | `mongodb://localhost:27018` | `mongodb://mongo:27017` |
| `MONGO_HOST_PORT` | not used | `27018` |
| `MONGO_VERSION` | not used | `8.2.7` |
| `APP_WORKERS` | unset, auto-sized | unset, auto-sized |
| `APP_WORKER_MAX` | `8` | `8` |
| `APP_WORKER_RESERVE_CORES` | `1` | `1` |
| `APP_WORKER_CPU_FRACTION` | `0.75` | `0.75` |
| `APP_MEMORY_PER_WORKER_GB` | `4` | `4` |
| `APP_RESERVE_MEMORY_GB` | `4` | `4` |
| `APP_BIOCPARALLEL_BACKEND` | `snow` | `snow` |

The local defaults preserve the existing development workflow. When the app runs inside Docker, Compose sets `MONGO_URL` to the MongoDB service name (`mongo`) so the app does not depend on `localhost` inside the container.

`APP_PLATFORM` defaults to `linux/amd64` to match the remote server audit. Docker Desktop runs this platform through emulation on Apple Silicon Macs.

Docker has hard container limits and the app has worker settings. `APP_CPUS` and `APP_MEM_LIMIT` are Docker-only hard caps for the app container. The worker settings control how many parallel workers the app starts for shared BiocParallel/Cardinal work and auto-sized training/clustering tasks.

Leave `APP_WORKERS` empty for automatic sizing. The auto-sizer uses the CPU cores and memory visible to R, reserves some headroom, and caps workers at `APP_WORKER_MAX`. Set `APP_WORKERS` to a positive integer when you want to force a specific worker count.

`APP_BIOCPARALLEL_BACKEND=snow` uses socket workers, which is safer for Shiny/Docker than forked multicore workers. Set it to `multicore` only if you specifically want forked workers on a Linux host and have tested that processing is stable.

Use `.env.example` as the conservative default for laptop Docker runs. On a large shared server running Docker, use `.env.server.example` as the starting point:

```sh
cp .env.server.example .env
```

For direct R runs on the server, use `.Renviron.server.example` instead:

```sh
cp .Renviron.server.example .Renviron
```

Do not use `.env.server.example` for direct R unless you also change `MONGO_URL`, because Docker uses `mongodb://mongo:27017` while direct R normally uses the host-mapped `mongodb://localhost:27018`.

For direct R runs, Docker hard caps do not apply. Configure workers with shell environment variables or a `.Renviron` file:

```sh
export APP_WORKERS=12
Rscript -e 'shiny::runApp(".", host = "0.0.0.0", port = 3838)'
```

or:

```sh
cp .Renviron.server.example .Renviron
```

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

The first build compiles R from source packages and installs Bioconductor/Cardinal and other mass-spec dependencies, so it can easily take 10-20+ minutes and several GB of disk space depending on your machine and network. This is expected — it is not hung. On Apple Silicon, emulating `linux/amd64` makes both the build and the running app noticeably slower than on native amd64 hardware. Subsequent builds are much faster thanks to Docker layer caching, unless `docker/r-packages.csv` or the Dockerfile changes.

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

## Troubleshooting

- **`Cannot connect to the Docker daemon` / socket errors**: Docker Desktop is not running. Start it and wait until it reports "running" before retrying.
- **Port already in use (`3838` or `27018`)**: another process (maybe a previous `docker compose up`) is already bound to that port. Stop it with `docker compose down`, or override the port via `.env` (`APP_PORT`, `MONGO_HOST_PORT`).
- **Build seems stuck**: check that it's still working with `docker compose logs -f app` or `docker builder prune -f` if a previous interrupted build left a bad cache. R/Bioconductor compilation is slow and quiet for long stretches, especially under Apple Silicon emulation.
- **`app` container exits immediately**: run `docker compose logs app` to see the R error; a common cause is `mongo` not being healthy yet (Compose's `depends_on: condition: service_healthy` should prevent this, but check `docker compose ps`).
