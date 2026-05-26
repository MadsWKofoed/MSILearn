FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Europe/Copenhagen \
    SHINY_PORT=3838 \
    MONGO_DB=MSI_DB \
    MONGO_URL=mongodb://mongo:27017 \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PIP_PROGRESS_BAR=off

RUN rm -f /etc/apt/apt.conf.d/docker-clean

RUN find /etc/dpkg/dpkg.cfg.d -type f -exec grep -l "pkg-config-dpkghook" {} \; \
    | xargs -r rm -f

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    cmake \
    curl \
    gfortran \
    libbz2-dev \
    libcurl4-openssl-dev \
    libfftw3-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libglpk-dev \
    libharfbuzz-dev \
    libhdf5-dev \
    libjpeg-dev \
    liblzma-dev \
    libnetcdf-dev \
    libopenblas-dev \
    libpng-dev \
    libproj-dev \
    libsasl2-dev \
    libssl-dev \
    libtiff5-dev \
    libudunits2-dev \
    libxml2-dev \
    libxt-dev \
    netcdf-bin \
    pkg-config \
    python3 \
    python3-pip \
    zlib1g-dev \
    && find /etc/dpkg/dpkg.cfg.d -type f -exec grep -l "pkg-config-dpkghook" {} \; | xargs -r rm -f \
    && rm -rf /var/lib/apt/lists/*

RUN python3 -c "exec('import time, urllib.request\\nurl=\"https://cdn.posit.co/r/ubuntu-2204/pkgs/r-4.4.3_1_amd64.deb\"\\nout=\"/tmp/r-4.4.3.deb\"\\nlast=None\\nfor attempt in range(1, 6):\\n    try:\\n        urllib.request.urlretrieve(url, out)\\n        break\\n    except Exception as exc:\\n        last=exc\\n        print(f\"download attempt {attempt} failed: {exc}\", flush=True)\\n        time.sleep(5)\\nelse:\\n    raise last')" \
    && apt-get update \
    && find /etc/dpkg/dpkg.cfg.d -type f -exec grep -l "pkg-config-dpkghook" {} \; | xargs -r rm -f \
    && apt-get install -y --no-install-recommends /tmp/r-4.4.3.deb \
    && ln -sf /opt/R/4.4.3/bin/R /usr/local/bin/R \
    && ln -sf /opt/R/4.4.3/bin/Rscript /usr/local/bin/Rscript \
    && rm -f /tmp/r-4.4.3.deb \
    && rm -rf /var/lib/apt/lists/*

RUN python3 -m pip install --no-cache-dir --progress-bar off --retries 5 --timeout 60 \
    Flask==3.1.3 \
    openslide-bin==4.0.0.12 \
    openslide-python==1.4.3

COPY docker/r-packages.csv /tmp/r-packages.csv
COPY docker/install_r_packages.R /tmp/install_r_packages.R
RUN CRAN_REPO="https://packagemanager.posit.co/cran/__linux__/jammy/2026-03-10" \
    BIOC_VERSION="3.20" \
    R_INSTALL_NCPUS="1" \
    Rscript /tmp/install_r_packages.R /tmp/r-packages.csv

WORKDIR /app
COPY . /app

EXPOSE 3838

CMD ["Rscript", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.integer(Sys.getenv('SHINY_PORT', '3838')))"]
