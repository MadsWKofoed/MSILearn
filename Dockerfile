FROM ubuntu:22.04

ARG R_VERSION=4.4.3
ARG BIOC_VERSION=3.20
ARG CRAN_SNAPSHOT=2026-03-10

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Europe/Copenhagen \
    SHINY_PORT=3838 \
    MONGO_DB=MSI_DB \
    MONGO_URL=mongodb://mongo:27017 \
    R_VERSION=${R_VERSION} \
    BIOC_VERSION=${BIOC_VERSION} \
    CRAN_REPO=https://packagemanager.posit.co/cran/__linux__/jammy/${CRAN_SNAPSHOT}

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
    locales \
    netcdf-bin \
    pkg-config \
    python3 \
    python3-pip \
    zlib1g-dev \
    && locale-gen en_US.UTF-8 \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8

RUN curl -fsSL -o /tmp/r-${R_VERSION}.deb \
    "https://cdn.posit.co/r/ubuntu-2204/pkgs/r-${R_VERSION}_1_amd64.deb" \
    && apt-get update \
    && apt-get install -y --no-install-recommends /tmp/r-${R_VERSION}.deb \
    && ln -sf /opt/R/${R_VERSION}/bin/R /usr/local/bin/R \
    && ln -sf /opt/R/${R_VERSION}/bin/Rscript /usr/local/bin/Rscript \
    && rm -f /tmp/r-${R_VERSION}.deb \
    && rm -rf /var/lib/apt/lists/*

RUN python3 -m pip install --no-cache-dir \
    Flask==3.1.3 \
    openslide-bin==4.0.0.12 \
    openslide-python==1.4.3

COPY docker/r-packages.csv /tmp/r-packages.csv
COPY docker/install_r_packages.R /tmp/install_r_packages.R
RUN Rscript /tmp/install_r_packages.R /tmp/r-packages.csv

WORKDIR /app
COPY . /app

EXPOSE 3838

CMD ["Rscript", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.integer(Sys.getenv('SHINY_PORT', '3838')))"]
