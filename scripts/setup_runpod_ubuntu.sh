#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

export DEBIAN_FRONTEND=noninteractive

if command -v sudo >/dev/null 2>&1; then
  SUDO=sudo
else
  SUDO=
fi

echo "Installing Ubuntu system dependencies for R packages..."
$SUDO apt-get update
$SUDO apt-get install -y \
  ca-certificates \
  curl \
  git \
  wget \
  r-base \
  r-base-dev \
  build-essential \
  gfortran \
  cmake \
  pandoc \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libgit2-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libjpeg-dev \
  libtiff5-dev \
  libxt-dev \
  libblas-dev \
  liblapack-dev \
  libnlopt-dev \
  libgsl-dev

echo "Installing R packages through the project setup script..."
Rscript scripts/setup_packages.R

echo "Running package import smoke test..."
Rscript -e "source('R/strategy.R'); load_strategy_packages(); cat('R package setup OK\n')"

echo "Runpod setup finished."
