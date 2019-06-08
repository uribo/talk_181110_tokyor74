FROM rocker/binder:3.6.0

## Copies your repo files into the Docker Container
USER root
COPY DESCRIPTION .

## Install dependencies
RUN R -q -e 'remotes::install_deps(".")'
