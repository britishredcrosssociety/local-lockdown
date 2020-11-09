FROM rocker/shiny:3.6.3
COPY . /srv/shiny-server

ARG TIDYR_VERSION=1.0.3

# Install dependencies
RUN apt-get update \
    # Get dependencies for Ubuntu to install R deps
    && apt-get install -y \
        libudunits2-dev \
        gdal-bin \
        proj-bin \
        libgdal-dev \
        libproj-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
    # I'd rather this was automated, but that's a limitation of R
    && install2.r --error \
        --skipinstalled \
        remotes \
        stringr \
        readr \
        magrittr \
        dplyr \
        sf \
        leaflet \
        scales \
        shinydashboard \
        shinyWidgets \
        echarts4r \
        sever \
        waiter \
        dashboardthemes \
    # Needed for installGithub.r
    && install2.r --error remotes \
    # Tidyr v1.1.1 has a cpp11 error - https://stackoverflow.com/questions/63348135/error-installing-tidyr-on-ubuntu-18-04-r-4-0-2
    && installGithub.r \
        --deps TRUE \
        tidyverse/tidyr@v${TIDYR_VERSION}

USER shiny
