# Use the official R Shiny server base image
FROM rocker/shiny:latest

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libglpk-dev \
    libmagick++-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    build-essential \
    libreadline-dev

# Install tidyverse and log the installation
RUN R -e "install.packages('tidyverse', repos='https://cloud.r-project.org')" > /tmp/tidyverse_install.log 2>&1

# Install other R packages
RUN R -e "install.packages(c('shiny', 'shinyWidgets', 'sf', 'viridis', 'shinythemes', 'ggiraph', 'kableExtra', 'stringr', 'AMR'), repos='https://cloud.r-project.org')" > /tmp/other_packages_install.log 2>&1

# Set repository for 'certegis' and install it
RUN R -e "options(repos = c(CerteMedEpi = 'https://certe-medical-epidemiology.r-universe.dev', repos = 'https://cloud.r-project.org')); install.packages('certegis')" > /tmp/certegis_install.log 2>&1

# Copy the Shiny app files into the container
COPY . /srv/shiny-server/

# Set file permissions
RUN chmod -R 755 /srv/shiny-server/

# Expose the port for the Shiny app
EXPOSE 3838

# Run the Shiny server
CMD ["/usr/bin/shiny-server"]