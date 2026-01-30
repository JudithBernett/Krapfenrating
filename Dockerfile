# Base image with Shiny Server
FROM rocker/shiny:4.3.2

# Install system dependencies needed by common R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    vim \
    && rm -rf /var/lib/apt/lists/*

# Install renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Copy renv files first for better layer caching
COPY renv.lock /srv/shiny-server/krapfen/
COPY .Rprofile /srv/shiny-server/krapfen/
COPY renv /srv/shiny-server/krapfen/renv

WORKDIR /srv/shiny-server/krapfen

# Restore R packages from renv.lock
RUN R -e "renv::restore()"

# Copy data files (these will be the initial state)
COPY data/*.csv /srv/shiny-server/krapfen/data/

# Copy the rest of the app
COPY app.R posterior.R utils.R /srv/shiny-server/krapfen/
COPY www /srv/shiny-server/krapfen/www
RUN R -e "renv::restore()"

# Fix permissions
RUN chown -R shiny:shiny /srv/shiny-server

RUN echo "options(shiny.maxRequestSize=5000*1024^2)" > .Rprofile

# Create a volume mount point for persistent data
VOLUME ["/srv/shiny-server/krapfen/data"]

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = 3838)"]

