# Base image with Shiny Server
FROM rocker/shiny:4.3.2

# Install system dependencies needed by common R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Copy app into Shiny Server directory
COPY . /srv/shiny-server/krapfen

WORKDIR /srv/shiny-server/krapfen

# Restore R packages from renv.lock
RUN R -e "renv::restore()"

# Fix permissions
RUN chown -R shiny:shiny /srv/shiny-server

RUN echo "options(shiny.maxRequestSize=5000*1024^2)" > .Rprofile

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = 3838)"]

