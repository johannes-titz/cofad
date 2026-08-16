ARG R_VERSION=4.6.1
FROM rocker/shiny:${R_VERSION}

LABEL org.opencontainers.image.title="cofad"
LABEL org.opencontainers.image.description="Contrast analysis Shiny app"
LABEL org.opencontainers.image.source="https://github.com/johannes-titz/cofad"
LABEL org.opencontainers.image.licenses="LGPL-3.0-or-later"

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      curl \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled --ncpus -1 \
      foreign \
      magrittr \
      plotly \
      rhandsontable \
      shinydashboard \
      shinyjs

COPY . /tmp/cofad

RUN R CMD INSTALL --clean --no-multiarch /tmp/cofad \
    && rm -rf /tmp/cofad /tmp/downloaded_packages

USER shiny
WORKDIR /home/shiny

EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=20s --retries=3 \
  CMD curl --fail --silent --show-error http://127.0.0.1:3838/ > /dev/null || exit 1

CMD ["R", "--vanilla", "-e", "shiny::runApp(cofad::run_app(), host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
