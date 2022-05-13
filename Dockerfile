FROM rocker/r-ver:3.6.3

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget \
    libpng-dev \
    libxml2-dev \
    libgsl0-dev \
    make \
    libcurl4-openssl-dev \
    libssl-dev \
    zlib1g-dev \
    libgmp3-dev \
    libmpfr-dev \
    libgit2-dev \
    libssh2-1-dev \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libproj-dev \
    libicu-dev \
    libglpk-dev

COPY /renv.lock ./renv.lock

# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny', 'rmarkdown', 'renv'), repos='$MRAN')" && \
    R -e "renv::consent(provided = TRUE)" && \
    R -e "renv::restore()" && \
    rm -rf /srv/shiny-server/* && \
    chown shiny:shiny /var/lib/shiny-server

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
