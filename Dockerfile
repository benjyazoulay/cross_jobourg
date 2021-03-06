FROM openanalytics/r-base

MAINTAINER Tobias Verbeke "tobias.verbeke@openanalytics.eu"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the cross_jobourg app
RUN apt-get update && apt-get install -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('ggplot2','plotly','stringr','Hmisc','xml2','shinythemes','htmlwidgets','httr','ngramr','dplyr','htmltools'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('shinyWidgets'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('DT'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('lubridate'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('rvest'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('shinybusy'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/cross_jobourg
COPY cross_jobourg /root/cross_jobourg

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/cross_jobourg')"]