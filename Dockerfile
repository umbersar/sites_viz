FROM rocker/shiny:3.5.1

RUN apt-get update && apt-get install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev libudunits2-dev libgdal-dev gdal-bin libproj-dev proj-data proj-bin libgeos-dev -y &&\
  mkdir -p /var/lib/shiny-server/bookmarks/shiny

RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sf', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sp', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet.extras', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('devtools', repos='http://cran.rstudio.com/', dependecies=TRUE)"
RUN R -e "devtools::install_github('r-spatial/leafgl')"

# Copy all scripts into working directory
COPY . /srv/shiny-server/

#remove root permissions
USER root
# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# select port
EXPOSE 3838

# run app
CMD ["/usr/bin/shiny-server.sh"]
