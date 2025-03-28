# Use rocker/shiny as the base image (pre-configured for Shiny apps)
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libjpeg-dev \
    libpng-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('caTools')"
RUN R -e "install.packages('car')"
RUN R -e "install.packages('caret')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinythemes')"
RUN R -e "install.packages('shinyjs')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('randomForest')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('DALEX')"

# Copy your app's R code into the container
COPY . /srv/shiny-server/

# Set the working directory
WORKDIR /srv/shiny-server

# Expose the port Shiny uses
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server')"]
