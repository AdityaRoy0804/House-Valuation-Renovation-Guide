# Use the official R image with Shiny pre-installed
FROM rocker/shiny:latest

# Copy the Shiny app files into the container
COPY . /srv/shiny-server/

# Set the working directory to the app directory
WORKDIR /srv/shiny-server/

# Install any additional R packages (if necessary)
RUN R -e "install.packages('shiny')"

# Expose the port that Shiny uses (default is 3838)
EXPOSE 3838

# Start the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
