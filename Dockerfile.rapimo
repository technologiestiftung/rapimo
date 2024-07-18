# Use the official R base image
FROM r-base:4.1.0

# Set the working directory in the container
WORKDIR /app

# Install necessary system libraries (if needed)
# For example, if plumber requires certain system libraries, install them here.

# Install necessary system libraries
RUN apt-get update && \
    apt-get install -y \
    libsodium-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libarchive-dev \
    libxml2-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install necessary R packages
RUN R -e "install.packages('plumber', repos='http://cran.rstudio.com/', lib='/usr/local/lib/R/site-library')" && \
    R -e "install.packages(c('dplyr', 'jsonlite', 'remotes'))" && \
    R -e "remotes::install_github('KWB-R/kwb.rabimo@dev', lib='/usr/local/lib/R/site-library')"

# Copy the R scripts into the container
COPY endpoints.R .
COPY run_api.R .

# Expose the port that Plumber will run on
EXPOSE 8000

# Run Plumber when the container starts
ENTRYPOINT ["R", "-e", "plumber::plumb('endpoints.R')$run(host = '0.0.0.0', port = 8000)"]
