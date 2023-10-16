# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# set up github command line utility (gh cli)
ENV GITHUB_CLI_VERSION=2.36.0

RUN apt-get update && apt-get install -y \
    curl \
    git

RUN curl -sSL https://github.com/cli/cli/releases/download/v${GITHUB_CLI_VERSION}/gh_${GITHUB_CLI_VERSION}_linux_amd64.tar.gz | tar xz -C /tmp && \
    mv /tmp/gh_${GITHUB_CLI_VERSION}_linux_amd64/bin/gh /usr/local/bin/gh && \
    rm -rf /tmp/gh_${GITHUB_CLI_VERSION}_linux_amd64


# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libpoppler-cpp-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## renv.lock file
COPY /renv.lock ./renv.lock

## app
COPY /projects ./app/projects
COPY /servers ./app/servers
COPY /UIs ./app/UIs
COPY /www ./app/www
COPY *.R ./app/

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'

# added by ethan to install tinytex so I can knit rmarkdown to pdf
RUN Rscript -e 'tinytex::install_tinytex()'


# expose port
EXPOSE 3838

# run github cli authorization to get it connected to repo
CMD gh auth login

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
