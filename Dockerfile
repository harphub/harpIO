# syntax=docker/dockerfile:1.4

# Build: docker build  -t harpio -f Dockerfile .
# Run: docker run -it --rm -v ./data:/data harpio

FROM ubuntu

WORKDIR /app
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y libproj-dev r-base r-base-dev --no-install-recommends && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Install harpIO from github
# RUN R -e 'install.packages("remotes")'
# RUN R -e 'remotes::install_github("harphub/harpIO")'

# Install deps
RUN R -e 'install.packages("remotes")'
RUN R -e 'install.packages("DBI")'
RUN R -e 'install.packages("dplyr")'
RUN R -e 'install.packages("glue")'
RUN R -e 'install.packages("lubridate")'
RUN R -e 'install.packages("purrr")'
RUN R -e 'install.packages("readr")'
RUN R -e 'install.packages("rlang")'
RUN R -e 'install.packages("RSQLite")'
RUN R -e 'install.packages("stringr")'
RUN R -e 'install.packages("tibble")'
RUN R -e 'install.packages("tidyr")'
RUN R -e 'install.packages("dbplyr")'
RUN R -e 'install.packages("tidyselect")'
RUN R -e 'install.packages("lifecycle")'
RUN R -e 'remotes::install_github("harphub/harpCore")'
RUN R -e 'remotes::install_github("harphub/meteogrid")'

# Install harpIO from local directory
COPY . /harpio/
RUN R -e 'install.packages("/harpio", repos = NULL, type="source")'

ENTRYPOINT [ "R", "--save" ]
