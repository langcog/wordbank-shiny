cd /home/ubuntu/wordbank-shiny

# install system dependencies
sudo apt update && sudo apt install \
  mysql-client-core \
  mysql-server \
  libmariadb-dev \
  libcurl4-openssl-dev \
  libxml2-dev \
  r-base \
  gdebi-core

# configure mysql
sudo mysql < scripts/setup_db.sql

# download and install posit connect
# replace ubuntu version if not 22.04, update connect version
# latest at https://docs.posit.co/connect/admin/getting-started/local-install/server-install/
curl -O https://cdn.posit.co/connect/2025.01/rstudio-connect_2025.01.0~ubuntu22_amd64.deb
sudo apt install ./rstudio-connect_2025.01.0~ubuntu22_amd64.deb

# activate license
sudo /opt/rstudio-connect/bin/license-manager activate <LICENSE-KEY>

# configure connect
# e.g. copy of scripts/rstudio-connect.gcfg
sudo cp scripts/rstudio-connect.gcfg /etc/rstudio-connect/rstudio-connect.gcfg
sudo nano /etc/rstudio-connect/rstudio-connect.gcfg
