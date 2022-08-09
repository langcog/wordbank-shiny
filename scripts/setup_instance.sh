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

# download and install shiny server (and libssl1.1 due to an issue with the shiny server pro build, will probably get fixed in later releases)
wget http://security.ubuntu.com/ubuntu/pool/main/o/openssl/libssl1.1_1.1.1-1ubuntu2.1~18.04.20_amd64.deb
sudo gdebi libssl1.1_1.1.1-1ubuntu2.1~18.04.20_amd64.deb
wget https://s3.amazonaws.com/rstudio-shiny-server-pro-build/ubuntu-18.04/x86_64/shiny-server-commercial-1.5.18.1120-amd64.deb
sudo gdebi shiny-server-commercial-1.5.18.1120-amd64.deb
rm *.deb

# configure shiny server
# first deactivate license on previous instance if needed
# sudo /opt/shiny-server/bin/license-manager deactivate
# activate license
sudo /opt/shiny-server/bin/license-manager activate ${LICENSE_KEY}
# copy configuration file from repo to server location
sudo cp scripts/shiny-server.conf /etc/shiny-server/shiny-server.conf
# restart server
sudo systemctl restart shiny-server

# install R packages
sudo Rscript scripts/setup_r.R
