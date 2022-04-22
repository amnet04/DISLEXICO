#!/bin/bash

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server

# Create env vars file for shiny
> /home/shiny/.Renviron
PROJDATA="/home/shiny/${ProjectName}.RData"
echo "PROJDATA = ${PROJDATA}" > /home/shiny/.Renviron

if [ ! -f ${PROJDATA} ]
then
    echo "No se encuentra el archivo con datos procesados, procesando..."
    /usr/local/bin/R -f /home/shiny/dataprocess.R
else    
  echo "Ya se encontró archivo ${PROJDATA}, si quiere procesarlo de nuevo, borre el creado"
fi

if [ "$APPLICATION_LOGS_TO_STDOUT" != "false" ];
then
    # push the "real" application logs to stdout with xtail in detached mode
    exec xtail /var/log/shiny-server/ &
fi

# start shiny server
exec shiny-server 2>&1
