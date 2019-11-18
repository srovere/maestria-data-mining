#!/bin/bash
SCRIPT=$1
CONFIGURACION=$2
Rscript ${SCRIPT} ${CONFIGURACION}
sudo shutdown -h now
