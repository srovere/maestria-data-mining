#!/bin/bash

# Ejecutar segmentacion
/opt/OTB/bin/otbcli_LSMSSegmentation \
	-in $1 \
	-inpos $2 \
	-out $3 \
	-spatialr 50 \
	-ranger 0.025 \
	-minsize 0 \
	-tilesizex 1024 \
	-tilesizey 1024
