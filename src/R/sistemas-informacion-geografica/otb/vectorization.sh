#!/bin/bash

# Ejecutar segmentacion
/opt/OTB/bin/otbcli_LSMSVectorization \
	-in $1 \
	-inseg $2 \
	-out $3 \
	-tilesizex 1024 \
	-tilesizey 1024
