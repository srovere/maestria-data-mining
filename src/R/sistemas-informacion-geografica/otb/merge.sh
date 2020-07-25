#!/bin/bash

# Definicion de variables de entorno
INPUT_SMOOTH=${CARPETA}/${PERIODO}_smooth.tif
INPUT_SEGMENTACION=${CARPETA}/${PERIODO}_segmentacion.tif
OUTPUT=${CARPETA}/${PERIODO}_merged.tif

# Ejecutar segmentacion
/opt/OTB/bin/otbcli_LSMSSmallRegionsMerging \
	-in ${INPUT_SMOOTH} \
	-inseg ${INPUT_SEGMENTACION} \
	-out ${OUTPUT} \
	-minsize 256 \
	-tilesizex 1024 \
	-tilesizey 1024
