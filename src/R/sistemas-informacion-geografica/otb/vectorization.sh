#!/bin/bash

# Definicion de variables de entorno
INPUT=${CARPETA}/${PERIODO}.tif
INPUT_MERGED=${CARPETA}/${PERIODO}_segmentacion.tif
OUTPUT=${CARPETA}/${PERIODO}_vectorized.shp

# Ejecutar segmentacion
/opt/OTB/bin/otbcli_LSMSVectorization \
	-in ${INPUT} \
	-inseg ${INPUT_MERGED} \
	-out ${OUTPUT} \
	-tilesizex 1024 \
	-tilesizey 1024
