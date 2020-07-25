#!/bin/bash

# Definicion de variables de entorno
INPUT_SMOOTH=${CARPETA}/${PERIODO}_smooth.tif
INPUT_POSICION=${CARPETA}/${PERIODO}_posicion.tif
OUTPUT=${CARPETA}/${PERIODO}_segmentacion.tif

# Ejecutar segmentacion
/opt/OTB/bin/otbcli_LSMSSegmentation \
	-in ${INPUT_SMOOTH} \
	-inpos ${INPUT_POSICION} \
	-out ${OUTPUT} \
	-spatialr 10 \
	-ranger 0.005 \
	-minsize 0 \
	-tilesizex 1024 \
	-tilesizey 1024
