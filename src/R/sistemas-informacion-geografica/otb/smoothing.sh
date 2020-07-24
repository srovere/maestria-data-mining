#!/bin/bash

# Definicion de variables de entorno
INPUT=${CARPETA}/${PERIODO}.tif
OUTPUT_SMOOTH=${CARPETA}/${PERIODO}_smooth.tif
OUTPUT_POSICION=${CARPETA}/${PERIODO}_posicion.tif

# Ejecutar smoothing
/opt/OTB/bin/otbcli_MeanShiftSmoothing \
        -in ${INPUT} \
        -fout ${OUTPUT_SMOOTH} \
        -foutpos ${OUTPUT_POSICION} \
        -spatialr 10 \
        -ranger 0.005 \
        -thres 0.1 \
        -maxiter 100 \
        -ram 16384
