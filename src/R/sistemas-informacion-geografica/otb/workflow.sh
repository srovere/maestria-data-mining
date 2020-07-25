#!/bin/bash

PERIODO=$1
CARPETA_INPUT=../tp-teledeteccion2020/images/${PERIODO}
CARPETA_OUTPUT=../tp-teledeteccion2020/images/segmentacion/${PERIODO}

for ARCHIVO_COMPLETO in "$CARPETA_INPUT"/*
do
	ARCHIVO=`basename $ARCHIVO_COMPLETO`
	NOMBRE="${ARCHIVO%.tif}"

	echo "Realizando smoothing en $NOMBRE"
	./smoothing.sh $ARCHIVO_COMPLETO ${CARPETA_OUTPUT}/${NOMBRE}_smooth.tif ${CARPETA_OUTPUT}/${NOMBRE}_position.tif

	echo "Realizando segmentacion en $NOMBRE"
	./segmentation.sh ${CARPETA_OUTPUT}/${NOMBRE}_smooth.tif ${CARPETA_OUTPUT}/${NOMBRE}_position.tif ${CARPETA_OUTPUT}/${NOMBRE}_segmentacion.tif

	echo "Realizando vectorizacion en $NOMBRE"
	./vectorization.sh $ARCHIVO_COMPLETO ${CARPETA_OUTPUT}/${NOMBRE}_segmentacion.tif ${CARPETA_OUTPUT}/${NOMBRE}_vectorial.shp
done
