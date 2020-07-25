#!/bin/bash

export CARPETA=../tp-teledeteccion2020/images/final
export PERIODO=$1

./smoothing.sh
./segmentation.sh
./vectorization.sh
