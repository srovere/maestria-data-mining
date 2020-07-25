#!/bin/bash

# Ejecutar smoothing
/opt/OTB/bin/otbcli_MeanShiftSmoothing \
        -in $1 \
        -fout $2 \
        -foutpos $3 \
        -spatialr 100 \
        -ranger 0.05 \
        -thres 0.1 \
        -maxiter 100 \
        -ram 16384
