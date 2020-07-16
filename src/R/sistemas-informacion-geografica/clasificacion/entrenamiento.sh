#!/bin/bash
/opt/OTB/bin/otbcli_PolygonClassStatistics  -in    ../tp-teledeteccion2020/images/masked/201801_epsg_22185.tif \
                                            -vec   ../tp-teledeteccion2020/ground_truth/WaterBodies.shp \
                                            -field Objeto \
                                            -out   classes.xml
