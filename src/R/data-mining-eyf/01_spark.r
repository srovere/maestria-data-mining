# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(arrow)
require(sparklyr)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- II. Crear conexión a Spark ----
# ---------------------------------------------------------------------------------------#

# Configuracion de Spark
spark.config                      <- sparklyr::spark_config()
spark.config[["spark.r.command"]] <- "/opt/R/default/bin/Rscript"

# Crear conexion
spark.connection <- sparklyr::spark_connect(master = "spark://yoda:7077", config = spark.config)

# Lectura de datos 
febrero <- sparklyr::spark_read_csv(sc = spark.connection, name = "febrero", delimiter = "\t", 
                                    quote = "", path = paste0(getwd(), "/input/201902.txt"))
abril   <- sparklyr::spark_read_csv(sc = spark.connection, name = "abril", delimiter = "\t", 
                                    quote = "", path = paste0(getwd(), "/input/201904.txt"))
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- III. Finalizar script ----
# ---------------------------------------------------------------------------------------#

# Cerrar conexion
sparklyr::spark_disconnect(spark.connection)
# ----------------------------------------------------------------------------------------