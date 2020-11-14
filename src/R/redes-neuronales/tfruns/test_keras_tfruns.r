# Borrar ambiente
rm(list = objects())

# Cargar librerias
require(caret)
require(dplyr)
require(keras)
require(purrr)
require(reticulate)
require(stringr)
require(tensorflow)
require(tfestimators)
require(tfruns)

# Especificar variables de entorno
{
  conda_home <- "/opt/anaconda3"
  conda_bin  <- paste0(conda_home, "/bin/conda")
  conda_env  <- "r-reticulate"
  conda_lib  <- paste0(conda_home, "/envs/", conda_env, "/lib")
  if (is.na(stringr::str_locate(Sys.getenv("LD_LIBRARY_PATH"), conda_lib)[,1])) {
    Sys.setenv(LD_LIBRARY_PATH=paste0(Sys.getenv("LD_LIBRARY_PATH"), ":", conda_lib))
  }
  Sys.getenv("LD_LIBRARY_PATH")
  
  # Inicializar tensorflow
  tensorflow::use_condaenv(condaenv = conda_env, conda = conda_bin)
  
  # Indicar el dispositivo de ejecucion
  tf$debugging$set_log_device_placement(TRUE)
  
  # Borrar variables
  rm(conda_home, conda_bin, conda_env, conda_lib)
}

tfruns::clean_runs(confirm = FALSE)
tfruns::training_run("model_iris.r")
