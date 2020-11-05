# Borrar ambiente
rm(list = objects())

# Indicar LD_LIBRARY_PATH
conda_home <- "/opt/anaconda3"
conda_bin  <- paste0(conda_home, "/bin/conda")
conda_env  <- "r-reticulate"
conda_lib  <- paste0(conda_home, "/envs/", conda_env, "/lib")
Sys.setenv(LD_LIBRARY_PATH=paste0(Sys.getenv("LD_LIBRARY_PATH"), ":", conda_lib))
Sys.getenv("LD_LIBRARY_PATH")

# Cargar libreria
require(keras)
require(tensorflow)

# Inicializar tensorflow
tensorflow::use_condaenv(condaenv = conda_env, conda = conda_bin)
Sys.sleep(5)

# Testear
tf$constant("Hello Tensorflow")
tf_gpu_configured()