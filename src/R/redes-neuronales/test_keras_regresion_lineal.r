# Borrar ambiente
rm(list = objects())

# Cargar librerias
require(caret)
require(dplyr)
require(keras)
require(reticulate)
require(stringr)
require(tensorflow)

# Inicializar Tensorflow con ambiente de Anaconda
{
  # Especificar variables de entorno
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
}

# Ejecutar en dispositivo seleccionado
# device <- "GPU:0" # GPU
device <- "CPU:0" # CPU
with(tf$device(device), {
  # Crear dataset de entrada
  set.seed(0)
  x       <- matrix(runif(1000), ncol = 1)
  y       <- 2*x + 5 + rnorm(1000, mean = 0, sd = 1)
  train   <- as.vector(caret::createDataPartition(x, p = 0.7, list = FALSE))
  x.train <- matrix(x[train], ncol = 1)
  x.test  <- matrix(x[-train], ncol = 1)
  y.train <- matrix(y[train], ncol = 1)
  y.test  <- matrix(y[-train], ncol = 1)
  
  # Crear modelo
  model <- keras::keras_model_sequential(name = "Regresion")
  
  # Agregar 1 neurona
  model %>% keras::layer_dense(
    units = 1, 
    input_shape = 1,
    activation = 'linear',
    name = "Salida",
    dtype = "float64"
  )
  summary(model)
  
  # Compilar
  model %>% keras::compile(
    loss = "mse", 
    optimizer = keras::optimizer_sgd(lr = 0.2),
    metrics = c('mean_absolute_error', 'mean_squared_error')
  )
  
  # Estimar parametros
  model %>% keras::fit(x = x.train, y = y.train, epochs = 10, verbose = TRUE)
  
  # Evaluar modelo
  scores <- model %>% keras::evaluate(x.test, y.test)
})

keras::get_weights(model)