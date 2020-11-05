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
require(dplyr)
require(keras)
require(tensorflow)

# Inicializar tensorflow
tensorflow::use_condaenv(condaenv = conda_env, conda = conda_bin)

with(tf$device('GPU:0'), {
  # Crear dataset de entrada
  set.seed(0)
  x <- matrix(runif(1000), ncol = 1)
  y <- 2*x + 1 + rnorm(1000, mean = 0, sd = 1)
  
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
  model %>% keras::compile(loss = "mse", optimizer = keras::optimizer_adam(lr = 0.2))
  summary(model)
  
  # Estimar parametros
  model %>% keras::fit(x = x, y = y, epochs = 1000, verbose = TRUE)
  summary(model)
})

keras::get_weights(model)