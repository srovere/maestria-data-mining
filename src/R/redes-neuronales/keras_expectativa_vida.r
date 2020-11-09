# Borrar ambiente
rm(list = objects())

# Cargar librerias
require(caret)
require(dplyr)
require(keras)
require(readr)
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
  
  # Borrar variables
  rm(conda_home, conda_bin, conda_env, conda_lib)
}

# Leer dataset de entrada
datos <- readr::read_csv("data/life_expectancy.csv") %>%
  # Quitar pais
  dplyr::select(-Country) %>%
  # Pasar Status a binario (0 = developing, 1 = developed)
  dplyr::mutate(Status = dplyr::if_else(Status == "Developed", 1, 0))

# ELiminar observaciones con datos faltantes
faltantes <- apply(X = as.matrix(datos), MARGIN = 1, FUN = function(x) {
  return (any(is.na(x)))
}) %>% which()

# Obtener datos de input, pasarlos a matriz y normalizarlos
x.train <- datos %>%
  dplyr::select(-`Life expectancy`) %>%
  as.matrix() %>%
  apply(X = ., MARGIN = 2, FUN = scale)
x.train <- x.train[-faltantes,]

# Obtener datos de output y pasarlos a matrix de Nx1
y.train <- datos %>%
  dplyr::pull(`Life expectancy`) %>%
  as.matrix()
y.train <- y.train[-faltantes,]

# Ejecutar en dispositivo seleccionado
# device <- "GPU:0" # GPU
device <- "CPU:0" # CPU
with(tf$device(device), {
  # Crear modelo
  model <- keras::keras_model_sequential(name = "Regresion")
  
  # Agregar 1 neurona
  model %>% keras::layer_dense(
    units = 1, 
    input_shape = ncol(x.train),
    activation = 'linear',
    name = "Salida",
    dtype = "float64"
  )
  summary(model)
  
  # Compilar
  model %>% keras::compile(
    loss = "mse", 
    optimizer = keras::optimizer_sgd(lr = 0.01),
    metrics = c('mean_absolute_error', 'mean_squared_error')
  )
  
  # Estimar parametros
  model %>% keras::fit(x = x.train, y = y.train, epochs = 20, verbose = TRUE)

  # Obtener pesos y bias
  pesos        <- as.vector(keras::get_weights(model)[[1]])
  names(pesos) <- colnames(x.train)
  bias         <- as.vector(keras::get_weights(model)[[2]])
})

# Entreno otro modelo eliminando algunos atributos
datos.filtrados <- datos %>%
  dplyr::select(-`Adult Mortality`, -`infant deaths`, `under-five deaths`)
x.train <- datos.filtrados %>%
  dplyr::select(-`Life expectancy`) %>%
  as.matrix() %>%
  apply(X = ., MARGIN = 2, FUN = scale)
x.train <- x.train[-faltantes,]
y.train <- datos.filtrados %>%
  dplyr::pull(`Life expectancy`) %>%
  as.matrix()
y.train <- y.train[-faltantes,]
with(tf$device(device), {
  # Crear modelo
  model <- keras::keras_model_sequential(name = "Regresion")
  
  # Agregar 1 neurona
  model %>% keras::layer_dense(
    units = 1, 
    input_shape = ncol(x.train),
    activation = 'linear',
    name = "Salida",
    dtype = "float64"
  )
  summary(model)
  
  # Compilar
  model %>% keras::compile(
    loss = "mse", 
    optimizer = keras::optimizer_sgd(lr = 0.01),
    metrics = c('mean_absolute_error', 'mean_squared_error')
  )
  
  # Estimar parametros
  model %>% keras::fit(x = x.train, y = y.train, epochs = 20, verbose = TRUE)
  
  # Obtener pesos y bias
  pesos        <- as.vector(keras::get_weights(model)[[1]])
  names(pesos) <- colnames(x.train)
  bias         <- as.vector(keras::get_weights(model)[[2]])
})
