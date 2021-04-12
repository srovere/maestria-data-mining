# Borrar ambiente
rm(list = objects())

# Cargar librerias
require(Cairo)
require(caret)
require(dplyr)
require(jpeg)
require(keras)
require(purrr)
require(reticulate)
require(stringr)
require(tensorflow)

# Uso de Cairo para grafico
options(bitmapType = "cairo")

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
  
  # Probar inicializacion
  initialized <- FALSE
  while (! initialized) {
    tryCatch({
      physical_devices = tf$config$experimental$list_physical_devices('GPU')
      tf$config$experimental$set_memory_growth(physical_devices[[1]], TRUE)
      tf$debugging$set_log_device_placement(TRUE) 
      initialized <- TRUE
    }, error = function(e) {
      warning(paste0("Error al inicializar Tensoflow: ", as.character(e), "\n"))
      Sys.sleep(1L)
    })
  }
  
  # Borrar variables
  rm(conda_home, conda_bin, conda_env, conda_lib)
}

# Funcion para cargar imagenes
CargarImagenes <- function(carpeta, clase_negativa = "normal", clase_positiva = "pneumonia", max.imagenes = NA, random.seed = 0L) {
  # Generar listado de archivos a leer y clases
  set.seed(random.seed)
  archivos_neg <- list.files(path = paste0(carpeta, "/", clase_negativa), full.names = TRUE)
  archivos_pos <- list.files(path = paste0(carpeta, "/", clase_positiva), full.names = TRUE)
  archivos     <- c(archivos_neg, archivos_pos)
  etiquetas    <- c(rep(0, length(archivos_neg)), rep(1, length(archivos_pos)))
  reorden      <- sample(seq_along(archivos))
  
  # Leer imagenes
  observaciones <- ifelse(is.na(max.imagenes), length(archivos), max.imagenes)
  imagenes      <- array(data = NA, dim = c(observaciones, 600, 600, 1))
  etiquetas     <- etiquetas[reorden][1:observaciones]
  for (i in seq_len(observaciones)) {
    archivo            <- archivos[reorden[i]] 
    imagenes[i, , , 1] <- t(apply(jpeg::readJPEG(archivo), FUN = rev, MARGIN = 2))
  }
  
  # Devolver datos y etiquetas
  return (list(x = imagenes, y = etiquetas))
}

# Cargar dataset
train <- CargarImagenes(carpeta ="data/pneumonia/train")
test  <- CargarImagenes(carpeta ="data/pneumonia/test")

# Entrenar usando una red neuronal convolucional
device <- "GPU:0"
with(tf$device(device), {
  # Definir el modelo
  set.seed(0)
  model <- keras_model_sequential(name = "Pneumonia") %>%
    keras::layer_conv_2d(filters = 16, kernel_size = c(3,3), padding = 'same', input_shape = dim(train$x)[-1]) %>%
    keras::layer_conv_2d(filters = 16, kernel_size = c(3,3), padding = 'same') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_relu() %>%
    keras::layer_dropout(rate = 0.1) %>%
    keras::layer_conv_2d(filters = 32, kernel_size = c(3,3), padding = 'same') %>%
    keras::layer_conv_2d(filters = 32, kernel_size = c(3,3), padding = 'same') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_relu() %>%
    keras::layer_dropout(rate = 0.1) %>%
    keras::layer_conv_2d(filters = 64, kernel_size = c(3,3), padding = 'same') %>%
    keras::layer_conv_2d(filters = 64, kernel_size = c(3,3), padding = 'same') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_relu() %>%
    keras::layer_dropout(rate = 0.1) %>%
    keras::layer_global_average_pooling_2d() %>%
    keras::layer_dense(units = 1, activation = 'sigmoid')
  summary(model)
  
  # Compile model
  model %>% keras::compile(
    loss = "binary_crossentropy", 
    optimizer = keras::optimizer_sgd(lr = 0.1, momentum = 0.25),
    metrics = c('accuracy', 'Precision', 'Recall')
  )
  
  # Definir el generator
  batch_size      <- 4
  epochs          <- 10
  steps_per_epoch <- ceiling(dim(train$x)[1] / batch_size)
  batch_generator <- function(data, batch_size) {
    max_length <- dim(data$x)[1]
    indice     <- 1
    reorden    <- sample(seq_len(max_length))    
    function() {
      if (indice > max_length) {
        indice  <<- 1
        reorden <<- sample(seq_len(max_length))    
      }
      desde  <- indice
      hasta  <- min(length(reorden), indice + batch_size - 1)
      ids    <- reorden[desde:hasta]
      X      <- array_reshape(data$x[ids,,,], c(dim(data$x[ids,,,]), 1))
      Y      <- data$y[ids]
      #warning(paste0("ML: ", max_length, ". I: ", indice, ". X: (", paste0(dim(X), collapse = ", "), "). Y: (", paste0(dim(Y), collapse = ", "), ")."))
      indice <<- hasta + 1
      list(X, Y)
    }
  }
  generator <- keras:::as_generator.function(batch_generator(train, batch_size))
  
  # Definir callback de early stopping
  early_stopping <- keras::callback_early_stopping(
    patience = epochs %/% 2,
    monitor = "val_loss",
    mode = "min",
    restore_best_weights = TRUE
  )
  
  # Definir callback para checkpoint
  checkpoint <- keras::callback_model_checkpoint(
    filepath = "models/pneumonia.hdf5",
    monitor = "val_loss",
    verbose = 1,
    save_best_only = TRUE,
    mode = "min"
  )
  
  # Train model
  model %>% keras::fit_generator(generator = generator, 
                                 steps_per_epoch = steps_per_epoch,
                                 epochs = epochs,
                                 callbacks = list(early_stopping, checkpoint),
                                 validation_data = list(test$x, test$y))
  
  # Save model
  # keras::save_model_hdf5(model, filepath = "models/pneumonia.hdf5")
})
