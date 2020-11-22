# Borrar ambiente
rm(list = objects())

# Cargar librerias
require(Cairo)
require(caret)
require(dplyr)
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
  
  # Indicar el dispositivo de ejecucion
  tf$debugging$set_log_device_placement(TRUE)
  
  # Borrar variables
  rm(conda_home, conda_bin, conda_env, conda_lib)
}

# Cargar dataset
cifar10 <- keras::dataset_cifar10()

# Entrenar usando una red neuronal convolucional
device <- "GPU:0"
with(tf$device(device), {
  # Obtener datos de train y test e indicar que tiene solamente 1 canal
  # Hacer encoding one-host con los datos a predecir
  x_train <- cifar10$train$x / 255
  y_train <- keras::to_categorical(cifar10$train$y, num_classes = 10)
  x_test  <- cifar10$test$x / 255
  y_test  <- keras::to_categorical(cifar10$test$y, num_classes = 10)
  
  # Definir el modelo
  model <- keras_model_sequential(name = "CIFAR10") %>%
    keras::layer_conv_2d(filters = 32, kernel_size = c(3,3), padding = 'same', input_shape = dim(x_train)[-1]) %>%
    keras::layer_conv_2d(filters = 32, kernel_size = c(3,3), padding = 'same') %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_relu() %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    keras::layer_dropout(rate = 0.1) %>%
    keras::layer_conv_2d(filters = 64, kernel_size = c(3,3), padding = 'same') %>%
    keras::layer_conv_2d(filters = 64, kernel_size = c(3,3), padding = 'same') %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_relu() %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    keras::layer_dropout(rate = 0.1) %>%
    keras::layer_conv_2d(filters = 128, kernel_size = c(3,3), padding = 'same') %>%
    keras::layer_conv_2d(filters = 128, kernel_size = c(3,3), padding = 'same') %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_relu() %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    keras::layer_dropout(rate = 0.1) %>%
    keras::layer_flatten() %>%
    keras::layer_dense(units = 10) %>%
    keras::layer_dropout(rate = 0.1) %>%
    keras::layer_activation(activation = 'softmax')
  
  # Compile model
  model %>% keras::compile(
    loss = "categorical_crossentropy", 
    optimizer = keras::optimizer_sgd(lr = 0.01, momentum = 0.25),
    metrics = c('accuracy', 'Precision', 'Recall')
  )
  
  # Train model
  model %>% keras::fit(x = x_train, y = y_train, epochs = 50,
                       validation_data = list(x_test, y_test))
  
  # Save model
  keras::save_model_hdf5(model, filepath = "models/cifar10.hdf5")
})
