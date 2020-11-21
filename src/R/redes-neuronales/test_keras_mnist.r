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
mnist <- keras::dataset_mnist(path = paste0(getwd(), "/large_data/mnist.npz"))

# Definir funcion para hacer plot
plot_imagen <- function(imagen) {
  im <- imagen / 255
  im <- 1 - t(apply(im, 2, rev)) 
  image(x=1:28, y=1:28, z=im, col=gray((0:255)/255), xaxt='n', yaxt='n', xlab='')
}

# Mostrar una imagen
plot_imagen(mnist$train$x[1 , ,])

# Entrenar usando una red neuronal con capas densas solamente (+ la de flattening)
device <- "GPU:0"
with(tf$device(device), {
  # Crear modelo
  model <- keras::keras_model_sequential(name = "Clasificacion_MNIST")
  
  # Agregar neuronas
  model %>% 
    keras::layer_flatten(input_shape = c(28, 28)) %>%
    keras::layer_dense(
    units = 10, # 3 neuronas de salida
    activation = 'softmax', # funcion de activacion softmax
    name = "Salida" # Nombre de la capa
  )
  summary(model)
  
  # Compilar
  model %>% keras::compile(
    loss = "categorical_crossentropy", 
    optimizer = keras::optimizer_sgd(lr = 0.1),
    metrics = c('accuracy', 'Precision', 'Recall')
  )
  
  # Hacer encoding one hot para los valores de y
  y_train <- keras::to_categorical(mnist$train$y, num_classes = 10)
  y_test  <- keras::to_categorical(mnist$test$y, num_classes = 10)
  
  # Estimar pesos
  model %>% keras::fit(x = mnist$train$x / 255, 
                       y = y_train,
                       validation_data = list(mnist$test$x / 255, y_test),
                       epochs = 10)
})

# Entrenar usando una red neuronal convolucional
device <- "GPU:0"
with(tf$device(device), {
  # Obtener datos de train y test e indicar que tiene solamente 1 canal
  # Hacer encoding one-host con los datos a predecir
  x_train <- array_reshape(mnist$train$x, c(dim(mnist$train$x), 1)) / 255
  y_train <- keras::to_categorical(mnist$train$y, num_classes = 10)
  x_test  <- array_reshape(mnist$test$x, c(dim(mnist$test$x), 1)) / 255
  y_test  <- keras::to_categorical(mnist$test$y, num_classes = 10)
  
  # Definir el modelo
  model <- keras_model_sequential() %>%
    keras::layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu',
                         input_shape = dim(x_train)[-1]) %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    keras::layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    keras::layer_flatten() %>%
    keras::layer_dense(units = 10, activation = 'softmax')
  
  # Compile model
  model %>% keras::compile(
    loss = "categorical_crossentropy", 
    optimizer = keras::optimizer_sgd(lr = 0.1),
    metrics = c('accuracy', 'Precision', 'Recall')
  )
  
  # Train model
  model %>% keras::fit(x = x_train, y = y_train, epochs = 10,
                       validation_data = list(x_test, y_test))
})