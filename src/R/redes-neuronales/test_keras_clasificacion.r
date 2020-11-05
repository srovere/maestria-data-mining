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
require(caret)
require(dplyr)
require(keras)
require(tensorflow)

# Inicializar tensorflow
tensorflow::use_condaenv(condaenv = conda_env, conda = conda_bin)

# Indicar el dispositivo de ejecucion
tf$debugging$set_log_device_placement(TRUE)

# Ejecutar en dispositivo seleccionado
# device <- "GPU:0" # GPU
device <- "CPU:0" # CPU
with(tf$device(device), {
  # Crear dataset de entrada
  set.seed(0)
  datos   <- iris %>%
    dplyr::mutate(Sepal.Length = scale(Sepal.Length),
                  Sepal.Width = scale(Sepal.Width),
                  Petal.Length = scale(Petal.Length),
                  Petal.Width = scale(Petal.Width))
  train   <- caret::createDataPartition(dplyr::pull(datos, Species), p = 0.7, list = FALSE)
  x.train <- dplyr::select(datos[train,], -Species) %>% as.matrix()
  x.test  <- dplyr::select(datos[-train,], -Species)  %>% as.matrix()
  y.train <- dplyr::select(datos[train,], Species)
  y.test  <- dplyr::select(datos[-train,], Species)
  
  # Crear modelo
  model <- keras::keras_model_sequential(name = "Clasificacion")
  
  # Agregar neuronas
  model %>% keras::layer_dense(
    units = 2, # 2 neuronas para trazar 2 hiperplanos
    input_shape = 4, # 4 datos de entrada
    activation = 'sigmoid', # funcion de activacion sigmoidea
    name = "Oculta" # Nombre de la capa
  ) %>% keras::layer_dense(
    units = 3, # 3 neuronas de salida
    activation = 'softmax', # funcion de activacion softmax
    name = "Salida" # Nombre de la capa
  )
  summary(model)
  
  # Compilar
  model %>% keras::compile(
    loss = "categorical_crossentropy", 
    optimizer = keras::optimizer_sgd(lr = 0.6),
    metrics = c('accuracy', 'Recall')
  )
  
  # Estimar parametros
  one_hot_labels <- keras::to_categorical(as.integer(y.train$Species) - 1, num_classes = 3)
  model %>% keras::fit(x = x.train, y = one_hot_labels, epochs = 100)
  
  # Evaluar modelo
  one_hot_labels <- keras::to_categorical(as.integer(y.test$Species) - 1, num_classes = 3)
  scores <- model %>% keras::evaluate(x.test, one_hot_labels)
})

# Prediccion
prediccion <- predict(model, x.test)
especies   <- levels(datos$Species)
y.cats     <- factor(apply(X = prediccion, MARGIN = 1, FUN = function(x) {
  especies[which(x == max(x))]
}), levels = especies)

# Matriz de confusion
caret::confusionMatrix(data = y.cats, reference = y.test$Species)
