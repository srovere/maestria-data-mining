require(caret)
require(dplyr)
require(keras)
require(purrr)
require(reticulate)
require(stringr)
require(tensorflow)
require(tfestimators)
require(tfruns)

# Parametros
input.file    <- "../data/iris.csv"
clase         <- "name"
capas         <- list(list(units = 2, activation = 'sigmoid'))
learning.rate <- 0.3
random.seed   <- 0
prob.train    <- 0.7
device        <- "CPU:0"
positive      <- NULL
verbose       <- FALSE
epochs        <- 100

# Leer y escalar datos
datos      <- readr::read_csv(file = input.file)
features   <- dplyr::select(datos, -clase) %>%
  as.matrix()
labels     <- dplyr::select(datos, clase) %>%
  as.matrix() %>%
  keras::to_categorical(., num_classes = length(unique(dplyr::pull(datos, clase))))

# Ejecutar en dispositivo seleccionado
with(tf$device(device), {
  # Crear dataset de entrada
  set.seed(random.seed)
  train   <- caret::createDataPartition(dplyr::pull(datos, clase), p = prob.train, list = FALSE)
  medias  <- apply(X = features[train, ], MARGIN = 2, FUN = mean)  
  desvios <- apply(X = features[train, ], MARGIN = 2, FUN = sd)
  x.train <- scale(features[train, ], center = medias, scale = desvios)
  x.test  <- scale(features[-train, ], center = medias, scale = desvios)
  y.train <- labels[train, ]
  y.test  <- labels[-train, ]
  
  # Crear modelo
  model <- keras::keras_model_sequential(name = "Clasificacion")
  
  # Agregar capas ocultas
  input_shape <- ncol(features)
  for (i in seq_along(capas)) {
    model %>% keras::layer_dense(
      units = capas[[i]]$units,
      input_shape = input_shape,
      activation = capas[[i]]$activation,
      name = paste0("Oculta_", i)
    )  
    input_shape = capas[[i]]$units
  }
  
  # Agregar capa de salida
  model %>% keras::layer_dense(
    units = ncol(labels),
    activation = 'softmax',
    name = "Salida"
  )
  
  # Compilar
  model %>% keras::compile(
    loss = "categorical_crossentropy", 
    optimizer = keras::optimizer_sgd(lr = learning.rate),
    metrics = c('accuracy', 'Precision', 'Recall')
  )
  
  # Estimar parametros
  history <- model %>% keras::fit(x = x.train, y = y.train, validation_data = list(x.test, y.test),
                                  epochs = epochs, verbose = verbose)
  
  # Evaluar modelo
  score <- model %>% keras::evaluate(x.test, y.test)
})
