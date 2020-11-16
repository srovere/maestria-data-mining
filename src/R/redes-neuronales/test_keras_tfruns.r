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
require(tfestimators)
require(tfruns)

# Opcion para graficos
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

Clasificar <- function(input.file, clase, capas, learning.rate, epochs, random.seed = 0, prob.train = 0.7, device = "CPU:0", features.eliminables = NULL, positive = NULL, verbose = FALSE) {
  # Leer y escalar datos
  datos <- readr::read_csv(file = input.file)
  if (! is.null(features.eliminables)) {
    features.seleccionables <- colnames(datos)[which(! colnames(datos) %in% features.eliminables)]
    datos <- datos %>%
      dplyr::select(dplyr::one_of(features.seleccionables))
  }
  
  # Separar features de labels
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
    model %>% keras::fit(x = x.train, y = y.train, validation_data = list(x.test, y.test),
                         epochs = epochs, verbose = verbose)
    
    # Evaluar modelo
    score <- model %>% keras::evaluate(x.test, y.test)
  })
  
  return (model)
}

tfruns::clean_runs(confirm = FALSE)
tfruns::purge_runs(confirm = FALSE)
tfruns::training_run("models/iris.r")
tfruns::training_run("models/diabetes.r")
tfruns::training_run("models/moons.r")
tfruns::training_run("models/circulos.r")
tfruns::training_run("models/2_clases.r")
tfruns::training_run("models/6_clases.r")
tfruns::training_run("models/titanic.r")
