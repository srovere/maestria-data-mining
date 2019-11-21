# Set de funciones para definir modelos

# Arbol de decision
m_arbol_decision <- function(set.datos, clase, semilla, parametros) {
  set.seed(semilla)
  modelo <- rpart::rpart(formula = clase ~ ., data = set.datos, 
                         xval = round(parametros$xval), 
                         cp = parametros$cp, 
                         minsplit = round(parametros$ms), 
                         minbucket = round(parametros$mb), 
                         maxdepth = round(parametros$md))
  return (modelo)
}

# XGBoost
m_xgboost_closure <- function(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss", 
                              tree_method = "hist", grow_policy = "lossguide", nrounds = 500) {
  funcion <- function(set.datos, clase, semilla, parametros) {
    # Uso de funciones objetivos y de evalacion personalizadas
    if (! is.list(parametros)) {
      parametros <- as.list(parametros)  
    }
    
    feval <- NULL
    if (class(eval_metric) == "function") {
      feval = eval_metric
    } else {
      parametros$eval_metric <- eval_metric  
    }
    obj <- NULL
    if (class(objective) == "function") {
      obj = objective
    } else {
      parametros$objective <- objective
    }
    if (! is.null(parametros$max_depth)) {
      parametros$max_depth <- as.integer(round(parametros$max_depth))
    }
    if (! is.null(parametros$max_bin)) {
      parametros$max_bin <- as.integer(round(parametros$max_bin))
    }
    
    parametros$booster     <- booster
    parametros$tree_method <- tree_method
    parametros$nrounds     <- nrounds
    tryCatch({
      xgb.train              <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(set.datos, -!!clase)),
                                                     label = as.matrix(dplyr::select(set.datos, !!clase)))
      
      # Definir semilla y ejecutar modelo
      set.seed(semilla)
      modelo                 <- xgboost::xgb.train(data = xgb.train, nrounds = as.integer(round(parametros$nrounds)),
                                                   obj = obj, feval = feval, params = parametros)
      return (modelo)
    }, error = function(e) {
      archivo.error <- paste0(getwd(), "/xgboost.error.RData")
      save(semilla, parametros, feval, obj, e, file = archivo.error)
      stop("Error al ejecutar XGBoost")
    })
  }
  return (funcion)
}