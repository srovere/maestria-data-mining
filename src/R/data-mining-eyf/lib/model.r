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
m_xgboost_closure <- function(booster = "gbtree", objective = "binary:logistic", eval_metric = "mlogloss", tree_method = "hist") {
  funcion <- function(set.datos, clase, semilla, parametros) {
    set.seed(semilla)
    parametros$booster     <- booster
    parametros$objective   <- objective
    parametros$eval_metric <- eval_metric
    parametros$tree_method <- tree_method
    parametros$max_depth   <- as.integer(round(parametros$max_depth))
    xgb.train              <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(set.datos, -!!clase)),
                                                   label = as.matrix(dplyr::select(set.datos, !!clase)))
    modelo                 <- xgboost::xgb.train(data = xgb.train, nrounds = as.integer(round(parametros$nrounds)), 
                                                 nthread = parallel::detectCores(), params = parametros)
    return (modelo)
  }
  return (funcion)
}