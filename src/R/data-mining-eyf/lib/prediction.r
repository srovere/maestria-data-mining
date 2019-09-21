# Set de funciones para prediccion

# Funcion de prediccion basica (sin transformacion de set de datos de test)
pr_basica <- function(set.datos.test, clase, modelo) {
  prediccion <- as.data.frame(predict(modelo, set.datos.test, type = "prob"))
  return (data.frame(PBaja = prediccion$`1`))
}

# Funcion de prediccion para XGBoost
pr_xgboost <- function(set.datos.test, clase, modelo) {
  xgb.test <- xgboost::xgb.DMatrix(data = as.matrix(dplyr::select(set.datos.test, -!!clase)),
                                   label = as.matrix(dplyr::select(set.datos.test, !!clase)))
  return (data.frame("PBaja" = predict(modelo, xgb.test, reshape=T)))
}