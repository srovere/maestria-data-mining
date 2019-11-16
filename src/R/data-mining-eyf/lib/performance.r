# Set de funciones para calcular performance (ROC AUC, ganancia, etc)

# Ganancia
pe_ganancia = function(probabilidades, clase, proporcion = 1, punto_corte = 0.025) {
  binarias <- as.numeric(clase == "1")
  ganancia <- sum((probabilidades >= punto_corte) * ifelse(binarias == 1, 19500, -500))
  return (ganancia / proporcion)
}

pe_maxima_ganancia = function(probabilidades, clase, proporcion = 1, puntos_corte = seq(0.01, 0.1, 0.001)) {
  # Buscar el mejor punto de corte
  mejor.corte     <- NA
  maxima.ganancia <- NA
  for (punto_corte in puntos_corte) {
    ganancia <- pe_ganancia(probabilidades, clase, proporcion, punto_corte)
    if (is.na(maxima.ganancia) || (ganancia > maxima.ganancia)) {
      maxima.ganancia <- ganancia
      mejor.corte     <- punto_corte
    }
  }
    
  return (list(punto_corte = mejor.corte, ganancia = maxima.ganancia / proporcion))
}

# ROC AUC
pe_auc_roc = function(probabilidades, clase, punto_corte = 0.025) {
  binarias <- as.numeric(clase == "1")
  roc_pred <- ROCR::prediction(probabilidades, binarias, label.ordering=c(0, 1))
  auc_t    <- ROCR::performance(roc_pred, "auc")
  auc      <- unlist(auc_t@y.values)
  return (auc)
}

# Funcion de perdida para XGBoost
pe_perdida_xgboost <- function(preds, dtrain) {
  clases  <- xgboost::getinfo(dtrain, "label")
  perdida <- -pe_ganancia(preds, clases, proporcion = 1, punto_corte = 0.025)
  return(list(metric = "perdida", value = perdida))
}