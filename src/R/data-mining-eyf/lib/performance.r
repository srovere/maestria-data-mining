# Set de funciones para calcular performance (ROC AUC, ganancia, etc)

# Ganancia
pe_ganancia = function(probabilidades, clase, proporcion = 1, punto_corte = 0.025) {
  binarias <- as.numeric(clase == "1")
  ganancia <- sum((probabilidades >= punto_corte) * ifelse(binarias == 1, 19500, -500))
  return (ganancia / proporcion)
}

# ROC AUC
pe_auc_roc = function(probabilidades, clase, punto_corte = 0.025) {
  binarias <- as.numeric(clase == "1")
  roc_pred <- ROCR::prediction(probabilidades, binarias, label.ordering=c(0, 1))
  auc_t    <- ROCR::performance(roc_pred, "auc")
  auc      <- unlist(auc_t@y.values)
  return (auc)
}