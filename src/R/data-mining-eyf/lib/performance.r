# Set de funciones para calcular performance (ROC AUC, ganancia, etc)

# Ganancia
pe_ganancia = function(probabilidades, clase, proporcion = 1, punto_corte = 0.025) {
  binarias <- as.numeric(clase == "1")
  ganancia <- sum((probabilidades >= punto_corte) * ifelse(binarias == 1, 19500, -500))
  return (ganancia / proporcion)
}

pe_maxima_ganancia = function(probabilidades, clase, proporcion = 1, puntos_corte = seq(0.01, 0.1, 0.001)) {
  # Buscar el mejor punto de corte
  clases        <- as.numeric(clase == "1")
  tbl           <- as.data.table(cbind(probabilidades, clases))
  colnames(tbl) <- c("prob", "clase")
  
  # Ordeno la tabla de acuerdo a las probabilidades y calculo el punto de corte
  setorder(tbl, -prob)
  tbl[, ganancia := ifelse(clase == 1, 19500, -500)]
  tbl[, ganacum := cumsum(tbl$ganancia)]
  pos <- which.max(tbl$ganacum)
  vprob_corte <- tbl[pos, prob]
  
  # Luego para evaluar la ganancia toma todos los valores de tbl tales que peso == 1
  ganancia <- sum((preds >= vprob_corte) * ifelse(clases == 1, 19500, -500))
  return (list(punto_corte = vprob_corte, ganancia = ganancia / proporcion))
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

# Funcion de perdida para XGBoost con ganancia unificada
pe_perdida_xgboost_clases_unificadas <- function(preds, dtrain) {
  if (length(preds) == nrow(test)) {
    # Calcular mejor punto de corte
    clases        <- xgboost::getinfo(dtrain, "label")
    tbl           <- as.data.table(cbind(preds, clases))
    colnames(tbl) <- c("prob", "clase")
    
    # Ordeno la tabla de acuerdo a las probabilidades y calculo el punto de corte
    setorder(tbl, -prob )
    tbl[, ganancia := ifelse(clase == 1, 19500, -500)]
    tbl[, ganacum := cumsum(tbl$ganancia)]
    pos <- which.max(tbl$ganacum)
    vprob_corte <- tbl[pos, prob]
    
    # Luego para evaluar la ganancia toma todos los valores de tbl tales que peso == 1
    ganancia <- sum((preds > vprob_corte) * ifelse(clases == 1, 19500, -500))
    
    # Actualizar mejor punto de corte y mejor ganancia
    if (ganancia > mejor.corte$ganancia_mejor) {
      mejor.corte$prob_corte     <<- vprob_corte
      mejor.corte$ganancia_mejor <<- ganancia
    }
    
    return (list(name = "perdida", value = ifelse(is.na(ganancia), 0, -ganancia)))
  } else {
    # Solamente calcular ganancia (o mejor dicho, la perdida como -ganancia)
    return (pe_perdida_xgboost(preds, dtrain))
  }
}