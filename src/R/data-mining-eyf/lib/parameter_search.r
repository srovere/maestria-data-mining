# Set de funciones para realizar explortacion de parametros
ps_grid_search_rpart <- function(set.datos, clase, semillas, proporcion_train = 0.7, xval = 0, cp = 0.001, minsplit = 20, minbucket = 7, maxdepth = 30) {
  # i. Crear grilla de combinaciones de parametros a explorar
  grilla.parametros <- purrr::cross_df(list(cp = cp, ms = minsplit, mb = minbucket, md = maxdepth))
  
  # Experimentar para cada semilla
  resultados <- purrr::map_dfr(
    .x = semillas,
    .f = function(semilla, set.datos, grilla.parametros, proporcion_train, xval) {
      # ii. Definir conjuntos de train y test
      set.seed(semilla)
      train_casos <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)
      train       <- set.datos[  train_casos, ]
      test        <- set.datos[ -train_casos, ]
      
      # iii. Definir parametros como "iterables" y barra de progreso
      number.of.values    <- nrow(grilla.parametros)
      input.values        <- iterators::iter(obj = grilla.parametros, by = 'row')
      progressBar         <- utils::txtProgressBar(max = number.of.values, style = 3)
      progressBarFunction <- function(n) {
        setTxtProgressBar(progressBar, n)
      }
      snowOptions         <- list(progress = progressBarFunction)
      
      # iv. Extraer funcione del global environment
      functions <- purrr::keep(
        .x = base::ls(envir = base::globalenv()),
        .p = function(obj.name) {
          obj <- base::get(obj.name, envir = base::globalenv())
          return ("function" %in% class(obj))
        }
      )
      
      # iv. Crear cluster
      cluster <- snow::makeCluster(type = "SOCK", spec = rep('localhost', length.out = parallel::detectCores()))
      doSNOW::registerDoSNOW(cluster)
      
      # v. Ejecutar de forma paralela
      resultados.semilla <- foreach::foreach(input.value = input.values,
                                             .options.snow = snowOptions,
                                             .export = functions,
                                             .errorhandling = 'pass',
                                             .verbose = FALSE) %dopar% {
        modelo <- rpart::rpart(formula = clase ~ ., data = set.datos, 
                               xval = xval, 
                               cp = input.value$cp, 
                               minsplit = input.value$ms, 
                               minbucket = input.value$mb, 
                               maxdepth = input.value$md)
      
        train_prediccion <- as.data.frame(predict(modelo, train, type = "prob"))
        test_prediccion  <- as.data.frame(predict(modelo, test, type = "prob"))
        ganancia_train   <- pe_ganancia(probabilidades = train_prediccion$`1`, clase = train$clase, proporcion = proporcion_train)
        roc_auc_train    <- pe_auc_roc(probabilidades = train_prediccion$`1`, clase = train$clase)
        ganancia_test    <- pe_ganancia(probabilidades = test_prediccion$`1`, clase = test$clase, proporcion = 1 - proporcion_train)
        roc_auc_test     <- pe_auc_roc(probabilidades = test_prediccion$`1`, clase = test$clase)
        return (
          data.frame(semilla = semilla, proporcion_train = proporcion_train, cp = input.value$cp, ms = input.value$ms, 
                     mb = input.value$mb, md = input.value$md, ganancia_train = ganancia_train, roc_auc_train = roc_auc_train, 
                     ganancia_test = ganancia_test, roc_auc_test = roc_auc_test)
        )
      }
      
      warning(resultados.semilla)
      # vi. Finalizar cluster y devolver resultados
      snow::stopCluster(cluster)
      
      return (as.data.frame(data.table::rbindlist(resultados.semilla)))
  }, set.datos = set.datos, grilla.parametros = grilla.parametros, proporcion_train = proporcion_train, xval = xval)
  return (resultados)
}