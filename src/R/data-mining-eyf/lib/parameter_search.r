# Set de funciones para realizar explortacion de parametros

# Grid search paralelizado
ps_grid_search <- function(set.datos, clase, semillas, proporcion_train = 0.7, funcion_modelo, grilla.parametros) {
  resultados <- purrr::map_dfr(
    .x = semillas,
    .f = function(semilla, set.datos, grilla.parametros, proporcion_train, funcion_modelo) {
      # i. Definir conjuntos de train y test
      set.seed(semilla)
      train_casos <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)
      train       <- set.datos[  train_casos, ]
      test        <- set.datos[ -train_casos, ]
      
      # ii. Definir parametros como "iterables" y barra de progreso
      number.of.values    <- nrow(grilla.parametros)
      input.values        <- iterators::iter(obj = grilla.parametros, by = 'row')
      progressBar         <- utils::txtProgressBar(max = number.of.values, style = 3)
      progressBarFunction <- function(n) {
        setTxtProgressBar(progressBar, n)
      }
      snowOptions         <- list(progress = progressBarFunction)
      
      # iii. Extraer funcione del global environment
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
                                             .packages = c("dplyr", "caret"),
                                             .verbose = FALSE) %dopar% {
        modelo           <- funcion_modelo(set.datos = set.datos, clase = clase, parametros = as.list(input.value))
        train_prediccion <- as.data.frame(predict(modelo, train, type = "prob"))
        test_prediccion  <- as.data.frame(predict(modelo, test, type = "prob"))
        ganancia_train   <- pe_ganancia(probabilidades = train_prediccion$`1`, clase = train$clase, proporcion = proporcion_train)
        roc_auc_train    <- pe_auc_roc(probabilidades = train_prediccion$`1`, clase = train$clase)
        ganancia_test    <- pe_ganancia(probabilidades = test_prediccion$`1`, clase = test$clase, proporcion = 1 - proporcion_train)
        roc_auc_test     <- pe_auc_roc(probabilidades = test_prediccion$`1`, clase = test$clase)
        return (
          input.value %>%
            dplyr::mutate(semilla = semilla, 
                          proporcion_train = proporcion_train,
                          ganancia_train = ganancia_train, 
                          roc_auc_train = roc_auc_train, 
                          ganancia_test = ganancia_test, 
                          roc_auc_test = roc_auc_test)
        )
      }
      
      # vi. Finalizar cluster y devolver resultados
      snow::stopCluster(cluster)
      
      return (as.data.frame(data.table::rbindlist(resultados.semilla)))
  }, set.datos = set.datos, grilla.parametros = grilla.parametros, proporcion_train = proporcion_train, funcion_modelo = funcion_modelo)
  return (resultados)
}

# Optimizacion bayesiana (TODO)
ps_bayesian_optimization <- function(set.datos, clase, semillas, proporcion_train = 0.7, funcion_modelo, limites.parametros,
                                     init_points = 10, n_iter = 20, acq = "ucb", kappa = 2.576, eps = 0.001) {
  # i. Definir closure para funcion objetivo
  funcion_objetivo_closure <- function(funcion_modelo, training.set, test.set, clase, proporcion_train) {
      funcion <- function(...) {
        parametros.modelo <- list(...)
        modelo            <- funcion_modelo(set.datos = training.set, clase = clase, parametros = parametros.modelo)
        test_prediccion   <- as.data.frame(predict(modelo, test.set, type = "prob"))
        ganancia_test     <- pe_ganancia(probabilidades = test_prediccion$`1`, clase = test$clase, proporcion = 1 - proporcion_train)
        roc_auc_test      <- pe_auc_roc(probabilidades = test_prediccion$`1`, clase = test$clase)  
        return (list(Score = ganancia_test, Pred = 0))
      }
      return (funcion)
  }
  
  resultados <- purrr::map_dfr(
    .x = semillas,
    .f = function(semilla, set.datos, limites.parametros, proporcion_train, funcion_objetivo) {
      # ii. Definir conjuntos de train y test
      set.seed(semilla)
      train_casos <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)
      train       <- set.datos[  train_casos, ]
      test        <- set.datos[ -train_casos, ]
      
      # iii. Definir funcion objetivo
      funcion_objetivo <- funcion_objetivo_closure(funcion_modelo, train, test, clase, proporcion_train)
      
      # iv. Efectuar optimizacion bayesiana
      resultados.semilla <- rBayesianOptimization::BayesianOptimization(FUN = funcion_objetivo, bounds = limites.parametros, 
                                                                        verbose = TRUE, init_points = init_points, n_iter = n_iter, 
                                                                        acq = acq, kappa = kappa, eps = eps)
      
      # v. Devolver resultados
      return (
        resultados.semilla %>%
          dplyr::mutate(semilla = semilla, proporcion_train = proporcion_train)
      )
  }, set.datos = set.datos, limites.parametros = limites.parametros, proporcion_train = proporcion_train, 
     funcion_objetivo = funcion_objetivo, init_points = init_points, n_iter = n_iter, acq = acq, kappa = kappa, eps = eps)
  return (resultados)
}