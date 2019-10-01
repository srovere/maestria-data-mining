# Set de funciones para realizar explortacion de parametros

# Grid search secuencias
ps_grid_search <- function(set.datos, clase, semillas, proporcion_train = 0.7, funcion_modelo, funcion_prediccion, grilla.parametros) {
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
      
      # iii. Ejecutar de forma secuencial
      resultados.semilla <- foreach::foreach(input.value = input.values,
                                             .errorhandling = 'pass',
                                             .verbose = FALSE) %do% {
                                               modelo          <- funcion_modelo(set.datos = train, clase = clase, semilla = semilla,
                                                                                 parametros = as.list(input.value))
                                               test_prediccion <- funcion_prediccion(set.datos.test = test, clase = clase, modelo = modelo)
                                               ganancia_test   <- pe_ganancia(probabilidades = test_prediccion$PBaja, clase = test$clase, proporcion = 1 - proporcion_train)
                                               roc_auc_test    <- pe_auc_roc(probabilidades = test_prediccion$PBaja, clase = test$clase)
                                               return (
                                                 input.value %>%
                                                   dplyr::mutate(semilla = semilla, 
                                                                 proporcion_train = proporcion_train,
                                                                 ganancia_test = ganancia_test, 
                                                                 roc_auc_test = roc_auc_test)
                                               )
                                             }
      
      return (as.data.frame(data.table::rbindlist(resultados.semilla)))
    }, set.datos = set.datos, grilla.parametros = grilla.parametros, proporcion_train = proporcion_train, funcion_modelo = funcion_modelo)
  return (resultados)
}

# Grid search paralelizado
ps_parallel_grid_search <- function(set.datos, clase, semillas, proporcion_train = 0.7, funcion_modelo, funcion_prediccion, grilla.parametros) {
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
        modelo          <- funcion_modelo(set.datos = train, clase = clase, semilla = semilla,
                                          parametros = as.list(input.value))
        test_prediccion <- funcion_prediccion(set.datos.test = test, clase = clase, modelo = modelo)
        ganancia_test   <- pe_ganancia(probabilidades = test_prediccion$PBaja, clase = test$clase, proporcion = 1 - proporcion_train)
        roc_auc_test    <- pe_auc_roc(probabilidades = test_prediccion$PBaja, clase = test$clase)
        return (
          input.value %>%
            dplyr::mutate(semilla = semilla, 
                          proporcion_train = proporcion_train,
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

# Optimizacion bayesiana (con mlrMBO)
ps_bayesian_optimization <- function(set.datos, clase, semillas, proporcion_train = 0.7, funcion_modelo, funcion_prediccion,
                                     limites.parametros, init_points = 50, n_iter = 50, logger,
                                     file_persistence_interval = 600, file_persistence_path = NULL) {
  # i. Definir closure para funcion objetivo
  funcion_objetivo_closure <- function(funcion_modelo, semillas, training.sets, test.sets, clase, proporcion_train) {
    funcion <- function(x) {
      parametros.modelo <- x
      ganancias_test    <- purrr::imap(
        .x = semillas,
        .f = function(semilla, posicion) {
          logger$info(paste0("Ejecutando con semilla ", semilla, " y parametros: ", 
                             paste0(names(parametros.modelo), "=", parametros.modelo, collapse = ", ")))
          modelo            <- funcion_modelo(set.datos = training.sets[[posicion]], clase = clase, semilla = semilla,
                                              parametros = parametros.modelo)
          test_prediccion   <- funcion_prediccion(set.datos.test = test.sets[[posicion]], clase = clase, modelo = modelo)
          ganancia_test     <- pe_ganancia(probabilidades = test_prediccion$PBaja, clase = test.sets[[posicion]]$clase, 
                                           proporcion = 1 - proporcion_train)
          return (ganancia_test)
        }
      ) %>% unlist()
      return (mean(ganancias_test))
    }
    return (funcion)
  }
  
  # ii. Definir sets de datos para cada semilla
  training.sets <- list()
  test.sets     <- list()
  purrr::iwalk(
    .x = semillas,
    .f = function(semilla, posicion) {
      # ii. Definir conjuntos de train y test
      set.seed(semilla)
      train_casos               <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)
      training.sets[[posicion]] <<- set.datos[  train_casos, ]
      test.sets[[posicion]]     <<- set.datos[ -train_casos, ]
    }
  )
  
  # iii. Definir funcion objetivo
  funcion_objetivo <- funcion_objetivo_closure(funcion_modelo, semillas, training.sets, test.sets, clase, proporcion_train)
  
  # iv. Efectuar optimizacion bayesiana
  if (! is.null(file_persistence_path) && file.exists(file_persistence_path)) {
    # Retoma el procesamiento en donde lo dejo
    resultados <- mlrMBO::mboContinue(file_persistence_path)
  } else {
    # Creacion de funcion objetivo
    obj.fun <- smoof::makeSingleObjectiveFunction(
      name    = "MBOObjectiveFunction",
      fn      = funcion_objetivo,
      par.set = limites.parametros
    )
    
    # Definir initial design
    design   <- ParamHelpers::generateDesign(n = init_points, par.set = getParamSet(obj.fun), fun = lhs::randomLHS)
    
    # Definir funcion de control
    ctrl <- mlrMBO::makeMBOControl(save.on.disk.at.time = file_persistence_interval, 
                                   save.file.path = file_persistence_path)
    ctrl <- mlrMBO::setMBOControlTermination(ctrl, iters = n_iter)
    
    # Ejecutar
    resultados <- mlrMBO::mbo(fun = obj.fun, 
                              design = design, 
                              control = ctrl, 
                              show.info = TRUE)
  }
  
  return (resultados)
}