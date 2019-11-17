# Set de funciones para realizar explortacion de parametros

# Grid search secuencias
ps_grid_search <- function(set.datos, set.datos.test = NULL, clase, semillas, proporcion_train = 0.7, 
                           funcion_modelo, funcion_prediccion, grilla.parametros, logger) {
  resultados <- purrr::map_dfr(
    .x = semillas,
    .f = function(semilla, set.datos, set.datos.test, grilla.parametros, proporcion_train, funcion_modelo) {
      # i. Definir conjuntos de train y test
      set.seed(semilla)
      proporcion_test <- NULL
      if (is.null(set.datos.test)) {
        train_casos     <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)
        train           <- set.datos[  train_casos, ]
        test            <- set.datos[ -train_casos, ]
        proporcion_test <- 1 - proporcion_train
      } else {
        if (proporcion_train < 1) {                                                                                                                                                                                                     
          train_casos <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)                                                                                                                             
          train       <- set.datos[train_casos, ]                                                                                                                                                                                       
        } else {                                                                                                                                                                                                                        
          train <- set.datos                                                                                                                                                                                                            
        }
        test            <- set.datos.test
        proporcion_test <- 1
      }
      
      # ii. Definir parametros como "iterables" y barra de progreso
      number.of.values    <- nrow(grilla.parametros)
      input.values        <- iterators::iter(obj = grilla.parametros, by = 'row')
      
      # iii. Ejecutar de forma secuencial
      resultados.semilla <- foreach::foreach(input.value = input.values,
                                             .errorhandling = 'pass',
                                             .verbose = FALSE) %do% {
         logger$info(paste0("Ejecutando con parametros: ", paste0(names(as.list(input.value)), "=", as.list(input.value), collapse = ", ")))
         set.seed(semilla)
         modelo          <- funcion_modelo(set.datos = train, clase = clase, semilla = semilla,
                                           parametros = as.list(input.value))
         test_prediccion <- funcion_prediccion(set.datos.test = test, clase = clase, modelo = modelo)
         ganancia_test   <- pe_ganancia(probabilidades = test_prediccion$PBaja, clase = test$clase, 
                                        proporcion = proporcion_test)
         roc_auc_test    <- pe_auc_roc(probabilidades = test_prediccion$PBaja, clase = test$clase)
         return (
           input.value %>%
             dplyr::mutate(semilla = semilla, 
                           ganancia_test = ganancia_test, 
                           roc_auc_test = roc_auc_test)
         )
       }
      
      return (as.data.frame(data.table::rbindlist(resultados.semilla)))
    }, set.datos = set.datos, set.datos.test = set.datos.test, grilla.parametros = grilla.parametros, 
       proporcion_train = proporcion_train, funcion_modelo = funcion_modelo)
  return (resultados)
}

# Grid search paralelizado
ps_parallel_grid_search <- function(set.datos, set.datos.test = NULL, clase, semillas, proporcion_train = 0.7, 
                                    funcion_modelo, funcion_prediccion, grilla.parametros, logger) {
  resultados <- purrr::map_dfr(
    .x = semillas,
    .f = function(semilla, set.datos, set.datos.test, grilla.parametros, proporcion_train, funcion_modelo) {
      # i. Definir conjuntos de train y test
      set.seed(semilla)
      proporcion_test <- NULL
      if (is.null(set.datos.test)) {
        train_casos     <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)
        train           <- set.datos[  train_casos, ]
        test            <- set.datos[ -train_casos, ]
        proporcion_test <- 1 - proporcion_train
      } else {
        if (proporcion_train < 1) {                                                                                                                                                                                                     
          train_casos <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)                                                                                                                             
          train       <- set.datos[train_casos, ]                                                                                                                                                                                       
        } else {                                                                                                                                                                                                                        
          train <- set.datos                                                                                                                                                                                                            
        }
        test            <- set.datos.test
        proporcion_test <- 1
      }
      
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
        logger$info(paste0("Ejecutando con parametros: ", paste0(names(as.list(input.value)), "=", as.list(input.value), collapse = ", ")))                                               
        set.seed(semilla)                                       
        modelo          <- funcion_modelo(set.datos = train, clase = clase, semilla = semilla,
                                          parametros = as.list(input.value))
        test_prediccion <- funcion_prediccion(set.datos.test = test, clase = clase, modelo = modelo)
        ganancia_test   <- pe_ganancia(probabilidades = test_prediccion$PBaja, clase = test$clase, 
                                       proporcion = proporcion_test)
        roc_auc_test    <- pe_auc_roc(probabilidades = test_prediccion$PBaja, clase = test$clase)
        return (
          input.value %>%
            dplyr::mutate(semilla = semilla, 
                          ganancia_test = ganancia_test, 
                          roc_auc_test = roc_auc_test)
        )
      }
      
      # vi. Finalizar cluster y devolver resultados
      snow::stopCluster(cluster)
      
      return (as.data.frame(data.table::rbindlist(resultados.semilla)))
  }, set.datos = set.datos, set.datos.test = set.datos.test, grilla.parametros = grilla.parametros, 
     proporcion_train = proporcion_train, funcion_modelo = funcion_modelo)
  return (resultados)
}

# Optimizacion bayesiana (con mlrMBO)
ps_bayesian_optimization <- function(set.datos, set.datos.test = NULL, clase, semillas, proporcion_train = 0.7, 
                                     funcion_modelo, funcion_prediccion,
                                     limites.parametros, init_points = 50, n_iter = 50, logger,
                                     file_persistence_interval = 600, file_persistence_path = NULL) {
  # i. Definir closure para funcion objetivo
  funcion_objetivo_closure <- function(funcion_modelo, semillas, set.datos, set.datos.test = NULL, clase, proporcion_train) {
    funcion <- function(x) {
      parametros.modelo <- x
      ganancias_test    <- purrr::imap(
        .x = semillas,
        .f = function(semilla, posicion) {
          logger$info(paste0("Ejecutando con parametros: ", paste0(names(parametros.modelo), "=", parametros.modelo, collapse = ", ")))
          set.seed(semilla)
          proporcion_test <- NULL
          if (is.null(set.datos.test)) {
            train_casos     <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)
            train           <- set.datos[  train_casos, ]
            test            <- set.datos[ -train_casos, ]
            proporcion_test <- 1 - proporcion_train
          } else {
            if (proporcion_train < 1) {                                                                                                                                                                                                     
              train_casos <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)                                                                                                                             
              train       <- set.datos[train_casos, ]                                                                                                                                                                                       
            } else {                                                                                                                                                                                                                        
              train <- set.datos                                                                                                                                                                                                            
            }
            test            <- set.datos.test
            proporcion_test <- 1
          }
          
          set.seed(semilla)
          modelo          <- funcion_modelo(set.datos = train, clase = clase, semilla = semilla,
                                            parametros = parametros.modelo)
          test_prediccion <- funcion_prediccion(set.datos.test = test, clase = clase, modelo = modelo)
          ganancia_test   <- pe_ganancia(probabilidades = test_prediccion$PBaja, clase = test$clase, 
                                         proporcion = proporcion_test)
          return (ganancia_test)
        }
      ) %>% unlist()
      return (mean(ganancias_test))
    }
    return (funcion)
  }
  
  # ii. Definir funcion objetivo
  funcion_objetivo <- funcion_objetivo_closure(funcion_modelo, semillas, set.datos, set.datos.test, clase, proporcion_train)
  
  # iii. Efectuar optimizacion bayesiana
  if (! is.null(file_persistence_path) && file.exists(file_persistence_path)) {
    # Retoma el procesamiento en donde lo dejo
    resultados <- mlrMBO::mboContinue(file_persistence_path)
  } else {
    # Creacion de funcion objetivo
    obj.fun <- smoof::makeSingleObjectiveFunction(
      name     = "MBOObjectiveFunction",
      fn       = funcion_objetivo,
      par.set  = limites.parametros,
      minimize = FALSE
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

# Optimizacion de hiperparametros con algoritmos geneticos (con GA)
ps_ga_optimization <- function(set.datos, set.datos.test = NULL, clase, semillas, proporcion_train = 0.7, 
                               funcion_modelo, funcion_prediccion,
                               limites.parametros, parametros_prueba = NULL, max_iterations = 20, 
                               tamano_poblacion = 50, run = 10, logger) {
  # i. Definir closure para funcion objetivo
  funcion_objetivo_closure <- function(funcion_modelo, nombres.parametros, semillas, set.datos, set.datos.test = NULL, clase, proporcion_train) {
    funcion <- function(x) {
      parametros.modelo        <- as.list(x)
      names(parametros.modelo) <- nombres.parametros
      ganancias_test           <- purrr::imap(
        .x = semillas,
        .f = function(semilla, posicion) {
          logger$info(paste0("Ejecutando con parametros: ", paste0(names(parametros.modelo), "=", parametros.modelo, collapse = ", ")))
          set.seed(semilla)
          proporcion_test <- NULL
          if (is.null(set.datos.test)) {
            train_casos     <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)
            train           <- set.datos[  train_casos, ]
            test            <- set.datos[ -train_casos, ]
            proporcion_test <- 1 - proporcion_train
          } else {
            if (proporcion_train < 1) {                                                                                                                                                                                                     
              train_casos <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)                                                                                                                             
              train       <- set.datos[train_casos, ]                                                                                                                                                                                       
            } else {                                                                                                                                                                                                                        
              train <- set.datos                                                                                                                                                                                                            
            }   
            test            <- set.datos.test
            proporcion_test <- 1
          }
          
          set.seed(semilla)
          modelo          <- funcion_modelo(set.datos = train, clase = clase, semilla = semilla,
                                            parametros = parametros.modelo)
          test_prediccion <- funcion_prediccion(set.datos.test = test, clase = clase, modelo = modelo)
          ganancia_test   <- pe_ganancia(probabilidades = test_prediccion$PBaja, clase = test$clase, 
                                         proporcion = proporcion_test)
          return (ganancia_test)
        }
      ) %>% unlist()
      return (mean(ganancias_test))
    }
    return (funcion)
  }
  
  # ii. Definir funcion objetivo
  nombres.parametros <- names(limites.parametros)
  funcion_objetivo   <- funcion_objetivo_closure(funcion_modelo, nombres.parametros, semillas, set.datos,
                                                 set.datos.test, clase, proporcion_train)
  
  # iii. Efectuar optimizacion con GA
  parametros.inferiores <- unlist(purrr::map(.x = names(limites.parametros), ~ limites.parametros[[.x]][1]))
  parametros.superiores <- unlist(purrr::map(.x = names(limites.parametros), ~ limites.parametros[[.x]][2]))
  resultados <- GA::ga(
    type = "real-valued", 
    fitness = funcion_objetivo,
    lower = parametros.inferiores, 
    upper = parametros.superiores, 
    popSize = tamano_poblacion, 
    maxiter = max_iterations,
    run = run
  )
  
  return (resultados)
}

# Seleccion de features con algoritmos geneticos (con GA)
ps_ga_feature_selection <- function(set.datos, set.datos.test = NULL, clase, semillas, proporcion_train = 0.7, 
                                    funcion_modelo, funcion_prediccion, parametros, max_iterations = 20, 
                                    tamano_poblacion = 50, run = 10, logger) {
  # i. Definir closure para funcion objetivo
  funcion_objetivo_closure <- function(funcion_modelo, parametros, nombres.variables, semillas, set.datos, set.datos.test = NULL, clase, proporcion_train) {
    funcion <- function(variables.seleccionadas) {
      # Agregar siempre la clase al set de datos
      variables      <- c(nombres.variables[variables.seleccionadas == 1], clase)
      ganancias_test <- purrr::imap(
        .x = semillas,
        .f = function(semilla, posicion) {
          set.seed(semilla)
          proporcion_test <- NULL
          if (is.null(set.datos.test)) {
            train_casos     <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)
            train           <- set.datos[  train_casos, variables ]
            test            <- set.datos[ -train_casos, variables ]
            proporcion_test <- 1 - proporcion_train
          } else {
            if (proporcion_train < 1) {                                                                                                                                                                                                     
              train_casos <- caret::createDataPartition(set.datos[, clase], p = proporcion_train, list = FALSE)                                                                                                                             
              train       <- set.datos[ train_casos, variables ]                                                                                                                                                                                       
            } else {                                                                                                                                                                                                                        
              train <- set.datos[, variables]                                                                                                                                                                             
            }   
            test            <- set.datos.test[, variables ]
            proporcion_test <- 1
          }
          
          set.seed(semilla)
          modelo          <- funcion_modelo(set.datos = train, clase = clase, semilla = semilla,
                                            parametros = parametros)
          test_prediccion <- funcion_prediccion(set.datos.test = test, clase = clase, modelo = modelo)
          ganancia_test   <- pe_ganancia(probabilidades = test_prediccion$PBaja, clase = test$clase, 
                                         proporcion = proporcion_test)
          return (ganancia_test)
        }
      ) %>% unlist()
      return (mean(ganancias_test))
    }
    return (funcion)
  }
  
  # ii. Definir funcion objetivo
  cantidad.bits.features <- ncol(set.datos) - 1 # Todos los atributos menos la clase
  feature.names          <- setdiff(colnames(set.datos), clase) # Todos los atributos menos la clase
  funcion_objetivo       <- funcion_objetivo_closure(funcion_modelo, parametros, feature.names, semillas, set.datos,
                                                     set.datos.test, clase, proporcion_train)
  
  # iii. Efectuar optimizacion con GA
  resultados <- GA::ga(
    fitness = funcion_objetivo,
    type = "binary", # optimization data type
    crossover = gabin_uCrossover,  # cross-over method
    elitism = 3, # best N indiv. to pass to next iteration
    pmutation = 0.03, # mutation rate prob
    nBits = cantidad.bits.features, # total number of variables
    names = feature.names, # variable name
    keepBest = TRUE, # keep the best solution at the end
    parallel = FALSE, # allow parallel procesing
    popSize = tamano_poblacion, 
    maxiter = max_iterations,
    run = run
  )
  
  return (resultados)
}