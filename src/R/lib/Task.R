require(doSNOW)
require(foreach)
require(iterators)
require(purrr)
require(R6)
require(snow)
require(utils)

Task <- R6Class("Task",
  private = list(
    start.time    = NULL,
    end.time      = NULL,
    func.name     = NULL,
    packages      = NULL,
    timeout.value = NULL,
    errors        = NULL
  ),
  public = list(
    initialize = function(func.name, packages = NULL, timeout.value = NULL) {
      if (is.null(func.name) || (class(func.name)[1] != "character")) {
        stop("Nombre de funcion invalida")
      }
      
      private$func.name     <- func.name
      private$packages      <- packages
      private$timeout.value <- timeout.value
      private$errors        <- list()
    },

    getFuncName = function() {
      return (private$func.name)
    },

    getStartTime = function() {
      return (private$start.time)
    },

    getEndTime = function() {
      return (private$end.time)
    },

    getErrors = function() {
      return (private$errors)
    },

    getPackages = function() {
      return (private$packages)
    },

    getTimeoutValue = function() {
      return (private$timeout.value)
    },

    run = function(number.of.processes, input.values, ...) {
      # 1. Crear progress bar y convertir valores de input a iterator en caso de ser un data.frame
      number.of.values <- NULL
      if ("data.frame" %in% class(input.values)) {
        number.of.values <- nrow(input.values)
        input.values     <- iterators::iter(obj = input.values, by = 'row')
      } else if (is.vector(input.values) || is.list(input.values)) {
        number.of.values <- length(input.values)
      } else {
        stop("La variable input.values debe ser un data.frame, lista o vector")
      }
      progressBar <- utils::txtProgressBar(max = number.of.values, style = 3)
      progressBarFunction <- function(n) {
        setTxtProgressBar(progressBar, n)
      }
      snowOptions <- list(progress = progressBarFunction)

      # 2. Extraer funciones del global environment
      functions <- purrr::keep(
        .x = base::ls(envir = base::globalenv()),
        .p = function(obj.name) {
          obj <- base::get(obj.name, envir = base::globalenv())
          return ("function" %in% class(obj))
        }
      )

      # 3. Crear cluster
      log.file <- paste0(private$func.name, ".log")
      cluster  <- snow::makeCluster(type = "SOCK",
                                    spec = rep('localhost', length.out = number.of.processes),
                                    outfile = log.file)
      doSNOW::registerDoSNOW(cluster)

      # 4. Ejecutar procesamiento paralelo e informar resultados
      private$start.time <- Sys.time()
      rvs <- foreach::foreach(input.value = input.values, .packages = private$packages,
                              .options.snow = snowOptions, .errorhandling = 'pass',
                              .export = functions, .verbose = FALSE) %dopar% {
        params    <- list(...)
        child.con <- NULL
        tryCatch({
          # i. Agregar parametros default
          params$input.value <- input.value

          # ii. Ejecutar codigo
          return.value <- NULL
          if (! is.null(private$timeout.value)) {
            R.utils::evalWithTimeout({
              return.value <- do.call(what = private$func.name, args = params, quote = TRUE)
            }, timeout = private$timeout)
          } else {
            return.value <- do.call(what = private$func.name, args = params, quote = TRUE)
          }

          # iii. Devolver resultado
          return (list(input.value = input.value, output.value = return.value, error = NULL))
        }, error = function(e) {
          warning(paste0("Error procesando para objeto ", base::toString(input.value), ". Error: ", e))
          return (list(input.value = input.value, output.value = NULL, error = as.character(e)))
        })
      }
      snow::stopCluster(cluster)
      private$end.time <- Sys.time()

      # 5. Separar resultados de errores. Guardar errores. Devolver resultados.
      results <- purrr::keep(
        .x = rvs,
        .p = function(x) { return (is.null(x$error)) }
      ) %>% purrr::map(
        .f = function(x) { return (x$output.value) }
      )
      private$errors <- purrr::keep(
        .x = rvs,
        .p = function(x) { return (! is.null(x$error)) }
      )
      return (results)
    }
  )
)
