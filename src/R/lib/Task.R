require(doSNOW)
require(foreach)
require(iterators)
require(purrr)
require(R6)
require(snow)
require(utils)

Task <- R6Class("Task",
  private = list(
    parent.script = NULL,
    start.time    = NULL,
    end.time      = NULL,
    func.name     = NULL,
    db.config     = NULL,
    facades       = NULL,
    packages      = NULL,
    timeout.value = NULL,
    errors        = NULL
  ),
  public = list(
    initialize = function(parent.script,func.name, db.config = NULL, packages = NULL,
                          facades = NULL, timeout.value = NULL) {
      if (is.null(parent.script) || (class(parent.script)[1] != "Script")) {
        stop("Script padre invalido")
      }
      if (is.null(func.name) || (class(func.name)[1] != "character")) {
        stop("Nombre de funcion invalida")
      }
      if ((is.null(facades) && ! is.null(db.config)) || (! is.null(facades) && is.null(db.config))) {
        stop("Para especificar facades, debe ingresar los datos de acceso a la base de datos")
      }

      private$parent.script <- parent.script
      private$func.name     <- func.name
      private$db.config     <- db.config
      private$packages      <- packages
      private$facades       <- facades
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

    getDBConfig = function() {
      return (private$db.config)
    },

    getPackages = function() {
      return (private$packages)
    },

    getFacades = function() {
      return (private$facades)
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
      log.file <- paste0(private$parent.script$getRunDir(), "/",
                         private$parent.script$getName(), "-",
                         private$func.name, ".log")
      cluster  <- snow::makeCluster(type = "SOCK",
                                    spec = rep('localhost', length.out = number.of.processes),
                                    outfile = log.file)
      doSNOW::registerDoSNOW(cluster)

      # 4. Ejecutar procesamiento paralelo e informar resultados
      private$start.time <- Sys.time()
      facades            <- private$facades
      rvs <- foreach::foreach(input.value = input.values, .packages = private$packages,
                              .options.snow = snowOptions, .errorhandling = 'pass',
                              .export = functions, .verbose = FALSE) %dopar% {
        params    <- list(...)
        child.con <- NULL
        tryCatch({
          # i. Crear conexion a base de datos y hacer setConnection en facades
          if (! is.null(facades)) {
            Sys.setenv(TZ = "UTC")
            child.con <- DBI::dbConnect(drv = private$db.config$driver,
                                        dbname = private$db.config$name,
                                        user = private$db.config$user,
                                        password = private$db.config$pass,
                                        host = private$db.config$host)
            DBI::dbExecute(child.con, "SET TIME ZONE 'UTC'")

            for (facade.name in names(facades)) {
              a.facade <- facades[[facade.name]]
              a.facade$setConnection(child.con)
              params[[facade.name]] <- a.facade
            }
          }

          # ii. Agregar parametros default
          params$script      <- private$parent.script
          params$input.value <- input.value

          # iii. Ejecutar codigo
          return.value <- NULL
          if (! is.null(private$timeout.value)) {
            R.utils::evalWithTimeout({
              return.value <- do.call(what = private$func.name, args = params, quote = TRUE)
            }, timeout = private$timeout)
          } else {
            return.value <- do.call(what = private$func.name, args = params, quote = TRUE)
          }

          # iv. Devolver resultado
          return (list(input.value = input.value, output.value = return.value, error = NULL))
        }, error = function(e) {
          private$parent.script$info(paste0("Error procesando para objeto ", base::toString(input.value), ". Error: ", e))
          return (list(input.value = input.value, output.value = NULL, error = as.character(e)))
        }, finally = {
          if (! is.null(child.con)) {
            DBI::dbDisconnect(child.con)
          }
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
