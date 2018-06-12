require(R6)
require(futile.logger)

Script <- R6Class("Script",
	private = list(
	  run.dir    = NULL,
	  name       = NULL,
	  start.time = NULL
  ),
	public = list(
		initialize = function(run.dir = NULL, name = NULL, log.level = INFO, create.appender = FALSE) {
			if (is.null(run.dir)) {
			  stop("Carpeta de ejecución indefinida");
			} else if (! dir.exists(run.dir)) {
			  stop("Carpeta de ejecución inexistente");
			}
		  
		  if (is.null(name) || (length(name) == 0)) {
		    stop("Debe indicar un nombre para el script")
		  }
			
		  private$run.dir <- run.dir
		  private$name    <- name
		  
		  # Inicializacion del logger
		  appender <- NULL
		  if (create.appender) {
		    appender <- futile.logger::appender.file(paste0(run.dir, "/", name, ".log"))
		  }
		  layout <- futile.logger::layout.format('[~l] [~t] ~m')
		  futile.logger::flog.logger(name = private$name, threshold = log.level, 
		                             layout = layout, appender = appender)
		},

		getRunDir = function() {
			return (private$run.dir)
		},
		setRunDir = function(run.dir) {
		  private$run.dir <- run.dir
		},
		
		getName = function() {
		  return (private$name)
		},
		setName = function(name) {
		  private$name <- name
		},
		
		getPidFile = function() {
		  return (paste0(private$run.dir, "/", private$name, ".pid"))
		},
		
		getPid = function() {
	    pid.file <- self$getPidFile()
	    if (file.exists(pid.file)) {
	      return (as.integer(readLines(con = pid.file, n = 1, warn = FALSE)))
	    } else {
	      return (NULL)
	    }
	  },
		
		isRunning = function() {
		  return (! is.null(self$getPid()))
		},
		
		assertNotRunning = function() {
		  if (self$isRunning()) {
		    self$error(paste0("Se esperaba que el script ", self$getName(), " no esté ejecutándose, pero actualmente está corriendo con PID ", self$getPid()))
		  }
		},
		
		start = function() {
		  pid <- self$getPid()
		  if (is.null(pid)) {
		    pid.file <- self$getPidFile()
		    pid      <- Sys.getpid()
		    writeLines(text = as.character(pid), con = pid.file)
		    private$start.time <<- Sys.time()
		    self$info(paste0("Iniciando script ", private$name, " con PID ", pid))
		  } else {
		    self$error(paste0("El script actualmente esta corriendo con el PID ", pid))
		  }
		},
		
		stop = function(run.config = NULL) {
		  pid <- self$getPid()
		  if (! is.null(pid)) {
		    pid.file <- self$getPidFile()
		    file.remove(pid.file)
		    self$info(paste0("Finalizando script ", private$name))
		    if (! is.null(run.config)) {
		      self$setRunConfig(run.config)
		    }
		    base::invisible(futile.logger::flog.remove(private$name))
		  } else {
		    self$error("El script no estaba actualmente corriendo")
		  }
		},
		
		getStartTime = function() {
		  return (private$start.time)
		},
		
		getRunConfigFile = function() {
		  return (paste0(private$run.dir, "/", private$name, ".run"))
		},
		
		getRunConfig = function() {
		  config.file <- self$getRunConfigFile()
		  if (file.exists(config.file)) {
		    return (yaml::yaml.load_file(config.file))
		  } else {
		    return (list())
		  }
		},
		
		setRunConfig = function(run.config) {
		  config.file     <- self$getRunConfigFile()
		  run.config.yaml <- yaml::as.yaml(run.config)
		  return (base::write(x = run.config.yaml, file = config.file, ncolumns = 1, append = FALSE))
		},
		
		debug = function(mensaje) {
		  futile.logger::flog.debug(msg = mensaje, name = private$name)
		},
		
		info = function(mensaje) {
		  futile.logger::flog.info(msg = mensaje, name = private$name)
		},
		
		warn = function(mensaje) {
		  futile.logger::flog.warn(msg = mensaje, name = private$name)	  
		},
		
		error = function(mensaje) {
		  futile.logger::flog.error(msg = mensaje, name = private$name)
		  base::stop("Abortando ejecucion")	  
		},
		
		cleanDirectory = function(directory) {
		  for (path in list.files(path = directory, full.names = TRUE)) {
		    base::unlink(x = path, recursive = TRUE)
		  }
		}
	)
)