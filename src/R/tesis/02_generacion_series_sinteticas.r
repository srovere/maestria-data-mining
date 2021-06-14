# -----------------------------------------------------------------------------#
# --- PASO 1. Inicializar entorno ----
# -----------------------------------------------------------------------------#

# i. Borrar objetos existentes en el ambiente
rm(list = ls()); gc()

# ii. Configurar huso horario en UTC
Sys.setenv(TZ = "UTC")

# iii. Cargar paquetes a utilizar
list.of.packages <- c("dplyr", "gamwgen", "jsonlite", "magrittr", "purrr", 
                      "tidyr", "sf", "yaml")
for (pack in list.of.packages) {
  if (! require(pack, character.only = TRUE)) {
    stop(paste0("Paquete no encontrado: ", pack))
  }
}
rm(pack); gc()

# iv. Cargar archivo de configuración
config <- yaml::yaml.load_file("configuracion_generacion_series_sinteticas.yml")

# v. Cargar datos de series historicas
load("input/SeriesHistoricas.RData")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Correr ajuste de parámetros ----
# -----------------------------------------------------------------------------#

# i. Definir parametros para el ajuste
control_fit <- gamwgen::local_fit_control(
  prcp_occurrence_threshold = config$fit$control$prcp_occurrence_threshold,
  avbl_cores = config$fit$control$avbl_cores,
  planar_crs_in_metric_coords = config$fit$control$planar_crs_in_metric_coords
)

# ii. Obtener covariables estacionales por estacion para temeperaturas y precipitaciones
seasonal_covariates <- summarise_seasonal_climate(climate, umbral_faltantes = 0.1)

# iii. Realizar ajuste
gamgen_fit <- gamwgen::local_calibrate(
  climate = climate,
  stations = stations,
  seasonal_covariates = seasonal_covariates,
  control = control_fit,
  verbose = FALSE
)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Simular series sinteticas ----
# -----------------------------------------------------------------------------#

# i. Definir parametros para las simulaciones
control_sim <- gamwgen::local_simulation_control(
  nsim = config$sim$control$nsim,
  seed = config$sim$control$seed,
  avbl_cores = config$sim$control$avbl_cores,
  use_spatially_correlated_noise = config$sim$control$use_spatially_correlated_noise,
  use_temporary_files_to_save_ram = config$sim$control$use_temporary_files_to_save_ram,
  remove_temp_files_used_to_save_ram = config$sim$control$remove_temp_files_used_to_save_ram
)

# ii. Completar datos faltantes
filled_covariates <- gamwgen:::fill_missing_seasonal_climate(seasonal_covariates = gamgen_fit$seasonal_covariates)

# iii. Ejecutar simulaciones
simulated_climate <- local_simulation(
  model = gamgen_fit,
  simulation_locations = stations,
  start_date = as.Date(config$sim$start_date),
  end_date = as.Date(config$sim$end_date),
  control = control_sim,
  output_folder = paste0(getwd(), "/output"),
  output_filename = "SeriesSinteticas.csv",
  seasonal_covariates = filled_covariates,
  verbose = TRUE
)
# ------------------------------------------------------------------------------