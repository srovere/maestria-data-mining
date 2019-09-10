# Set de funciones para hacer feature engineering
fe_clase_binaria <- function(clase_ternaria) {
  return (factor(ifelse(clase_ternaria == "BAJA+2", 1, 0)))
}