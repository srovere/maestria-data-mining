# Limpiar ambiente
rm(list = objects())

# Cargar paquetes
require(dplyr)
require(rlang)

# Cargar dataset
load("input/LAB05/titanic.raw.rdata")

# Funcion para calcular soporte
Contar    <- function(datos, itemset) {
  for (col.name in names(itemset)) {
    if (! is.null(itemset[[col.name]]) && (length(itemset[[col.name]]) > 0)) {
      field.name <- rlang::sym(col.name)
      if (length(itemset[[col.name]]) > 1) {
        field.values <- itemset[[col.name]]
        datos        <- dplyr::filter(datos, UQ(field.name) %in% field.values)
      } else {
        field.value <- itemset[[col.name]]
        datos       <- dplyr::filter(datos, UQ(field.name) == field.value)
      }
    }
  }
  return (nrow(datos))
}
Unir      <- function(itemset.1, itemset.2) {
  col.names <- union(names(itemset.1), names(itemset.2))
  itemset   <- list()
  for (col.name in col.names) {
    col.values.1        <- itemset.1[[col.name]]
    col.values.2        <- itemset.2[[col.name]]
    itemset[[col.name]] <- union(col.values.1, col.values.2)
  }
  return (itemset)
}
Soporte   <- function(datos, itemset) {
  return (Contar(datos, itemset) / nrow(datos))
}
Confianza <- function(datos, itemset.x, itemset.y) {
  itemset.z <- Unir(itemset.x, itemset.y)
  return (Contar(datos, itemset.z) / Contar(datos, itemset.x))
}

Soporte(titanic.raw, list(Survived = "Yes"))
Soporte(titanic.raw, list(Survived = "Yes", Sex = "Male"))
Soporte(titanic.raw, list(Survived = "Yes", Sex = "Male", Class = "3rd"))
Soporte(titanic.raw, list(Survived = "No", Sex = "Male", Class = "3rd"))

Confianza(titanic.raw, list(Class = "Crew"), list(Survived = "Yes"))
Confianza(titanic.raw, list(Class = "1st"), list(Survived = "Yes"))
Confianza(titanic.raw, list(Class = "Crew"), list(Survived = "No"))
Confianza(titanic.raw, list(Class = "1st"), list(Survived = "No"))
Confianza(titanic.raw, list(Age = "Adult", Sex = "Female"), list(Survived = "Yes"))
Confianza(titanic.raw, list(Age = "Adult", Sex = "Male"), list(Survived = "No"))

# --- Uso de arules
require(arules)

# Carga de dataset
data("Groceries")

# Buscar reglas
reglas <- apriori(Groceries, parameter = list(support=0.01, confidence=0.01, target = "rules"))
