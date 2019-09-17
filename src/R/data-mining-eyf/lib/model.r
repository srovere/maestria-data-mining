# Set de funciones para definir modelos

# Arbol de decision
m_arbol_decision <- function(set.datos, clase, semilla, parametros) {
  set.seed(semilla)
  modelo <- rpart::rpart(formula = clase ~ ., data = set.datos, 
                         xval = round(parametros$xval), 
                         cp = parametros$cp, 
                         minsplit = round(parametros$ms), 
                         minbucket = round(parametros$mb), 
                         maxdepth = round(parametros$md))
  return (modelo)
}