# Set de funciones para definir modelos

# Arbol de decision
m_arbol_decision <- function(set.datos, clase, parametros) {
  modelo <- rpart::rpart(formula = clase ~ ., data = set.datos, 
                         xval = parametros$xval, 
                         cp = parametros$cp, 
                         minsplit = parametros$ms, 
                         minbucket = parametros$mb, 
                         maxdepth = parametros$md)
  return (modelo)
}