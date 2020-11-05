# Borrar ambiente
rm(list = objects())

# Carga de paquetes
require(neuralnet)

# Definir datos
datos.train <- data.frame(
  x1 = c(0, 1, 0, 1, 0, 1, 0, 1),
  x2 = c(0, 0, 1, 1, 0, 0, 1, 1),
  x3 = c(0, 0, 0, 0, 1, 1, 1, 1),
  y = seq(from = 0, to = 7)
)

# Entrenar red neuronal
modelo <- neuralnet::neuralnet(formula = y ~ ., data = datos.train, hidden = 0, linear.output = TRUE)
pred   <- predict(modelo, dplyr::select(datos.train, -y))
plot(modelo)