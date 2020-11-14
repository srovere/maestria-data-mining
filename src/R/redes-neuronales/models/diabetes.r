# Clasificacion de dataset "diabetes.csv"
Clasificar(
  input.file    = "data/diabetes.csv",
  clase         = "Clase",
  capas         = list(
    list(units = 10, activation = 'tanh'),
    list(units = 10, activation = 'relu'),
    list(units = 10, activation = 'tanh')
  ),
  learning.rate = 0.25,
  epochs        = 100,
  positive      = "1"
)