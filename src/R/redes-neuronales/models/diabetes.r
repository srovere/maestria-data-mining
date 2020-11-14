# Clasificacion de dataset "diabetes.csv"
Clasificar(
  input.file    = "data/diabetes.csv",
  clase         = "Clase",
  capas         = list(
    list(units = 64, activation = 'relu'),
    list(units = 32, activation = 'relu')
  ),
  learning.rate = 0.1,
  epochs        = 200,
  positive      = "1"
)