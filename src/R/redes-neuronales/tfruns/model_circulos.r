# Circulos
Clasificar(
  input.file    = "data/circulos.csv",
  clase         = "target",
  capas         = list(list(units = 4, activation = 'sigmoid')),
  learning.rate = 0.3,
  epochs        = 100
)