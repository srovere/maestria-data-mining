# Moons
Clasificar(
  input.file    = "data/moons.csv",
  clase         = "target",
  capas         = list(list(units = 3, activation = 'sigmoid')),
  learning.rate = 0.3,
  epochs        = 100
)
