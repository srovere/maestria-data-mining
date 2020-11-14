# 6 clases (dificil)
Clasificar(
  input.file    = "data/6_clases_dificil.csv",
  clase         = "target",
  capas         = list(list(units = 4, activation = 'sigmoid')),
  learning.rate = 0.3,
  epochs        = 200
)