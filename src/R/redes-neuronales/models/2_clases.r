# 2 clases (simple)
Clasificar(
  input.file    = "data/2_clases_simple.csv",
  clase         = "target",
  capas         = list(list(units = 2, activation = 'sigmoid')),
  learning.rate = 0.3,
  epochs        = 100
)