# Clasificacion de dataset "titanic.csv"
Clasificar(
  input.file           = "data/titanic.csv",
  clase                = "survived",
  capas                = list(
    list(units = 8, activation = 'relu'),
    list(units = 8, activation = 'relu')
  ),
  learning.rate        =  0.1,
  epochs               = 200,
  features.eliminables = c("id"),
  positive             = "1"
)