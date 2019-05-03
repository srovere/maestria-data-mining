# ----------------------------------------------------------------------------------
# --- Analisis de datos de IMC
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Inicializacion del entorno ----
# ---------------------------------------------------------------------------------#
# i. Eliminacion de objetos del ambiente
rm(list = objects())

# ii. Carga de librerias
require(dplyr)
require(ggplot2)
require(readxl)
require(tidyr)
# ----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------#
# ---- Lectura de registros de IMC ----
# ---------------------------------------------------------------------------------#
# i. Leer registros diarios
registros.imc <- readxl::read_xlsx(path = paste0("input/IMCinfantil.xlsx")) %>%
  dplyr::mutate(CatPeso = as.factor(CatPeso)) 

# ii. Graficar proporcion por CatPeso
ggplot2::ggplot(data = registros.imc, 
                mapping = ggplot2::aes(x = factor(1), fill = CatPeso)) +
  ggplot2::geom_bar(width = 1) + 
  ggplot2::coord_polar(theta = "y")
# ----------------------------------------------------------------------------------

