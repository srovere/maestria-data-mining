# ---------------------------------------------------------------------------------------#
# ---- Baseline de TP2 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#
rm(list = objects())

require(dplyr)
require(magrittr)
require(purrr)
require(sf)
require(tidyr)

load("input/PreciosClaros.RData")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- II. Tratamiento de precios ----                            
# ---------------------------------------------------------------------------------------#

# 1. Pasar a formato ancho.
precios.ancho <- precios %>%
  dplyr::select(productoId, comercioId, banderaId, sucursalId, medicion, precio) %>%
  tidyr::spread(key = medicion, value = precio)

# 2. Imputar precios tomando promedios de mediciones contiguas.
# Si las mediciones contiguas son faltantes, el valor queda como faltantes.
matriz.precios <- precios.ancho %>%
  dplyr::select(-productoId, -comercioId, -banderaId, -sucursalId) %>%
  purrr::pmap(
    .l = .,
    .f = function(...) {
      valores.precios <- unlist(list(...))
      valores.lag       <- dplyr::lag(valores.precios)
      valores.lead      <- dplyr::lead(valores.precios)
      promedios         <- (valores.lag + valores.lead) / 2
      valores.imputados <- ifelse(! is.na(valores.precios), valores.precios, promedios) 
      return (valores.imputados)
    }
  ) %>% do.call(what = "rbind", args = .) %>% as.data.frame()

precios.ancho.imputado <- precios.ancho %>%
  dplyr::select(productoId, comercioId, banderaId, sucursalId) %>%
  dplyr::bind_cols(matriz.precios)
  
# 3. Generar precios por periodos (promedio por periodo y promedio total)
#    P1 = M1, M2, M3
#    P2 = M4, M5
#    P3 = M6, M7
#    P4 = M8, M9, M10
periodos <- data.frame(periodo = as.character(c(1, 1, 1, 2, 2, 3, 3, 4, 4, 4)), 
                       medicion = as.character(seq(from = 1, to = 10)))

precios.promedio.periodo <- precios.ancho.imputado %>%
  tidyr::gather(key = medicion, value = precio, -productoId, -comercioId, -banderaId, -sucursalId) %>%
  dplyr::inner_join(periodos, by = c("medicion")) %>%
  dplyr::group_by(productoId, comercioId, banderaId, sucursalId, periodo) %>%
  dplyr::summarise(promedio = mean(precio)) %>%
  dplyr::rename(P = periodo) %>%
  tidyr::spread(key = P, value = promedio, sep = "")

precios.promedio.total <- precios.ancho.imputado %>%
  tidyr::gather(key = medicion, value = precio, -productoId, -comercioId, -banderaId, -sucursalId) %>%
  dplyr::group_by(productoId, comercioId, banderaId, sucursalId) %>%
  dplyr::summarise(PT = mean(precio))

# 4. Integrar y eliminar productos/sucursales con faltantes
precios.completos <- precios.ancho.imputado %>%
  dplyr::inner_join(precios.promedio.periodo, by = c("productoId", "comercioId", "banderaId", "sucursalId")) %>%
  dplyr::inner_join(precios.promedio.total, by = c("productoId", "comercioId", "banderaId", "sucursalId")) %>%
  dplyr::filter(! is.na(PT))

# 5. Calcular variaciones interperiodo y total
# 6. Discretizar variaciones
DiscretizarVariacion <- function(variacion) {
  intervalos <- c(-Inf, -0.1, -0.05, -0.005, 0.005, 0.05, 0.1, Inf)
  categorias <- c("Disminución Fuerte", "Disminución Ḿoderada", "Disminución Leve", "Mantiene", "Aumento Leve", "Aumento Moderado", "Aumento Fuerte")
  return (cut(x = variacion, breaks = intervalos, labels = categorias))
}
precios.variacion <- precios.completos %>%
  dplyr::mutate(V1 = (P2-P1)/P1, V2 = (P3-P2)/P2, V3 = (P4-P3)/P3, VT = (P4-P1)/P1) %>%
  dplyr::mutate(DV1 = DiscretizarVariacion(V1), DV2 = DiscretizarVariacion(V2), DV3 = DiscretizarVariacion(V3),
                DVT = DiscretizarVariacion(VT))
# ----------------------------------------------------------------------------------------