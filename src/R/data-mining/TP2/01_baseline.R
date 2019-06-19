# ---------------------------------------------------------------------------------------#
# ---- Baseline de TP2 ----                            
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# ---- I. Inicialización de ambiente ----                            
# ---------------------------------------------------------------------------------------#

# Borrar variables de ambiente
rm(list = objects())

# Cargar paquetes
require(dplyr)
require(httr)
require(jsonlite)
require(magrittr)
require(purrr)
require(sf)
require(stringr)
require(tidyr)
require(tm)
require(yaml)

# Cargar archivo de parametros
parametros <- yaml::yaml.load_file("parametros.yml")

# Cargar datos de entrada
load("input/PreciosClaros.RData")

# Definir funcion de discretizacion
Discretizar <- function(variacion, intervalos, etiquetas) {
  intervalos.completos <- c(-Inf, intervalos, Inf)
  return (cut(x = variacion, breaks = intervalos.completos, labels = etiquetas))
}
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
      
      # Mejora para primer precio y ultimo precio
      n <- length(valores.precios)
      if (is.na(valores.precios[1]) && ! is.na(valores.precios[2]) && ! is.na(valores.precios[3])) {
        # Con la misma logica de arriba: v2 = (v3 + v1) / 2,
        # se deduce que => v1 = 2 * v2 - v3
        valores.imputados[1] <- 2 * valores.precios[2] - valores.precios[3]
      }
      if (is.na(valores.precios[n]) && ! is.na(valores.precios[n-1]) && ! is.na(valores.precios[n-2])) {
        # Con la misma logica de arriba: vN-1 = (vN + vN-2) / 2,
        # se deduce que => vN = 2 * vN-1 - vN-2
        valores.imputados[n] <- 2 * valores.precios[n-1] - valores.precios[n-2]
      }
      
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
precios.variacion <- precios.completos %>%
  dplyr::mutate(V1 = (P2-P1)/P1, V2 = (P3-P2)/P2, V3 = (P4-P3)/P3, VT = (P4-P1)/P1) %>%
  dplyr::mutate(DV1 = Discretizar(V1, parametros$variacion.precios.periodo$intervalos, parametros$variacion.precios.periodo$etiquetas), 
                DV2 = Discretizar(V2, parametros$variacion.precios.periodo$intervalos, parametros$variacion.precios.periodo$etiquetas), 
                DV3 = Discretizar(V3, parametros$variacion.precios.periodo$intervalos, parametros$variacion.precios.periodo$etiquetas),
                DVT = Discretizar(VT, parametros$variacion.precios.total$intervalos, parametros$variacion.precios.total$etiquetas))
grafico.variacion <- ggplot2::ggplot(data = dplyr::select(precios.variacion, V1, V2, V3) %>% tidyr::gather(key = Periodo, value = Variacion)) +
  ggplot2::geom_boxplot(mapping = ggplot2::aes(x = Periodo, y = Variacion))

# 7. Calcular precios promedio por producto para cada periodo y para el periodo total
precios.productos.periodo <- precios.completos %>%
  dplyr::select(-comercioId, -banderaId, -sucursalId, -P1, -P2, -P3, -P4, -PT) %>%
  tidyr::gather(key = medicion, value = precio, -productoId) %>%
  dplyr::inner_join(periodos, by = c("medicion")) %>%
  dplyr::select(-medicion)
precios.promedio.producto.periodo <- precios.productos.periodo %>%
  dplyr::mutate(periodo = paste0('PP', periodo)) %>%
  dplyr::group_by(productoId, periodo) %>%
  dplyr::summarise(PP = mean(precio)) %>%
  tidyr::spread(key = periodo, value = PP)
precios.promedio.producto <- precios.productos.periodo %>%
  dplyr::group_by(productoId) %>%
  dplyr::summarise(PPT = mean(precio)) %>%
  dplyr::inner_join(precios.promedio.producto.periodo, by = c("productoId"))

# 8. Tomando los precios promedio por producto, calcular el precio relativo por producto/sucursal 
#    para todos los periodos y para el periodo total. Discretizarlos.
precios.relativos <- precios.variacion %>%
  dplyr::inner_join(precios.promedio.producto, by = c("productoId")) %>%
  dplyr::mutate(PR1 = (P1 - PP1)/PP1, PR2 = (P2 - PP2)/PP2, PR3 = (P3 - PP3)/PP3,  
                PR4 = (P4 - PP4)/PP4, PRT = (PT - PPT)/PPT) %>%
  dplyr::mutate(DPR1 = Discretizar(PR1, parametros$nivel.precios$intervalos, parametros$nivel.precios$etiquetas), 
                DPR2 = Discretizar(PR2, parametros$nivel.precios$intervalos, parametros$nivel.precios$etiquetas),
                DPR3 = Discretizar(PR3, parametros$nivel.precios$intervalos, parametros$nivel.precios$etiquetas), 
                DPR4 = Discretizar(PR4, parametros$nivel.precios$intervalos, parametros$nivel.precios$etiquetas),
                DPRT = Discretizar(PRT, parametros$nivel.precios$intervalos, parametros$nivel.precios$etiquetas))

# 9. Seleccionar las siguientes variables:
#    (productoId, comercioId, banderaId, sucursalId, DPR1, DPR2, DPR3, DPR4, DPRT, DV1, DV2, DV3, DVT)
precios.asociacion <- precios.relativos %>%
  dplyr::select(productoId, comercioId, banderaId, sucursalId, DPR1, DPR2, DPR3, DPR4, DPRT, DV1, DV2, DV3, DVT)
rm(precios.ancho, matriz.precios, precios.ancho.imputado, precios.promedio.periodo, precios.promedio.total, precios.completos,
   precios.variacion, precios.productos.periodo, precios.promedio.producto.periodo, precios.promedio.producto, precios.relativos)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- III. Tratamiento de productos ----                            
# ---------------------------------------------------------------------------------------#

productos.asociacion <- productos %>%
  # 1/2. Generar copias de campos textuales en minusculas
  dplyr::mutate(nombre_asoc = base::tolower(nombre), marca_asoc = base::tolower(marca), 
                presentacion_asoc = base::tolower(presentacion)) %>%
  # 3. Quitar numeros
  dplyr::mutate(nombre_asoc = tm::removeNumbers(nombre_asoc),
                marca_asoc = tm::removeNumbers(marca_asoc),
                presentacion_asoc = tm::removeNumbers(presentacion_asoc)) %>%
  # 4. Quitar puntuacion
  dplyr::mutate(nombre_asoc = tm::removePunctuation(nombre_asoc),
                marca_asoc = tm::removePunctuation(marca_asoc),
                presentacion_asoc = tm::removePunctuation(presentacion_asoc)) %>%
  # 5. Quitar tildes
  dplyr::mutate(nombre_asoc = iconv(nombre_asoc, from="UTF-8", to="ASCII//TRANSLIT"),
                marca_asoc = iconv(marca_asoc, from="UTF-8", to="ASCII//TRANSLIT"),
                presentacion_asoc = iconv(presentacion_asoc, from="UTF-8", to="ASCII//TRANSLIT")) %>%
  # 6. Borrar espacios en marca y presentacion
  dplyr::mutate(marca_asoc = trimws(tm::stripWhitespace(marca_asoc)),
                presentacion_asoc = trimws(tm::stripWhitespace(presentacion_asoc)))

presentaciones <- unique(dplyr::pull(productos.asociacion, presentacion_asoc))
marcas         <- unique(dplyr::pull(productos.asociacion, marca_asoc))

# 7. Eliminar unidades de presentacion del nombre
productos.asociacion %<>%
  dplyr::mutate(nombre_asoc = tm::removeWords(nombre_asoc, words = presentaciones)) %>%
  # 8. Eliminar marcas del nombre
  dplyr::mutate(nombre_asoc = tm::removeWords(nombre_asoc, words = marcas)) %>%
  # 9. Eliminar palabras vacias (stopwords) y hacer trim y strip
  dplyr::mutate(nombre_asoc = trimws(tm::stripWhitespace(tm::removeWords(nombre_asoc, words = tm::stopwords("es")))),
                marca_asoc = trimws(tm::stripWhitespace(tm::removeWords(marca_asoc, words = tm::stopwords("es")))),
                presentacion_asoc = trimws(tm::stripWhitespace(tm::removeWords(presentacion_asoc, words = tm::stopwords("es")))))

# 10. Generar un vocaculario con los nombres de los productos. Nos quedamos
#     con las palabras que aparecen 4 veces o mas (percentil 75).
palabras.nombres         <- stringr::str_split(dplyr::pull(productos.asociacion, nombre_asoc), " ")
tabla.palabras           <- as.data.frame(table(unlist(palabras.nombres)))
colnames(tabla.palabras) <- c("palabra", "frecuencia")
umbral.frecuencia        <- quantile(tabla.palabras$frecuencia, 0.75)
vocabulario              <- tabla.palabras %>%
  dplyr::mutate(palabra = as.character(palabra)) %>%
  dplyr::filter(frecuencia >= umbral.frecuencia) %>%
  dplyr::pull(palabra) %>%
  sort()

# 11. Generar matriz de presencia/ausencia para cada producto
matriz.presencia.ausencia <- purrr::imap(
  .x = palabras.nombres,
  .f = function(palabras.producto, seq_index) {
    existencia <- ifelse(vocabulario %in% palabras.producto, 'S', NA)
  }
) %>% do.call(what = "rbind", args = .)
colnames(matriz.presencia.ausencia) <- paste0("termino_", vocabulario)
rownames(matriz.presencia.ausencia) <- dplyr::pull(productos.asociacion, productoId)

rm(presentaciones, marcas, palabras.nombres, umbral.frecuencia)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- IV. Cotizacion del dolar ----                            
# ---------------------------------------------------------------------------------------#
BuscarCotizacion <- function(url, token) {
  request <- httr::GET(url = url, httr::add_headers(Authorization = paste0("BEARER ", token)))
  return (jsonlite::fromJSON(httr::content(request, as = "text")))
}

cotizacion.mayorista <- BuscarCotizacion(parametros$cotizacion$url.mayorista, parametros$cotizacion$token) %>%
  dplyr::rename(fecha = d, mayorista = v)
cotizacion.minorista <- BuscarCotizacion(parametros$cotizacion$url.minorista, parametros$cotizacion$token) %>%
  dplyr::rename(fecha = d, minorista = v)
cotizacion           <- dplyr::full_join(cotizacion.mayorista, cotizacion.minorista, by = c("fecha")) %>%
  dplyr::mutate(fecha = as.Date(fecha)) %>%
  dplyr::arrange(fecha) %>%
  dplyr::as.tbl()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---- IV. Almacenar variables resultantes de utilidad ----                            
# ---------------------------------------------------------------------------------------#
save(precios.asociacion, productos.asociacion, tabla.palabras, vocabulario, matriz.presencia.ausencia,
     cotizacion, periodos, file = "input/ReglasAsociacion.RData")
# ----------------------------------------------------------------------------------------