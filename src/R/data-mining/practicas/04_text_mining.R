# Limpiar ambiente
rm(list = objects())

# Cargar paquetes
require(dplyr)
require(mongolite)
require(proxy)
require(tm)
require(wordcloud)

# Cargar tweets
coleccion <- mongolite::mongo(db = "TextMining", collection = "tweets")
tweets    <- coleccion$find() %>%
  dplyr::select(created_at, screen_name, lng, lat, hashtags, text)
coleccion$disconnect()
rm(coleccion)

# Creacion de corpus
corpus = tm::Corpus(tm::VectorSource(enc2utf8(tweets$text)))

# Limpiamos el texto
corpus.procesado <- corpus %>%
  # Pasamos el texto a minúsculas
  tm::tm_map(tm::content_transformer(tolower)) %>%
  # Removemos números
  tm::tm_map(tm::removeNumbers) %>%
  # Removemos palabras vacias en español
  tm::tm_map(tm::removeWords, tm::stopwords("spanish")) %>%
  # Removemos puntuaciones
  tm::tm_map(tm::removePunctuation) %>%
  # Eliminamos espacios
  tm::tm_map(tm::stripWhitespace)

# Generación de la matriz Texto-Documento
matriz.texto.documento <- tm::TermDocumentMatrix(corpus.procesado)

# Búsqueda de términos más frecuentes
matriz       <- as.matrix(matriz.texto.documento)
estadisticas <- data.frame(termino = rownames(matriz), cantidad = apply(X = matriz, FUN = sum, MARGIN = 1)) %>%
  dplyr::arrange(dplyr::desc(cantidad))

# Construir nube de palabras
wordcloud::wordcloud(words = estadisticas$termino, freq = estadisticas$cantidad, 
                     col = rainbow(n = length(estadisticas$termino), alpha = 0.9), 
                     random.order = FALSE, ordered.colors = TRUE, rot.per = 0.3)

# Escalado multidimensional con los 50 términos más frecuentes
# Distancia del coseno
cantidad.terminos <- 50
topN              <- estadisticas[1:cantidad.terminos, ]
topN.dtm          <- matriz.texto.documento[as.character(topN$termino)[1:cantidad.terminos],]
cosine_dist_mat   <- as.matrix(proxy::dist(as.matrix(topN.dtm), method = "cosine"))
fit               <- stats::cmdscale(cosine_dist_mat, eig=TRUE, k=2) # k is the number of dim

# Grafico
x <- fit$points[,1]
y <- fit$points[,2]
par(bg="white")
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Escalado Multidimensional - Top 50 términos", type="n")
text(x, y, labels = rownames(fit$points), cex=.7, col = c("#e41a1c","#377eb8","#4daf4a","#984ea3")[as.factor(floor(log(topN$cantidad)))]) 
