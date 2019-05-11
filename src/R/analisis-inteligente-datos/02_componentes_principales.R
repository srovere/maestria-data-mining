# Inicializar ambiente
require(ggplot2)
require(GGally)
require(MASS)
require(readxl)

# Ejercicio 1. E[X] = (0, 0, 0)
rm(list = objects())
matriz.covarianza <- matrix(c(3, 1, 1, 
                            1, 3, 1,
                            1, 1, 5), ncol = 3)
autovs            <- base::eigen(matriz.covarianza)
autovalores       <- autovs$values
autovectores      <- autovs$vectors
X                 <- c(2, 2, 1)
Y                 <- X %*% autovectores

# Ejercicio 2.
rm(list = objects())
chalets           <- readxl::read_excel(path = "chalets.xlsx")
colnames(chalets) <- c("promotora", "duracion_hipoteca", "precio_medio", "superficie_cocina")
chalets           <- dplyr::select(chalets, duracion_hipoteca, precio_medio, superficie_cocina)

boxplot(chalets)
GGally::ggpairs(data = chalets, columns = 1:ncol(chalets))

medias        <- apply(X = chalets, FUN = mean, MARGIN = 2)
covarianzas   <- cov(x = chalets)
correlaciones <- cor(chalets)
comp.princ    <- stats::princomp(x = chalets, cor = TRUE, scores = TRUE)
loadings1     <- comp.princ$loadings[,1]
prop.var.1    <- (comp.princ$sdev ^ 2)[1] / sum((comp.princ$sdev ^ 2))