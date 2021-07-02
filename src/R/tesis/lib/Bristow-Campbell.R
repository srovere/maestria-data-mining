### Estima la radiación solar diaria en MJ/m² tomando como parámetro las mediciones de las
### variables requeridas por el método.
### Si no se nos pasa la temperatura mínima del día siguiente se calculará asumiendo que la
### secuencia de observaciones de T. Mín. es consecutiva, cometiendo ciero grado de error en
### caso de no ser así.
estimate.bristowcampbell <-  function(tmax, tmin, extrat, bc.coef, tmin.next = NULL) {
    data <- .df_bristowcampbell(tmax, tmin, tmin.next, extrat)

    # Estimamos la radiación diaria en MJ/m²/día.
    bc <- data$extrat * bc.coef[['A']] * (1 - exp(-bc.coef[['B']] * (data$dTemp^bc.coef[['C']])))
    return(bc)
}

### Estima la radiación solar diaria en MJ/m² tomando como parámetro una serie XTS con los campos
### requeridos por el método.
### Además, permite especificar qué días de dicha serie utilizar para estimar.
estimate.bristowcampbell.xts <- function(xtsdata, bc.coef, days = NULL, fieldnames=c('tmax', 'tmin', 'extrat')) {
    data <- .xts_bristowcampbell(xtsdata, days, fieldnames)

    # Extraemos los campos de la serie XTS.
    Tmax <- data[, fieldnames[1]]
    Tmin <- data[, fieldnames[2]]
    extraT <- data[, fieldnames[3]]
    tmin.next <- data[, 'tmin.next']

    return(estimate.bristowcampbell(Tmax, Tmin, extraT, bc.coef, tmin.next))
}


### Realiza la calibración de Bristow-Campbell a partir de 4 ó 5 vectores con las variables
### meteorológicas que dicho método requiere.
### Si no se nos pasa la temperatura mínima del día siguiente se calculará asumiendo que la
### secuencia de observaciones de T. Mín. es consecutiva, cometiendo ciero grado de error en
### caso de no ser así.
calibrate.bristowcampbell <- function(tmax, tmin, extrat, solar.rad, tmin.next = NULL) {
    data <- .df_bristowcampbell(tmax, tmin, tmin.next, extrat, solar.rad)

    # Calibramos el modelo.
    BCcal <- robustbase::nlrob((solar.rad/extrat) ~ A * (1 - exp((-B * (dTemp^C)))), data,  start=list(A=0.69, B=0.02, C = 2),
                 control = list(maxiter = 500), na.action=na.exclude)
    return(c(coef(BCcal)))
}


### Realiza la calibración del método de Bristow-Campbell a partir de un objeto XTS.
### Recibe además los días de dicha secuencia que se deben usar para calibrar y los nombres
### de las columnas que corresponden a cada variable requerida.
### Tiene la ventaja de que calcula la temperatura mínima del día siguiente de manera certera.
calibrate.bristowcampbell.xts <- function(xtsdata, days = NULL, fieldnames=c('tmax', 'tmin', 'extrat', 'solar.rad')) {
    data <- .xts_bristowcampbell(xtsdata, days, fieldnames)

    # Extraemos los campos de la serie XTS.
    Tmax <- data[, fieldnames[1]]
    Tmin <- data[, fieldnames[2]]
    rad <- data[, fieldnames[4]]
    extraT <- data[, fieldnames[3]]
    tmin.next <- data[, 'tmin.next']

    return(calibrate.bristowcampbell(Tmax, Tmin, extraT, rad, tmin.next))
}


### Extrae los datos necesarios de la serie XTS y los devuelve como un dataframe.
### 'Laggea' la serie XTS para calcular la T. Mínima del día siguiente.
.xts_bristowcampbell <- function(xtsdata, days, fieldnames) {
    # Si no nos pasan una lista de días, tomamos todos los de la serie XTS.
    if(is.null(days))
        days <- index(xtsdata)

    # Obtenemos la temperatura mínima del día siguiente.
    TminNext <- xts::lag.xts(xtsdata, k=-1)[, fieldnames[2]]
    colnames(TminNext) <- c('tmin.next')
    data <- xts::cbind.xts(xtsdata, TminNext)

    # Reemplazamos los valores NA de temperatura del día siguiente con el valor de la medición
    # del mismo día.
    data$tmin.next[is.na(data$tmin.next)] <- data[is.na(data$tmin.next), fieldnames[2]]

    # Filtramos los días que hay que usar para calibrar.
    data <- data[zoo::index(data) %in% days,]

    # Convertimos los datos de la serie a un data frame.
    data <- data.frame(zoo::coredata(data[, c(fieldnames, 'tmin.next')]), stringsAsFactors=FALSE)

    return(data)
}


### Acomoda los campos que necesita la función de BC en un data frame.
### Calcula además la diferencia de temperatura y la agrega como una columna.
.df_bristowcampbell <- function(tmax, tmin, tmin.next, extrat, solar.rad = NULL) {
    # Si no nos pasan el vector de temperatura mínima del día siguiente, lo construimos
    # asumiendo que no hay días faltantes.
    if(is.null(tmin.next))
        tmin.next <- c(tmin[-1], tmin[length(tmin)])

    # Armamos un data frame para tener alineados en una misma fila todas las variables.
    data <- data.frame(tmax=as.numeric(as.character(tmax)), tmin=as.numeric(as.character(tmin)),
                       tmin.next=as.numeric(as.character(tmin.next)),
                       extrat=as.numeric(as.character(extrat)))

    if(!is.null(solar.rad))
        data <- cbind(data, solar.rad)

    # Calculamos la diferencia de temperatura de cada día.
    dTemp <- data$tmax - (data$tmin + data$tmin.next) / 2

    data <- cbind(data, dTemp)
    return(data)
}