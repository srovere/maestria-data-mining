
#Feature Engineering  TODO EN UNO
#paso las fechas abolutas a dias
#creo las variables nuevas en cada mes
#creo TENDENCIA, MAX, MIN, AVERAGE   en funcion de los ultimos 6 meses historia para cada variable, son moviles

#por eficiencia, la parte critica esta escrita en lenguaje C  
#ya que R es insoportablemente lento
#alguna vez se le tenia que poner freno a la demencia R/Python



#source( "~/cloud/cloud1/R/FeatureEngineering/fe_todoenuno.r" )


rm(list=ls())
gc()

library(data.table)
library(dplyr)
library(Rcpp)


kcarpeta_datasetsOri <-  "/devel/maestria-data-mining/src/R/data-mining-eyf/work/extra/"
kcarpeta_datasets    <-  "/devel/maestria-data-mining/src/R/data-mining-eyf/input/extra/"
kcampos_separador    <-  "\t"
kcampo_id            <-  "numero_de_cliente"
kcampo_foto          <-  "foto_mes"
kcampos_no_procesar  <- c( "numero_de_cliente", "foto_mes" )
ventana_regresion    <- 6

#-------------------------------------------------

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 4*n );
  

  for(int i = 0; i < n; i++) 
  {
   
    int  libre    = 0 ;
    int  xvalor   = 1 ;
 
    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;


       if( !R_IsNA( a ) ) 
       {
           y[ libre ]= a ;
       x[ libre ]= xvalor ;
          
       libre++ ;
       }
     
       xvalor++ ;
    }

 
    /* Si hay al menos dos valores */
    if( libre > 1 )
    {   
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ; 
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++) 
      { 
        xsum  += x[h] ; 
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;
 
        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ; 
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else  
    { 
      out[ i       ]  =  NA_REAL ; 
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }

  }


  return out;
}')

archivos.rdata <- list.files(path = kcarpeta_datasetsOri, pattern = "*.RData", full.names = TRUE)
dataset        <- NULL
for (archivo in archivos.rdata) {
  load(file = as.character(archivo))
  # Agregar columna de ranking de probabilidad
  columna_probabilidad <- colnames(resultados.modelo)[3]
  columna_ranking      <- paste0(columna_probabilidad, "_ranking")
  resultados.modelo    <- resultados.modelo %>%
    dplyr::group_by(foto_mes) %>%
    dplyr::mutate(!! columna_ranking := dplyr::percent_rank(!! rlang::sym(columna_probabilidad))) %>%
    dplyr::ungroup()
  
  if (is.null(dataset)) {
    dataset <- resultados.modelo
  } else {
    dataset <- dataset %>%
      dplyr::inner_join(resultados.modelo, by = c("numero_de_cliente", "foto_mes"))
  }
  rm(resultados.modelo, resumen.modelo) 
}

# Promediar probabilidades y transformar dataset en data.table
# Lo de sumar explicitamente (hardcoded) las probabilidades es MUY GRASA, pero a esta altura vale todo
dataset <- dataset %>%
  dplyr::mutate(probabilidad_promedio = (prob_M0_base+prob_M0_denicolay+prob_M0_full+prob_M1_base+prob_M1_denicolay+prob_M1_full)/6) %>%
  as.data.table()

# Ordenar dataset por numero de cliente y periodo
setorder(dataset, numero_de_cliente, foto_mes)

nrow( dataset )
ncol( dataset )
#----------

last <- nrow( dataset )
kcampo_id_idx  <-  match( kcampo_id, names(dataset) )
#----------


#creo el vector_desde que indica cada ventana
#de esta forma se acelera el procesamiento ya que lo hago una sola vez

vector_ids   <- dataset[[  kcampo_id_idx  ]]

vector_desde <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
vector_desde[ 1:ventana_regresion ]  <-  1

for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }
#----------



#agrego al dataset las TENDENCIAS
campos_a_procesar  <- setdiff( names(dataset) ,  kcampos_no_procesar  )  

for(  campo  in  campos_a_procesar )
{
  campo_idx     <-   match( campo,  names(dataset) )
  col_original  <-   dataset[[  campo_idx  ]]
  
  nueva_col     <- fhistC( col_original, vector_desde ) 
  
  #agrego las nuevas columnas al dataset
  dataset[ , paste( campo, "__tend", sep="" ):= nueva_col[ (0*last +1):(1*last) ]  ]
  dataset[ , paste( campo, "__min" , sep="" ):= nueva_col[ (1*last +1):(2*last) ]  ]
  dataset[ , paste( campo, "__max" , sep="" ):= nueva_col[ (2*last +1):(3*last) ]  ]
  dataset[ , paste( campo, "__avg" , sep="" ):= nueva_col[ (3*last +1):(4*last) ]  ]
}


#----------

#grabo el archivo completo
if (! dir.exists(kcarpeta_datasets)) {
  dir.create(kcarpeta_datasets)
}
saveRDS(object = dataset, file = paste0(kcarpeta_datasets, "/Extra2.rds"))