########################################################################################################################
# 1. Pasos preparatorios
########################################################################################################################

# Carga de librerías
import numpy
import pandas
#import pydotplus
import random
import sklearn.externals.six
import sklearn.metrics
import sklearn.model_selection
import sklearn.naive_bayes
import sklearn.tree

# Definicion de funcion para discretizar variables por cuantiles
def DiscretizarPorCuantiles(datos, columnas, intervalos):
    datosNuevos = datos.copy()
    informacion = {}
    for columna in columnas:
        paso      = 1 / intervalos
        cuantiles = numpy.arange(0, 1.0 + paso, paso)
        etiquetas = list(map(str, numpy.arange(1, intervalos + 1)))
        
        (nuevosDatos, intervalosEtiquetas) = pandas.qcut(x=datos[columna], q=cuantiles, labels=etiquetas, retbins=True)
        datosNuevos[columna] = nuevosDatos
        informacion[columna] = { "cuantiles": cuantiles, "etiquetas": etiquetas, "intervalos": intervalosEtiquetas }
    return [datosNuevos, informacion ]

# Lectura de set de datos discretizado
setDatosOriginal = pandas.read_csv(filepath_or_buffer = "input/SetDatosDiscretizado.csv", sep = "\t",
                                       na_values = "NA")
setDatosDiscretizado, informacion = DiscretizarPorCuantiles(datos=setDatosOriginal, columnas=['age','height','weight'], intervalos=10)

# Separo los features del data frame del target a aprender
atributos = setDatosDiscretizado.drop(columns = [ "cardio" ])
objetivo  = setDatosDiscretizado['cardio']

# Separar en sets de desarrollo (entrenamiento + validacion) y testeo
atributosDesarrollo, atributosTest, objetivoDesarrollo, objetivoTest = \
    sklearn.model_selection.train_test_split(atributos, objetivo, train_size = 0.8, test_size = 0.2, random_state = 0)
    
    # Separar el set de desarrollo en sets de entrenamiento y validacion
atributosEntrenamiento, atributosValidacion, objetivoEntrenamiento, objetivoValidacion = \
    sklearn.model_selection.train_test_split(atributosDesarrollo, objetivoDesarrollo,
                                             train_size = 0.8, test_size = 0.2, random_state = 0)

########################################################################################################################
# 2. Árboles de decisión
########################################################################################################################

# 2.1 Entrenamiento de árbol con altura 3 y estimacion de performance con 5-fold cross validation

def EntrenarYEvaluarPerformance(atributos, objetivo, parametros, metricas, metrica_mejor_ajuste, k=5):
    # Eliminar id a efectos de entrerar dado que no es un dato que deba tenerse en cuenta
    atributosSinId = atributos.drop(columns = [ "id" ])
    
    # Entrenar y calcular metricas
    random.seed(0)
    gridSearch = sklearn.model_selection.GridSearchCV(estimator=sklearn.tree.DecisionTreeClassifier(),
                                                      param_grid=parametros, cv=k, n_jobs=2, scoring=metricas,
                                                      return_train_score=True, refit=metrica_mejor_ajuste)
    gridSearch.fit(X=atributosSinId, y=objetivo)

    # Generar DataFrame de resultados
    resultados    = gridSearch.cv_results_
    combinaciones = resultados["params"]
    resultadosDF  = []
    for tipoSet in ['train', 'test']:
        datosTipoSet = [ tipoSet ]
        for metrica in metricas:
            datosMetrica = datosTipoSet.copy()
            datosMetrica.append(metrica)
            for i in range(0, k):
                keyName = "split" + str(i) + "_" + tipoSet + "_" + metrica
                if keyName in resultados:
                    valores = resultados[keyName]
                    for j in range(0, len(combinaciones)):
                        datosCombinacion = datosMetrica.copy()
                        datosCombinacion.append(i)
                        datosCombinacion.append(valores[j])
                        for parametro in parametros.keys():
                            datosCombinacion.append(combinaciones[j][parametro])
                        resultadosDF.append(datosCombinacion)

    columnas  = ["conjunto", "metrica", "k", "valor"]
    columnas.extend(parametros.keys())
    groupcols = ["conjunto", "metrica" ]
    groupcols.extend(parametros.keys())
    resultadosDF = pandas.DataFrame.from_records(resultadosDF, columns=columnas)\
        .groupby(groupcols, as_index=False)\
        .aggregate({'valor':{'media':'mean', 'desvio':'std'}})

    return [gridSearch, resultadosDF]

parametros             = { "max_depth": [ 3 ] }
metricas               = [ "accuracy", "roc_auc" ]
gridSearch, resultados = EntrenarYEvaluarPerformance(atributos=atributosDesarrollo, objetivo=objetivoDesarrollo,
                                                     parametros=parametros, metricas=metricas,
                                                     metrica_mejor_ajuste='roc_auc', k=5)
print(resultados)

# 2.2 Entrenamiento de árboles con las siguientes combinaciones:
#     Altura máxima: { 3, 6, Inf }
#     Criterio: { 'gini', 'entropy' }
#     Métricas de performance: { 'accuracy', 'roc_auc' }

parametros             = { "max_depth": [ 3, 6 ], "criterion": [ 'gini', 'entropy' ] }
metricas               = [ "accuracy", "roc_auc" ]
gridSearch, resultados = EntrenarYEvaluarPerformance(atributos=atributosDesarrollo, objetivo=objetivoDesarrollo,
                                                     parametros=parametros, metricas=metricas,
                                                     metrica_mejor_ajuste='roc_auc', k=5)
print(resultados)
print("Parámetros del mejor árbol: " + str(gridSearch.best_params_))

parametros             = { "criterion": [ 'gini', 'entropy' ] }
metricas               = [ "accuracy", "roc_auc" ]
gridSearch, resultados = EntrenarYEvaluarPerformance(atributos=atributosDesarrollo, objetivo=objetivoDesarrollo,
                                                     parametros=parametros, metricas=metricas,
                                                     metrica_mejor_ajuste='roc_auc', k=5)
print(resultados)
profundidadCorteAutomatico = gridSearch.best_estimator_.tree_.max_depth
print("Profundidad con corte automático: " + str(profundidadCorteAutomatico))

# 2.3 Tomando el mejor método del punto 2.2 (criterio = Gini, max_depth = 6) para hacer algunos
#     experimentos que permitan testear la tolerancia a faltantes.

# Definicion de función para calcular la moda de una lista de valores
def Moda(valores):
    moda = max(set(valores), key=valores.count)
    return moda

# Definición de función para agregar una cierta cantidad de faltantes a una columna
def AgregarFaltantes(atributos, tasaFaltantes, columna):
    # Determinar el tamano de la población y de la muestra
    tamanoPoblacion = atributos.shape[0]
    tamanoMuestra   = round(tasaFaltantes * tamanoPoblacion)

    # Seleccionar al azar una cantidad filas
    random.seed(0)
    filas   = list(numpy.arange(0, tamanoPoblacion))
    muestra = random.sample(filas, tamanoMuestra)

    # Para la columna indicada, poner como faltantes los valores de las filas elegidas
    atributosConFaltantes = atributos.copy()
    atributosConFaltantes.loc[list(elem in muestra for elem in filas), columna] = None
    return atributosConFaltantes

# Definicion de función para reeemplazar faltantes
def ReemplazarFaltantes(atributos, columna, objetivo=None):
    if objetivo is None:
        # Moda común
        valoresColumna = list(atributos[atributos[columna].notnull()][columna])
        modaColumna    = Moda(valoresColumna)
        atributosNuevo = atributos.copy()
        atributosNuevo[columna].fillna(modaColumna, inplace=True)
        return atributosNuevo
    else:
        # Moda por clase
        clases         = list(objetivo.unique())
        filasNaN       = numpy.logical_not(list(atributos[columna].notnull()))
        atributosNuevo = atributos.copy()
        for clase in clases:
            filasClase     = list(objetivo == clase)
            filasNaNClase  = list(numpy.logical_and(filasNaN, filasClase))
            atributosClase = atributos.loc[filasClase,]
            valoresColumna = list(atributosClase[atributosClase[columna].notnull()][columna])
            modaColumna    = Moda(valoresColumna)
            atributosNuevo.loc[filasNaNClase, columna] = modaColumna
        return atributosNuevo

# Generar sets de datos con tasas de faltantes de 0 a 0.8 con un paso de 0.05
# Reemplazarlos por la moda y moda de clase.
# Ajustar el árbol de decisión con Gini y altura 6(el mejor árbol de 2.2)
analisisSensibilidadFaltantes = []
for clase in [ 'Moda', 'Moda de clase' ]:
    for tasaFaltantes in [0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8]:
        print("Experimentando con ", clase, " y ", str(tasaFaltantes))
        atributosEntrenamientoSinFaltantes = atributosEntrenamiento
        if (tasaFaltantes > 0):
            atributosEntrenamientoConFaltantes = AgregarFaltantes(atributos=atributosEntrenamiento,
                                                                  tasaFaltantes=tasaFaltantes,
                                                                  columna="weight")
            if clase == "Moda":
                atributosEntrenamientoSinFaltantes = ReemplazarFaltantes(atributos=atributosEntrenamientoConFaltantes,
                                                                         columna="weight")
            else:
                atributosEntrenamientoSinFaltantes = ReemplazarFaltantes(atributos=atributosEntrenamientoConFaltantes,
                                                                         columna="weight",
                                                                         objetivo=objetivoEntrenamiento)

        # Eliminar id a efectos de entrerar dado que no es un dato que deba tenerse en cuenta
        atributosEntrenamientoSinFaltantes = atributosEntrenamientoSinFaltantes.drop(columns = [ "id" ])
        atributosValidacionSinId           = atributosValidacion.drop(columns = [ "id" ])
        
        # Ajustar el árbol y obtener la profundidad del mismo y el accuracy
        arbolDecision = sklearn.tree.DecisionTreeClassifier(criterion = "gini", max_depth = 6, random_state = 0)
        arbolDecision.fit(atributosEntrenamientoSinFaltantes, objetivoEntrenamiento)
        predicciones  = arbolDecision.predict(atributosValidacionSinId)
        accuracy      = sklearn.metrics.accuracy_score(objetivoValidacion, predicciones)
        cantidadNodos = arbolDecision.tree_.node_count
        analisisSensibilidadFaltantes.append([ tasaFaltantes, clase, cantidadNodos, accuracy ])

analisisSensibilidadFaltantes = pandas.DataFrame.from_records(analisisSensibilidadFaltantes,
                                                              columns=["faltantes", "relleno", "cantidadNodos", "accuracy"])
    
# 2.4 Tomando el mejor método del punto 2.2 (criterio = Gini, altura = 6) para hacer algunos
#     experimentos que permitan testear la tolerancia a ruido. Para ello se definirá
#     una tasa porcentual de ruido (TPR) de forma de alterar el valor original 
#     modificando aleatoriamente el valor del siguiente modo:
#     ruido           = random.uniform(-TPR, TPR) * valorOriginal
#     valorModificado = valorOriginal + ruido
def AlterarValorPorRuido(valorOriginal, tasaPorcentualRuido = 0.2):
    ruido           = random.uniform(-tasaPorcentualRuido, tasaPorcentualRuido) * valorOriginal
    valorModificado = valorOriginal + ruido
    return valorModificado

def AgregarRuido(datosOriginales, atributosDiscretizados, informacionDiscretizacion, tasaDatosRuidosos, columna):
    # Determinar el tamano de la población y de la muestra
    tamanoPoblacion = atributosDiscretizados.shape[0]
    tamanoMuestra   = round(tasaDatosRuidosos * tamanoPoblacion)

    # Seleccionar al azar una cantidad filas
    random.seed(0)
    filas   = list(numpy.arange(0, tamanoPoblacion))
    muestra = random.sample(filas, tamanoMuestra)
    
    # Buscar los valores originales de los datos de la columna haciendo join por ID    
    atributosReducidos = atributosDiscretizados.iloc[muestra][["id"]]
    setDatosReducido   = setDatosOriginal.merge(atributosReducidos, how = "inner", on = "id")
    
    # Agregar ruido
    setDatosReducido["conRuido"] = list(setDatosReducido.apply(lambda row: AlterarValorPorRuido(row[columna]), axis = 1))
    
    # Volver a discretizar
    setDatosReducido["conRuidoDiscretizado"] = pandas.cut(x = setDatosReducido["conRuido"],
                                   labels = informacionDiscretizacion[columna]["etiquetas"],
                                   bins = informacionDiscretizacion[columna]["intervalos"])
    
    # Hacer join nuevamente para obtener el set de atributos modificado
    setDatosReducido           = setDatosReducido[["id", "conRuidoDiscretizado"]]
    atributosConRuido          = atributosDiscretizados.merge(setDatosReducido, how = "left")
    atributosConRuido[columna] = list(atributosConRuido.apply(lambda row: row[columna] if pandas.isnull(row["conRuidoDiscretizado"]) 
    else row["conRuidoDiscretizado"], axis = 1))
    atributosConRuido          = atributosConRuido.drop(columns = [ "conRuidoDiscretizado" ])
    
    return atributosConRuido

# Generar sets de datos con tasas de ruido de 0 a 0.35 con un paso de 0.05
# Ajustar el árbol de decisión con Gini y altura 6 (el mejor árbol de 2.2)
analisisSensibilidadRuido = []
for tasaDatosRuidosos in [0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35 ]:
    print("Experimentando con", str(tasaDatosRuidosos))
    atributosEntrenamientoRuidosos = atributosEntrenamiento
    if (tasaDatosRuidosos > 0):
        atributosEntrenamientoRuidosos = AgregarRuido(datosOriginales=setDatosOriginal,
                                                      atributosDiscretizados=atributosEntrenamiento,
                                                      informacionDiscretizacion=informacion,
                                                      tasaDatosRuidosos=tasaDatosRuidosos,
                                                      columna="weight")
        
    # Eliminar id a efectos de entrerar dado que no es un dato que deba tenerse en cuenta
    atributosEntrenamientoRuidosos = atributosEntrenamientoRuidosos.drop(columns = [ "id" ])
    atributosValidacionSinId       = atributosValidacion.drop(columns = [ "id" ])
        
    # Ajustar el árbol y obtener la profundidad del mismo y el accuracy
    arbolDecision = sklearn.tree.DecisionTreeClassifier(criterion = "gini", max_depth = 6, random_state = 0)
    arbolDecision.fit(atributosEntrenamientoRuidosos, objetivoEntrenamiento)
    predicciones  = arbolDecision.predict(atributosValidacionSinId)
    accuracy      = sklearn.metrics.accuracy_score(objetivoValidacion, predicciones)
    cantidadNodos = arbolDecision.tree_.node_count
    analisisSensibilidadRuido.append([ tasaDatosRuidosos, cantidadNodos, accuracy ])
    
analisisSensibilidadRuido = pandas.DataFrame.from_records(analisisSensibilidadRuido,
                                                              columns=["ruido", "cantidadNodos", "accuracy"])
        
# Graficado del árbol óptimo
# dotFile = sklearn.externals.six.StringIO()
# sklearn.tree.export_graphviz(decisionTree, out_file = dotFile, filled = True, rounded = True, special_characters = True)
# graph = pydotplus.graph_from_dot_data(dotFile.getvalue())
# graph.write_png("output/DT-optimal.png")

########################################################################################################################
# 3. Naive Bayes
########################################################################################################################

# Eliminar id a efectos de entrerar dado que no es un dato que deba tenerse en cuenta
atributosDesarrolloSinId = atributosDesarrollo.drop(columns = [ "id" ])
atributosTestSinId       = atributosTest.drop(columns = [ "id" ])
 
# Ajustar estacion bayesiana       
naiveBayes        = sklearn.naive_bayes.MultinomialNB()
naiveBayes.fit(atributosDesarrolloSinId, objetivoDesarrollo)
prediccionesBayes = naiveBayes.predict(atributosTestSinId)
accuracy          = sklearn.metrics.accuracy_score(objetivoTest, prediccionesBayes)
roc_auc           = sklearn.metrics.roc_auc_score(objetivoTest, prediccionesBayes)

# Obtener probabilidades a priori
clases      = objetivoDesarrollo.unique()
clases.sort() 
probAPriori = []
for clase in clases:
    probabilidadValor = (objetivoDesarrollo == clase).sum() / objetivoDesarrollo.count()
    probAPriori.append([ clase, probabilidadValor ])
probAPriori = pandas.DataFrame.from_records(probAPriori, columns=["clase", "probabilidad"]) 

# Obtener las probabilidades condicionales
probCondicionales = []
features          = atributosDesarrolloSinId.columns
for clase in clases:
    filasClase    = objetivoDesarrollo == clase
    cantidadClase = (filasClase == True).sum()
    for feature in features:
       valores = atributosDesarrolloSinId[feature].unique().tolist()
       valores.sort()    
       for valor in valores:                 
           filasValor         = atributosDesarrolloSinId[feature] == valor
           filasValorClase    = filasClase & filasValor
           cantidadValorClase = (filasValorClase == True).sum()
           probValorClase     = cantidadValorClase / cantidadClase
           probCondicionales.append([ feature, valor, clase, probValorClase ])           
probCondicionales = pandas.DataFrame.from_records(probCondicionales, columns=["atributo", "valor", "clase", "probabilidad"]) 

########################################################################################################################
# 4. Comparación de algoritmos
########################################################################################################################

# Mejor árbol de decisión. Usamos todo el conjunto de desarrollo para ajustar.
parametros             = { "criterion": [ 'gini', 'entropy' ], 'max_depth': numpy.arange(1, profundidadCorteAutomatico) }
metricas               = [ "roc_auc" ]
gridSearch, resultados = EntrenarYEvaluarPerformance(atributos=atributosDesarrollo, objetivo=objetivoDesarrollo,
                                                     parametros=parametros, metricas=metricas,
                                                     metrica_mejor_ajuste='roc_auc', k=5)

# Encontramos que el mejor árbol es el de altura 6 con Gini Gain (aunque hay muy poca diferencia con Information Gain)
mejorArbolDecision     = sklearn.tree.DecisionTreeClassifier(criterion = "gini", max_depth = 6, random_state = 0)
mejorArbolDecision.fit(atributosDesarrolloSinId, objetivoDesarrollo)
mejorArbolPredicciones = mejorArbolDecision.predict(atributosTestSinId)
mejorArbolRocAuc       = sklearn.metrics.accuracy_score(objetivoTest, mejorArbolPredicciones)

# Mejor estimador bayesiano. Acá no hay hiperparámetros para testear.
# Solamente se pueden probar con los 4 algoritmos que provee la librería para
# calcular las probabilidades condicionales: Bernoulli, Multinomial, Complement y Gaussian.
# El algoritmo apropiado sería el multinomial (o su variante, Complement). De todos modos
# probamos con todos los algoritmos para ver cuál da mejor para este set de datos.
mejorRocAuc         = 0
mejorAlgoritmoBayes = None
for naiveBayes in [ sklearn.naive_bayes.BernoulliNB(), sklearn.naive_bayes.ComplementNB(),
                    sklearn.naive_bayes.MultinomialNB(), sklearn.naive_bayes.GaussianNB() ]:
    naiveBayes.fit(atributosDesarrolloSinId, objetivoDesarrollo)
    prediccionesBayes = naiveBayes.predict(atributosTestSinId)
    rocAuc            = sklearn.metrics.roc_auc_score(objetivoTest, prediccionesBayes)
    if ((mejorAlgoritmoBayes == None) | (rocAuc > mejorRocAuc)):
        mejorAlgoritmoBayes = naiveBayes
        mejorRocAuc         = rocAuc    
        
# Encontramos que el mejor algoritmo es el Gaussiano. Sin embargo, sigue siendo mejor la clasificación 
# con árboles de decisión. Esto se debe a que la estimación bayesiana naive se ve severamente afectada por
# la no independencia de los atributos del set de datos (por ejemplo, es sabido que las personas obesas tiende
# a tener presión alta y valores de glucosa y colesterol elevados).    
