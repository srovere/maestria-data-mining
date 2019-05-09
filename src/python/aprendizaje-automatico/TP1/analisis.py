########################################################################################################################
# 1. Pasos preparatorios
########################################################################################################################

# Carga de librerías
import numpy
import pandas
import pydotplus
import random
import sklearn.externals.six
import sklearn.metrics
import sklearn.model_selection
import sklearn.tree
import sys

# Definicion de funcion para discretizar variables por cuantiles
def DiscretizarPorCuantiles(datos, columnas, intervalos):
    datosNuevos = datos.copy()
    informacion = {}
    for columna in columnas:
        paso                 = 1 / intervalos
        cuantiles            = numpy.arange(0, 1.0 + paso, paso)
        etiquetas            = list(map(str, numpy.arange(1, intervalos + 1)))
        categorias           = pandas.qcut(x=datos[columna], q=cuantiles, labels=etiquetas)
        datosNuevos[columna] = categorias
        informacion[columna] = { "cuantiles": cuantiles, "etiquetas": etiquetas }
    return [datosNuevos, informacion ]

# Lectura de set de datos discretizado
setDatosOriginal = pandas.read_csv(filepath_or_buffer = "input/SetDatosDiscretizado.csv", sep = "\t",
                                       na_values = "NA")
setDatosDiscretizado, informacion = DiscretizarPorCuantiles(datos=setDatosOriginal, columnas=['age','height','weight'], intervalos=10)

# Separo los features del data frame del target a aprender
atributos = setDatosDiscretizado.drop(columns = [ "id", "cardio" ])
objetivo  = setDatosDiscretizado['cardio']

# Separar en sets de desarrollo (entrenamiento + validacion) y testeo
atributosDesarrollo, atributosTest, objetivoDesarrollo, objetivoTest = \
    sklearn.model_selection.train_test_split(atributos, objetivo, train_size = 0.8, test_size = 0.2, random_state = 0)

########################################################################################################################
# 2. Árboles de decisión
########################################################################################################################

# 2.1 Entrenamiento de árbol con altura 3 y estimacion de performance con 5-fold cross validation

def EntrenarYEvaluarPerformance(atributos, objetivo, parametros, metricas, metrica_mejor_ajuste, k=5):
    # Entrenar y calcular metricas
    random.seed(0)
    gridSearch = sklearn.model_selection.GridSearchCV(estimator=sklearn.tree.DecisionTreeClassifier(),
                                                      param_grid=parametros, cv=k, n_jobs=2, scoring=metricas,
                                                      return_train_score=True, refit=metrica_mejor_ajuste)
    gridSearch.fit(X=atributos, y=objetivo)

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
print("Profundidad del mejor árbol: " + str(gridSearch.best_estimator_.tree_.max_depth))

# 2.3 Tomando el mejor árbol del punto 2.2 (criterio = Gini) para hacer algunos
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

# Separar el set de desarrollo en sets de entrenamiento y validacion
atributosEntrenamiento, atributosValidacion, objetivoEntrenamiento, objetivoValidacion = \
    sklearn.model_selection.train_test_split(atributosDesarrollo, objetivoDesarrollo,
                                             train_size = 0.8, test_size = 0.2, random_state = 0)

# Generar sets de datos con tasas de faltantes de 0 a 0.8 con un paso de 0.05
# Reemplazarlos por la moda y moda de clase.
# Ajustar el árbol de decisión con Gini y altura 6 (el mejor árbol de 2.2)
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

        arbolDecision   = sklearn.tree.DecisionTreeClassifier(criterion = "gini", random_state = 0)
        arbolDecision.fit(atributosEntrenamientoSinFaltantes, objetivoEntrenamiento)
        predicciones    = arbolDecision.predict(atributosValidacion)
        accuracy        = sklearn.metrics.accuracy_score(objetivoValidacion, predicciones)
        profundidad     = arbolDecision.tree_.max_depth
        analisisSensibilidadFaltantes.append([ tasaFaltantes, clase, profundidad, accuracy ])

analisisSensibilidadFaltantes = pandas.DataFrame.from_records(analisisSensibilidadFaltantes,
                                                              columns=["faltantes", "relleno", "profundidad", "accuracy"])

# def EntrenarYEvaluarPerformance(atributos, objetivo, parametros, metricas, metrica_mejor_ajuste, k=5):

# Ajustar set de entrenamiento
# decisionTree = sklearn.tree.DecisionTreeClassifier(max_depth = 5, criterion = "gini", random_state = 0)
# decisionTree.fit(trainFeatures, trainTarget)

# Evaluar precisión
# predictions = decisionTree.predict(validationFeatures)
# confusionMatrix = sklearn.metrics.confusion_matrix(validationTarget, predictions)
# print(confusionMatrix)
# accuracy = sklearn.metrics.accuracy_score(validationTarget, predictions)
# print(accuracy)

# Graficado del árbol
# dotFile = sklearn.externals.six.StringIO()
# sklearn.tree.export_graphviz(decisionTree, out_file = dotFile, filled = True, rounded = True, special_characters = True)
# graph = pydotplus.graph_from_dot_data(dotFile.getvalue())
# graph.write_png("output/DT.png")

# Ahora hago una exploración de parámetros para optimizar el árbol
# parameters = { 'max_depth': range(1, 9), 'criterion':  [ 'entropy', 'gini' ] }
# gridSearch = sklearn.model_selection.GridSearchCV(sklearn.tree.DecisionTreeClassifier(), parameters, n_jobs = 4)
# gridSearch.fit(X = trainValidationFeatures, y = trainValidationTarget)
# treeModel  = gridSearch.best_estimator_
# print (gridSearch.best_score_, gridSearch.best_params_)

# Evaluar precisión con set de testeo
# predictions = treeModel.predict(testFeatures)
# accuracy = sklearn.metrics.accuracy_score(testTarget, predictions)
# print(accuracy)

# Graficado del árbol óptimo
# dotFile = sklearn.externals.six.StringIO()
# sklearn.tree.export_graphviz(decisionTree, out_file = dotFile, filled = True, rounded = True, special_characters = True)
# graph = pydotplus.graph_from_dot_data(dotFile.getvalue())
# graph.write_png("output/DT-optimal.png")
