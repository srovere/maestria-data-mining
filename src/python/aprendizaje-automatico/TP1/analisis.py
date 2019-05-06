########################################################################################################################
# 1. Pasos preparatorios
########################################################################################################################

# Carga de librerías
import pandas
import pydotplus
import sklearn.externals.six
import sklearn.metrics
import sklearn.model_selection
import sklearn.tree

# Lectura de set de datos discretizado
setDatosDiscretizado = pandas.read_csv(filepath_or_buffer = "input/SetDatosDiscretizado.csv",
                                       sep = "\t", na_values = "NA")

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

def EntrenarYEvaluarPerformance(atributos, objetivo, parametros, metricas, k=5):
    # Entrenar y calcular metricas
    gridSearch = sklearn.model_selection.GridSearchCV(estimator=sklearn.tree.DecisionTreeClassifier(),
                                                      param_grid=parametros, cv=k, n_jobs=2, scoring=metricas,
                                                      return_train_score=True, refit=False)
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
        .groupby(groupcols)\
        .aggregate({'valor':{'media':'mean', 'desvio':'std'}})

    return (resultadosDF)

parametros = {
    "max_depth": [ 3, 6 ],
    "criterion": [ "gini", "entropy" ]
}
metricas   = [ "accuracy", "roc_auc" ]
resultados = EntrenarYEvaluarPerformance(atributosDesarrollo, objetivoDesarrollo, parametros, metricas)

#crossValidation             = {}
#crossValidation["Accuracy"] = sklearn.model_selection.cross_validate(decisionTree, developmentFeatures, developmentTarget,
#                                                                 cv = 5, scoring = 'accuracy', return_train_score = True)
#crossValidation["ROC AUC"]  = sklearn.model_selection.cross_validate(decisionTree, developmentFeatures, developmentTarget,
#                                                                     cv = 5, scoring = 'roc_auc', return_train_score = True)
#crossValidationData         = []
#for scoringMethod in crossValidation.keys():
#    cvTrainScores = crossValidation[scoringMethod]["train_score"]
#    cvTestScores  = crossValidation[scoringMethod]["test_score"]
#    crossValidationData.append([ scoringMethod, "Entrenamiento", cvTrainScores.mean(), cvTrainScores.std() ])
#    crossValidationData.append([ scoringMethod, "Validación", cvTestScores.mean(), cvTestScores.std() ])

#crossValidationDataFrame = pandas.DataFrame.from_records(crossValidationData,
#                                                         columns = [ "Métrica", "Conjunto", "Media", "Desvío" ])

# 2.2 Entrenamiento de árboles con las siguientes combinaciones:
#     Altura máxima: { 3, 6, Inf }
#     Criterio: { 'gini', 'entropy' }
#     Métricas de performance: { 'accuracy', 'roc_auc' }


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
