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
features = setDatosDiscretizado.drop(columns = [ "id", "cardio" ])
target   = setDatosDiscretizado['cardio']

# Separar en sets de entrenamiento, validación y testeo
trainValidationFeatures, testFeatures, trainValidationTarget, testTarget = \
    sklearn.model_selection.train_test_split(features, target, train_size = 0.7, test_size = 0.3, random_state = 0)
trainFeatures, validationFeatures, trainTarget, validationTarget = \
    sklearn.model_selection.train_test_split(trainValidationFeatures, trainValidationTarget, train_size = 0.6, test_size = 0.4, random_state = 0)

# Ajustar set de entrenamiento
decisionTree = sklearn.tree.DecisionTreeClassifier(max_depth = 5, criterion = "gini", random_state = 0)
decisionTree.fit(trainFeatures, trainTarget)

# Evaluar precisión
predictions = decisionTree.predict(testFeatures)
confusionMatrix = sklearn.metrics.confusion_matrix(testTarget, predictions)
print(confusionMatrix)
accuracy = sklearn.metrics.accuracy_score(testTarget, predictions)
print(accuracy)

# Graficado del árbol
dotFile = sklearn.externals.six.StringIO()
sklearn.tree.export_graphviz(decisionTree, out_file = dotFile, filled = True, rounded = True, special_characters = True)
graph = pydotplus.graph_from_dot_data(dotFile.getvalue())
graph.write_png("output/DT.png")
