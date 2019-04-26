# Carga de librerías
import pandas
import sklearn.model_selection
import sklearn.tree

# Lectura de set de datos discretizado
setDatosDiscretizado = pandas.read_csv(filepath_or_buffer = "input/SetDatosDiscretizado.csv",
                                       sep = "\t", na_values = "NA")

# Separo los features del data frame del target a aprender
features = setDatosDiscretizado.drop(columns = "cardio")
target   = setDatosDiscretizado['cardio']

# Separar en sets de entrenamiento, validación y testeo
trainValidationFeatures, testFeatures, trainValidationTarget, testTarget = \
    sklearn.model_selection.train_test_split(features, target, train_size = 0.7, test_size = 0.3, random_state = 0)
trainFeatures, validationFeatures, trainTarget, validationTarget = \
    sklearn.model_selection.train_test_split(trainValidationFeatures, trainValidationTarget, train_size = 0.6, test_size = 0.4, random_state = 0)

# Ajustar set de entrenamiento
decisionTree = sklearn.tree.DecisionTreeClassifier(max_depth = 5)
decisionTree.fit(trainFeatures, trainTarget)
