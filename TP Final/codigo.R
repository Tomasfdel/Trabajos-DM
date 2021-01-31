#~ CARGA DE DATOS
dataset = read.csv("nasa.csv")


#~ PRETRATAMIENDO DE DATOS
#~ Eliminación de columnas con información ya presente pero con otra unidad de medición.
dataset = dataset[,-(6:11)]
dataset = dataset[,-(9:10)]
dataset = dataset[,-c(9, 11, 12)]
#~ Eliminación de columnas con valores constantes.
dataset = dataset[,-c(10, 28)]
#~ Eliminación del Diámetro Mínimo Estimado.
dataset = dataset[,-4]
#~ Eliminación de la Fecha de Determinación de Órbita.
dataset = dataset[,-10]
#~ Eliminación de la Fecha de Aproximación.
dataset = dataset[,-5]
#~ Eliminación de nombres e IDs.
dataset = dataset[,-(1:2)]
#~ Escalado de datos.
dataset[,1:21] = scale(dataset[,1:21])
#~ Conversión del predictor de bools a enteros.
dataset[,22] = as.integer(dataset[,22])


#~ ANÁLISIS DE VARIABLES RELEVANTES
source("Util/Filtro.R")
filter_result = non.parametric.filter(dataset[,1:21], dataset[, 22])
#~ Dejamos en el dataset los ocho campos más relevantes según el filtro, más el clasificador.
dataset = dataset[,c(filter_result$ordered.features.list[1:8], 22)]


#~ BÚSQUEDA DE CLUSTERS
#~ Cálculo de GAP y determinación de la cantidad sugerida de clusters.
source("Util/Clustering.R")
gapResult = gapStatistic(dataset[,-9], 1, 10, 500)
gapResult$suggestedClusterAmount
#~ Ejecución de K-Means.
kmeansResult = kmeans(dataset[,-9],cent=4)
#~ Distribución por cluster de los asteroides peligrosos y no peligrosos.
table(kmeansResult$cluster[dataset[,9] == 1])
table(kmeansResult$cluster[dataset[,9] == 2])


#~ CLASIFICACIÓN
source("Util/Clasificacion.R")
runRandomForest()
