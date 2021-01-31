library(class)
library(rpart)
source("../Ejercicio 1/spirals.R")


args = commandArgs(trailingOnly = TRUE)
if(length(args) != 3){
	cat("ERROR. Modo de uso: Rscript regular_spiral_test.R trainSize testSize KNN_K\n")
} else {
	trainSize = as.integer(args[1])
	testSize = as.integer(args[2])
	K = as.integer(args[3])
	
	spiralTrain = generateSpiralsData(trainSize)
	spiralTest = generateSpiralsData(testSize)
	
	spiralKnn = knn(spiralTrain[,-3], 
			        test = spiralTest[,-3],
			        cl = spiralTrain[,3], 
				    k = K)
	
	spiralTree = rpart("X3~.", data = spiralTrain, method = "class")
	spiralTreePred = predict(spiralTree, spiralTest)
	
	cat(sprintf("Error Espiral KNN: %f\n", 1 - mean(spiralTest[,3] == spiralKnn)))
	#~ 	La columna que tenga el valor más grande es a la que corresponde la clasificación del árbol,
	#~ 	por lo que determino para cada fila cuál es el índice del máximo y le resto 1, ya que las clases
	#~ 	son 0 y 1 mientras que las columnas tienen índices 1 y 2.
	cat(sprintf("Error Espiral Tree: %f\n", 1 - mean(spiralTest[,3] == max.col(spiralTreePred, "first") - 1)))	
}


#~ Resultados de una ejecución:
#~ trainSize = 200
#~ testSize = 2000
#~ KNN_K = 3

#~ Error Espiral KNN: 0.167500
#~ Error Espiral Tree: 0.277500




