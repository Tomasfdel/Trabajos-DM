library(class)
library(rpart)
source("../Ejercicio 1/diagonal.R")


args = commandArgs(trailingOnly = TRUE)
if(length(args) != 5){
	cat("ERROR. Modo de uso: Rscript regular_diagonal_test.R trainSize testSize dimension C KNN_K\n")
} else {
	trainSize = as.integer(args[1])
	testSize = as.integer(args[2])
	dimension = as.integer(args[3])
	C = as.double(args[4])
	K = as.integer(args[5])
	
	diagTrain = generateDiagonalData(trainSize, dimension, C)
	diagTest = generateDiagonalData(testSize, dimension, C)
	
	classIndex = dimension + 1
	diagKnn = knn(diagTrain[,-classIndex], 
				  test = diagTest[,-classIndex],
				  cl = diagTrain[,classIndex], 
				  k = K)
	
	#~ 	Como los nombres de las columnas en el dataset generado son X1, X2, ..., XclassIndex, tengo que
	#~ 	usar paste para generar el string correspondiente. Por ejemplo, para una dimensión 2, con 
	#~ 	classIndex siendo 3, terminaría armando el string "X3~."
	diagTree = rpart(paste("X", classIndex, "~.", sep = ""), data = diagTrain, method = "class")
	diagTreePred = predict(diagTree, diagTest)
	
	cat(sprintf("Error Diagonal KNN: %f\n", 1 - mean(diagTest[,classIndex] == diagKnn)))
	#~ 	La columna que tenga el valor más grande es a la que corresponde la clasificación del árbol,
	#~ 	por lo que determino para cada fila cuál es el índice del máximo y le resto 1, ya que las clases
	#~ 	son 0 y 1 mientras que las columnas tienen índices 1 y 2.
	cat(sprintf("Error Diagonal Tree: %f\n", 1 - mean(diagTest[,classIndex] == max.col(diagTreePred, "first") - 1)))	
}


#~ Resultados de una ejecución:
#~ trainSize = 200
#~ testSize = 2000
#~ dimension = 8
#~ C = 1
#~ KNN_K = 3

#~ Error Diagonal KNN: 0.217500
#~ Error Diagonal Tree: 0.273500


