library(class)
library(rpart)
source("../Ejercicio 1/diagonal.R")

getClassMembers = function(class, dataset, classIndex){
	# Devuelve todos los valores de dataset que pertenezcan a la clase class.
	
	dataset[dataset[classIndex] == class, ]
}

shuffleClassMembers = function(classMembers){
	#Reordena los elementos del dataframe de entrada.
	
	classMembers[sample(nrow(classMembers)),]
}

splitClasses = function(dataset, foldAmount){
	#~ 	Esta función devuelve una lista en la que cada elemento tiene un dataframe con todos los
	#~ 	valores que pertenecen a la misma clase.
	
	classIndex = dim(dataset)[2]
	dataByClass = lapply(unique(dataset[[classIndex]]), getClassMembers, dataset, classIndex)		
	lapply(dataByClass, shuffleClassMembers)	
}

getValuesOutOfFold = function(dataset, foldIndex, foldAmount){
	#~ Toma los elementos de una clase, calcula el tamaño que debe tener un fold y devuelve
	#~ todos los elementos que no pertenezcan al foldIndex correspondiente.
	
	foldSize = dim(dataset)[1] / foldAmount 
	dataset[- (((foldIndex - 1) * foldSize + 1) : (foldIndex * foldSize)) ,]
}


getValuesInFold = function(dataset, foldIndex, foldAmount){
	#~ Toma los elementos de una clase, calcula el tamaño que debe tener un fold y devuelve
	#~ todos los elementos que pertenezcan al foldIndex correspondiente.
	
	foldSize = dim(dataset)[1] / foldAmount 
	dataset[((foldIndex - 1) * foldSize + 1) : (foldIndex * foldSize) ,]
}


getFoldedData = function(dataset, foldIndex, foldAmount){
	#~ Dado un un dataset dividido por clases y un foldIndex, devuelve una lista con la combinación de 
	#~ los puntos de todas las clases que no pertenecen al fold indicado, y por otro lado los que sí pertenecen.

	trainList = lapply(dataset, getValuesOutOfFold, foldIndex, foldAmount)
	testList = lapply(dataset, getValuesInFold, foldIndex, foldAmount)
	
	#~ Una vez que se consiguieron los datos que pertenecen al fold y los que no, divididos por clase,
	#~ se los combina para obtener el set de entrenamiento y el de test.
	list(do.call(rbind.data.frame, trainList), do.call(rbind.data.frame, testList))
}


args = commandArgs(trailingOnly = TRUE)
if(length(args) != 5){
	cat("ERROR. Modo de uso: Rscript kfold_diagonal_test.R setSize dimension C KNN_K foldAmount\n")
} else {
	setSize = as.integer(args[1])
	dimension = as.integer(args[2])
	C = as.double(args[3])
	K = as.integer(args[4])
	foldAmount = as.integer(args[5])
	
	diagDataset = generateDiagonalData(setSize, dimension, C)
	splitDataset = splitClasses(diagDataset)	
	
	
	knnErrors = c()
	treeErrors = c()
	for(foldIndex in 1:foldAmount){
		#~ Teniendo los datos divididos por clase, armaremos el n-ésimo fold y lo devolveremos como set de test.
		#~ Todos los demás los uniremos para armar el set de entrenamiento. Cada fold se armará con una proporción
		#~ de puntos de cada clase igual a la proporción en el dataset completo.
		#~ Más allá de eso, el entrenamiento funciona igual que en el regular_test, salvo que guardamos los errores de
		#~ cada iteración para luego promediarlos.
		
		foldedDataset = getFoldedData(splitDataset, foldIndex, foldAmount)
		
		diagTrain = foldedDataset[[1]]
		diagTest = foldedDataset[[2]]
		
		classIndex = dimension + 1
		diagKnn = knn(diagTrain[,-classIndex], 
					  test = diagTest[,-classIndex],
					  cl = diagTrain[,classIndex], 
					  k = K)
					  
		diagTree = rpart(paste("X", classIndex, "~.", sep = ""), data = diagTrain, method = "class")
		diagTreePred = predict(diagTree, diagTest)
		
		knnErrors[foldIndex] = mean(diagTest[,dimension + 1] == diagKnn)
		treeErrors[foldIndex] = mean(diagTest[,dimension + 1] == max.col(diagTreePred, "first") - 1)
	}
	
	cat(sprintf("Error Diagonal KNN: %f\n", 1 - mean(knnErrors)))
	cat(sprintf("Error Diagonal Tree: %f\n", 1 - mean(treeErrors)))	
}


#~ Resultados de una ejecución:
#~ setSize = 200
#~ dimension = 8
#~ C = 1
#~ KNN_K = 3
#~ foldAmount = 5

#~ Error Diagonal KNN: 0.221500
#~ Error Diagonal Tree: 0.283500

#~ Vemos que los resultados son un poco más pesimistas que el test regular, 
#~ pero el margen es muy pequeño para ser realmente significativo.



