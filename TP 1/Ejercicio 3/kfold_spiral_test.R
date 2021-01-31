library(class)
library(rpart)
source("../Ejercicio 1/spirals.R")

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
if(length(args) != 3){
	cat("ERROR. Modo de uso: Rscript kfold_spiral_test.R setSize KNN_K foldAmount\n")
} else {
	setSize = as.integer(args[1])
	K = as.integer(args[2])
	foldAmount = as.integer(args[3])
	
	spiralDataset = generateSpiralsData(setSize)
	splitDataset = splitClasses(spiralDataset)	
	
	knnErrors = c()
	treeErrors = c()
	for(foldIndex in 1:foldAmount){
		#~ Teniendo los datos divididos por clase, armaremos el n-ésimo fold y lo devolveremos como set de test.
		#~ Todos los demás los uniremos para armar el set de entrenamiento. Cada fold se armará con una proporción
		#~ de puntos de cada clase igual a la proporción en el dataset completo.
		#~ Más allá de eso, el entrenamiento funciona igual que en el regular_test, salvo que guardamos los errores de
		#~ cada iteración para luego promediarlos.

		foldedDataset = getFoldedData(splitDataset, foldIndex, foldAmount)
		
		spiralTrain = foldedDataset[[1]]
		spiralTest = foldedDataset[[2]]
		
		spiralKnn = knn(spiralTrain[,-3], 
				  	    test = spiralTest[,-3],
					    cl = spiralTrain[,3], 
					    k = K)
	
		spiralTree = rpart("X3~.", data = spiralTrain, method = "class")
		spiralTreePred = predict(spiralTree, spiralTest)
		
		knnErrors[foldIndex] = mean(spiralTest[,3] == spiralKnn)
		treeErrors[foldIndex] = mean(spiralTest[,3] == max.col(spiralTreePred, "first") - 1)
	}
	
	cat(sprintf("Error Espiral KNN: %f\n", 1 - mean(knnErrors)))
	cat(sprintf("Error Espiral Tree: %f\n", 1 - mean(treeErrors)))	
}


#~ Resultados de una ejecución:
#~ setSize = 200
#~ KNN_K = 3
#~ foldAmount = 5

#~ Error Espiral KNN: 0.250000
#~ Error Espiral Tree: 0.380000

#~ Para este caso, los resultados son realmente más pesimistas por un margen significativo,
#~ a diferencia de en el caso diagonal.

