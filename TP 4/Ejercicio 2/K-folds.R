getClassMembers = function(class, dataset, classIndex){
	# Devuelve todos los valores de dataset que pertenezcan a la clase class.
	dataset[dataset[, classIndex] == class, ]
}


shuffleClassMembers = function(classMembers){
	#Reordena los elementos del dataframe de entrada.
	
	classMembers[sample(nrow(classMembers)),]
}


splitClasses = function(dataset, classIndex, foldAmount){
	#~ 	Devuelve una lista en la que cada elemento tiene un dataframe con todos los
	#~ 	valores que pertenecen a la misma clase.
	
	dataByClass = lapply(as.list(unique(dataset[, classIndex])), getClassMembers, dataset, classIndex)		
	lapply(dataByClass, shuffleClassMembers)	
}


getDataForFold = function(foldIndex, data, foldSize){
	#~ Dado el tamaño de los folds, devuelve los datos que corresponden al fold con el índice indicado.
	data[((foldIndex - 1) * foldSize + 1) : (foldIndex * foldSize) ,]
}


splitInFolds = function(data, foldAmount) {
	#~ Dado un conjunto de datos de la misma clase, lo divide en un conjunto de folds especificado.
	#~ En caso de que la cantidad de datos no sea divisible por el número de folds, reparte los valores 
	#~ restantes al azar.

	foldSize = dim(data)[1] %/% foldAmount
	splitData = lapply(as.list(1:foldAmount), getDataForFold, data, foldSize)
	
	remainingValues = dim(data)[1] - foldSize * foldAmount
	remainderFolds = sample(1:foldAmount, remainingValues)
	if(remainingValues > 0){
		for(index in 1:remainingValues){
			splitData[[remainderFolds[index]]] = rbind(splitData[[remainderFolds[index]]], 
													   data[foldSize * foldAmount + index,])	
		}
	}	
	return(splitData)
}


getListElementByIndex = function(list, index){
	#~ Devuelve el elemento de la lista en el índice dado.	
	list[[index]]	
}


mergeDataForFold = function(foldIndex, splitDataset){
	#~ Dado una lista de valores divididos por clase y luego por fold, y un índice de fold,
	#~ agrupa los valores de todas las clases pertenecientes a dicho fold.	
	foldParts = lapply(splitDataset, getListElementByIndex, foldIndex)
	do.call(rbind, foldParts)
}


getKFolds = function(dataset, classIndex, foldAmount){
	#~ 	Realiza k-folding estratificado sobre el dataset pasado como argumento. 
	
	dataByClass = splitClasses(dataset, classIndex, foldAmount)
	foldedDataByClass = lapply(dataByClass, splitInFolds, foldAmount)
	lapply(as.list(1:foldAmount), mergeDataForFold, foldedDataByClass)
}


mergeFolds = function(foldList, indexes) {
	#~ Combina los folds de los índices dados en una única matriz
	do.call(rbind, foldList[indexes])
}
