#~ -- MÉTODOS AUXILIARES --


#~ Realiza K-means para todos los K en el intervalo especificado, sobre
#~ el dataset dado. Devuelve la suma de distancias inter-clusters para cada K.
iterateKmeans = function(dataset, minK, maxK){
	results = double(maxK - minK + 1)
	for(K in minK : maxK){
		kmeans_result = kmeans(dataset, cent = K)
		results[K - minK + 1] = kmeans_result$tot.withinss
	}
	return(results)
}


#~ Devuelve una lista de datasets generados uniformemente en el área
#~ de la PCA del dataset dato.
generateUniformDatasets = function(baseDataset, amount){
	nRows = dim(baseDataset)[1]
	nColumns = dim(baseDataset)[2]

#~ 	Calcula la PCA de los datos y obtiene el rango de cada variable.
	datasetPCA = prcomp(baseDataset)
	valueRanges = apply(datasetPCA$x, 2, range)
	
	uniformDatasets = list()
	for(index in 1 : amount){
		uniformDataset = c()

#~ 		Construye cada dataset haciendo una muestra uniforme para cada variable
#~ 		entre el mínimo y máximo del atributo correspondiente para el dataset.
		for(columnIndex in 1 : min(nRows, nColumns)){
			uniformColumn = runif(nRows, valueRanges[1, columnIndex], valueRanges[2, columnIndex])
			uniformDataset = cbind(uniformDataset, uniformColumn)
		}			
	
		uniformDatasets[[index]] = uniformDataset	
	}
	
	return(uniformDatasets)
}


#~ Dada una lista de vectores, obtiene los valores en la targetIndex-ésima
#~ posición de cada vector.
getListItemsByIndex = function(list, targetIndex){
	results = double(length(list))
	for(index in 1:length(list)){
		results[index] = list[[index]][targetIndex]
	}
	return(results)
}


#~ Calcula el valor de estabilidad de acuerdo al código proporcionado en el enunciado
#~ para los resultados sobre dos réplicas, identificados por los índices en el dataset
#~ original de sus valores y los valores de cluster obtenidos para cada entrada.
getStabilityScore = function(datasetSize, sample1, sample2, clusters1, clusters2){
	v1 = v2 = rep(0, datasetSize)
	v1[sample1] = clusters1
	v2[sample2] = clusters2
	
	a = sqrt(v1%*%t(v1))
	m1 = a / -a + 2 * (a == round(a))
	m1[is.nan(m1)] = 0
	a = sqrt(v2%*%t(v2))
	m2 = a / -a + 2 * (a == round(a))
	m2[is.nan(m2)] = 0
	
	validos = sum(v1 * v2 > 0)
	score = sum((m1 * m2)[upper.tri(m1)] > 0) / (validos * (validos - 1) / 2)
	return(score)
}


#~ -- MÉTODOS PEDIDOS EN EL EJERCICIO --


#~ Calcula la GAP Statistic para un dataset entre los K mínimo y máximo dados, con cierta
#~ cantidad especificada de muestras de una distribución uniforme, utilizando K-means.
#~ Devuelve el valor de GAP para cada K en el rango dado, junto con su desvío estándar.
#~ Además, también retorna el número sugerido de clusters según el algoritmo estudiado.
gapStatistic = function(dataset, minK, maxK, uniformAmount){
#~ 	Consigue la sumas de distancias intercluster sobre el dataset base y le aplica
#~ 	el logaritmo a cada uno para usarlo en las fórmulas siguientes.
	datasetSums = log(iterateKmeans(dataset, minK, maxK))

#~ 	Realiza el mismo procedimiento para cada uno de los datasets de muestra.
	uniformDatasets = generateUniformDatasets(dataset, uniformAmount)
	uniformSums = list()
	for(index in 1:uniformAmount){
		uniformSums[[index]] = log(iterateKmeans(uniformDatasets[[index]], minK, maxK))
	}
#~ 	Luego, calcula Gap(K) y el desvío estándar para cada uno de los K usados.
	gapValues = double(maxK - minK + 1)
	gapDeviations = double(maxK - minK + 1)
	for(K in minK : maxK){
#~ 		Primero obtengo la suma inter-cluster para un K dado, en todas las muestras uniformes.
		uniformResults = getListItemsByIndex(uniformSums, K - minK + 1)
#~ 		Luego calculo el promedio de restarle la suma inter-cluster del dataset base a cada uno.
#~ 		Eso será Gap(K).
		gapValues[K - minK + 1] = mean(uniformResults) - datasetSums[K - minK + 1]
#~ 		Después calculo el desvío estándar de dicho promedio, ajustando por un factor al final ya que
#~ 		la función sd divide por el tamaño del arreglo - 1 en vez del tamaño del arreglo.
		uniformDeviation = sqrt((sd(uniformResults) ^ 2) * ((uniformAmount - 1) / uniformAmount)) 
		gapDeviations[K - minK + 1] = uniformDeviation * sqrt(1 + 1/uniformAmount)
	}
	
#~ 	Por último, calculo el primer K tal que satisfaga la desigualdad planteada en el algoritmo.
	clusterAmount = minK
	while(clusterAmount < maxK && 
	      gapValues[clusterAmount - minK + 1] < gapValues[clusterAmount - minK + 2] - gapDeviations[clusterAmount - minK + 2]){
		clusterAmount = clusterAmount + 1
	}
	
	return(list(gapValues = gapValues,
	            gapDeviations = gapDeviations,
	            suggestedClusterAmount = clusterAmount))
}

#~ Realiza el Método de la Estabilidad con K-means para un dataset dado, entre los valores
#~ de K especificados, con cierta cantidad de réplicas.
#~ Devuelve, para cada K, un arreglo con los valores de estabilidad de cada par de réplicas 
#~ para su posterior análisis o graficación.
stabilityMethod = function(dataset, minK, maxK, replicaAmount){
	nRows = dim(dataset)[1]

#~ 	Se producen las réplicas a utilizar mediante subsampling con 90% de tamaño de muestra.
	samples = list()
	for(index in 1:replicaAmount){
		samples[[index]] = sample(nRows, 0.9 * nRows)
	}
	
#~ 	Se calculan los valores de estabilidad para cada K sobre todas las réplicas.
	stability = list()
	for(K in minK : maxK){
		kMeansResults = list()
		for(replica in 1 : replicaAmount){
			kMeansResults[[replica]] = kmeans(dataset[samples[[replica]], ], K)$cluster
		}
		
#~ 		Se obtiene el valor de estabilidad entre cada par de réplicas.
		stabilityScores = c()
		for(index1 in 1 : (replicaAmount - 1)){
			for(index2 in (index1 + 1) : replicaAmount){
				stabilityScores = c(stabilityScores, 
				                    getStabilityScore(nRows, 
				                                      samples[[index1]], 
				                                      samples[[index2]], 
				                                      kMeansResults[[index1]], 
				                                      kMeansResults[[index2]]))
			}			
		}
		
		stability[[K - minK + 1]] = stabilityScores
	}
	
	names(stability) = minK : maxK
	return(stability)
}
