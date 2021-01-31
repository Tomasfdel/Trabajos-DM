#~ Filter con test no paramétrico.
non.parametric.filter <- function(data, classes){
	# Arreglo donde guardaremos el resultado del test para cada feature.
	feature_scores = double(dim(data)[2])
	
	# Realizamos el test de Kruskal-Wallis para cada feature.
	for(index in 1 : length(feature_scores)){
		feature_scores[index] = kruskal.test(data[, index], classes)$statistic
	}
	
	# Ordenamos los features por score decreciente y devolvemos la información
	# en el mismo formato que venimos usando. En este caso, usamos como importancia
	# el valor de los tests para luego poder analizar dichos valores.
	feature_scores = sort(feature_scores, decreasing = TRUE, index = TRUE)
	feature_names = colnames(data)[feature_scores$ix]
	importance = feature_scores $ x
	names(importance) = feature_names
	
	return(list(ordered.names.list = feature_names, 
			    ordered.features.list = feature_scores $ ix, 
			    importance = importance))
}
