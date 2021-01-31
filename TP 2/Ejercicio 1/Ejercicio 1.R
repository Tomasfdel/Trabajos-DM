source("../util/codigo_practico_2.R")

#~ Wrapper greedy backward.
backward.ranking <- function(data, classes, method, ...){
	feature_amount = dim(data)[2]
	feature_list = 1 : feature_amount
	# Lista donde se guardarán los features en orden de eliminación.
	feature_removal_order = double(feature_amount)
	  
	# Iteraremos hasta haber eliminado todos los features excepto uno.
	while(length(feature_list) > 1){
		# Almacenaremos tantas estimaciones de error como features nos queden en la iteración.
		error_estimations = double(length(feature_list))
		# Por cada feature que nos queda, estimaremos el error de entrenar un método usando
		# todos los demás menos él.
		for(feature_index in 1 : length(feature_list)){
			training_data = as.matrix(data[, feature_list[-feature_index]])
			error_estimations[feature_index] = do.call(method, c(list(training_data, classes), list(...)))
		}
		
		# Eliminamos el feature que, al entrenar sin él, nos resulta en el error mínimo
		# entre todas las opciones.
		feature_to_remove = which.min(error_estimations)
		feature_removal_order[length(feature_list)] = feature_list[feature_to_remove]
		feature_list = feature_list[- feature_to_remove]
	}
	
	# El último feature en eliminar es el que haya quedado de las iteraciones anteriores.
	feature_removal_order[1] = feature_list[1]
	
	# Luego, preparamos la información para que se devuelva con el mismo formato que 
	# tiene el método de base que estaba dado.
	feature_names = colnames(data)[feature_removal_order]
	importance = (feature_amount : 1) / feature_amount
	names(importance) = feature_names
	
 	return(list(ordered.names.list = feature_names, 
 	            ordered.features.list = feature_removal_order, 
 	            importance = importance))
}
	

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


#~ RFE.
rfe <- function(data, classes, method, ...){
	feature_amount = dim(data)[2]
	feature_list = 1 : feature_amount
	# Lista donde se guardarán los features en orden de eliminación.
	feature_removal_order = double(feature_amount)
	  		
	# Iteraremos hasta haber eliminado todos los features excepto uno.
	while(length(feature_list) > 1){
		# Calculamos los scores de los features que nos quedan en un momento dado.
		training_data = as.matrix(data[, feature_list])
		feature_scores = do.call(method, c(list(training_data, classes), list(...)))
		# Luego, eliminamos el que tenga el menor score.
		feature_to_remove = feature_scores$feats[1]
		feature_removal_order[length(feature_list)] = feature_list[feature_to_remove]
		feature_list = feature_list[- feature_to_remove]
	}
	
	# El último feature en eliminar es el que haya quedado de las iteraciones anteriores.
	feature_removal_order[1] = feature_list[1]
		
	# Preparamos la información para que tenga el mismo formato que los demás métodos.
	feature_names = colnames(data)[feature_removal_order]
	importance = (feature_amount : 1) / feature_amount
	names(importance) = feature_names
	
 	return(list(ordered.names.list = feature_names, 
 	            ordered.features.list = feature_removal_order, 
 	            importance = importance))
}
