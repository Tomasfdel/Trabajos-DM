source("../util/diagonal.R")
source("../util/codigo_practico_2.R")
source("../Ejercicio 1/Ejercicio 1.R")

# Crea un conjunto de datos siguiendo la especificación del ejercicio:
# 100 valores, de los cuales 50 tienen que ser de cada clase, cuyas
# primeras 10 variables corresponden a datos generados por el problema
# diagonal y otras 90 son ruido uniforme.
create_data = function(){
	diagonal_data = generateDiagonalData(100, 10, 2)
	ruido = crea.ruido.unif(50, 90)
	return(cbind(diagonal_data[, 1:10], ruido[, 1:90], diagonal_data[, 11]))
}

# Calcula la cantidad de hits que tuvo un método. Es decir, cuántas de 
# las diez variables más importantes no son ruido.
get_hit_amount = function(method_result){
	ordered_indexes = method_result$ordered.features.list
	first_indexes = ordered_indexes[1:10]
	return(sum(first_indexes <= 10))
}


# Genera un conjunto de datos 30 veces, le aplica los cuatro métodos e 
# imprime el número de hits por ejecución junto al promedio de cada método.
evaluate_method_hits = function(){
	forward_hits = double(30)
	backward_hits = double(30)
	filter_hits = double(30)
	rfe_hits = double(30)
	
	for(iteration in 1:30){
		cat(sprintf("Iteration: %d\n", iteration))
		
		data = create_data()
		
		forward_result = forward.ranking(data[, -101], as.factor(data[, 101]), method = "lda.est")
		forward_hits[iteration] = get_hit_amount(forward_result)
		
		backward_result = backward.ranking(data[, -101], as.factor(data[, 101]), method = "lda.est")
		backward_hits[iteration] = get_hit_amount(backward_result)
		
		filter_result = non.parametric.filter(data[, -101], as.factor(data[, 101]))
		filter_hits[iteration] = get_hit_amount(filter_result)
		
		rfe_result = rfe(data[, -101], as.factor(data[, 101]), method = "imp.linsvm")
		rfe_hits[iteration] = get_hit_amount(rfe_result)
	}
	
	cat("\n\nGreedy forward wrapper hits: ")
	cat(forward_hits)
	cat(sprintf("\nAverage hits: %f", mean(forward_hits)))
	
	cat("\n\nGreedy backward wrapper hits: ")
	cat(backward_hits)
	cat(sprintf("\nAverage hits: %f", mean(backward_hits)))
	
	cat("\n\nNon-parametric filter hits: ")
	cat(filter_hits)
	cat(sprintf("\nAverage hits: %f", mean(filter_hits)))
	
	cat("\n\nRFE hits: ")
	cat(rfe_hits)
	cat(sprintf("\nAverage hits: %f\n\n", mean(rfe_hits)))
}
