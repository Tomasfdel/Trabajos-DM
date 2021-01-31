generateDiagonalData <- function(n, d, C){
	#~ 	Como los valores de cada dimensión de los puntos se calculan independientemente de los demás,
	#~ 	no es necesario calcular punto por punto. Entonces, puedo generar la primer componente de todos
	#~ 	los puntos, luego la segunda, y continuar así con todas las dimensiones. Esa es la idea detrás
	#~ 	de usar replicate para la generación de puntos.
	
	class0 = data.frame(cbind(replicate(d, rnorm(n / 2, mean = -1, sd = C * sqrt(d))), rep(0, n / 2)))
	class1 = data.frame(cbind(replicate(d, rnorm(n / 2, mean =  1, sd = C * sqrt(d))), rep(1, n / 2)))
	rbind(class0, class1)
}
