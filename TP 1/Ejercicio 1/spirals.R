getSpiralPoint <- function(class){
	#~ 	Esta función toma la clase a la que tiene que pertenecer el punto y genera 
	#~ 	candidatos hasta que uno caiga en la franja deseada.
	
	found = FALSE
	while(! found){
		point = runif(2, -1, 1)
		distance = sqrt(sum(point ^ 2))
		angle = atan2(point[2], point[1])
		baseCurve = angle / (4 * pi)
		
		if(distance > 1){
			found = FALSE
		} else {			
			if((distance >= baseCurve && distance <= baseCurve + 0.25) ||
	           (distance >= baseCurve + 0.5 && distance <= baseCurve + 0.75) ||
	            distance >= baseCurve + 1){
				found = class == 0
	        } else { 
				found = class == 1
			}
		}
	}
	c(point, class)
}


generateSpiralsData <- function(n){
	#~ 	Como las coordenadas no son independientes entre sí para los puntos de las espirales,
	#~ 	uso replicate y luego transpongo los datos para que queden en el formato buscado.
	
	class0 = data.frame(t((replicate(n / 2, getSpiralPoint(0)))))
	class1 = data.frame(t((replicate(n / 2, getSpiralPoint(1)))))
	rbind(class0, class1)
}




