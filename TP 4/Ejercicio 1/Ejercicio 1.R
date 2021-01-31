library("adabag")

getBoostingErrors = function(trainData, testData, startDepth, endDepth){
	# Calcula el error en test de boosting para un intervalo de profundidades máximas [startDepth; endDepth].
	
	errors = c()
	for(depth in startDepth : endDepth){
		print(depth)
		boostResult = boosting(class ~ ., data = trainData, mfinal = 200, coef = "Freund", control = rpart.control(maxdepth = depth))
		predictions = predict(boostResult, testData)
		errors[depth - startDepth + 1] = predictions$error
	}
	return(errors)
}

