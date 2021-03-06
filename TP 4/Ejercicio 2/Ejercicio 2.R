library("adabag")
library("randomForest")
library("e1071")
source("K-folds.R")
load("lampone.Rdata")


getRFError = function(trainData, trainLabels, testData, testLabels, treeAmount, maxNodes){
	rf_result = randomForest(trainData,
	                         y = trainLabels,
	                         xtest = testData,
	                         ntree = treeAmount,
	                         maxnodes = maxNodes)
	return(mean(testLabels != rf_result$test$predicted))
}


runRandomForest = function(foldedDataset, targetColumn){
	foldAmount = length(foldedDataset)
	
	trees = (1:10) * 100
	maxNodes = (1:10) * 2
	
	foldIndexes = 1:foldAmount
	testErrors = rep(0, foldAmount)
	for(testFold in foldIndexes){
		cat(sprintf("Test fold: %d\n", testFold))
		validErrors = matrix(rep(0, length(trees) * length(maxNodes)),
		                     nrow = length(trees))
		for(validFold in foldIndexes){
			if(validFold != testFold){
				cat(sprintf("  - Validation fold: %d\n", validFold))
				trainData = mergeFolds(foldedDataset, foldIndexes[-c(testFold, validFold)])
				validData = mergeFolds(foldedDataset, c(validFold))
				for(treeIndex in 1 : length(trees)){
					for(nodeIndex in 1 : length(maxNodes)){
						validErrors[treeIndex, nodeIndex] = validErrors[treeIndex, nodeIndex] +  
						                                    getRFError(trainData[, -targetColumn], 
						                                               trainData[, targetColumn], 
						                                               validData[, -targetColumn], 
						                                               validData[, targetColumn], 
						                                               trees[treeIndex], 
						                                               maxNodes[nodeIndex])
					}
				
				}
			}
		}
				            
		bestValues = arrayInd(which.min(validErrors), dim(validErrors))
		bestTreeIndex = bestValues[1]
		bestNodeIndex = bestValues[2]
		cat(sprintf("Best result: Tree count %d, Max node count %d.\nError: %f\n\n", 
		            trees[bestTreeIndex], 
		            maxNodes[bestNodeIndex], 
		            validErrors[bestTreeIndex, bestNodeIndex]))

		
		trainData = mergeFolds(foldedDataset, foldIndexes[-c(testFold)])
		testData = mergeFolds(foldedDataset, c(testFold))
		testErrors[testFold] = getRFError(trainData[, -targetColumn], 
		                                  trainData[, targetColumn], 
		                                  testData[, -targetColumn], 
		                                  testData[, targetColumn], 
		                                  trees[bestTreeIndex], 
		                                  maxNodes[bestNodeIndex])
	}
	
	cat(sprintf("\n\nTest errors:"))
	print(testErrors)
	cat(sprintf("Average: %f\n", mean(testErrors)))	
}


getAdaboostError = function(trainData, testData, maxDepth){
	boost_result = boosting(N_tipo ~ ., 
							data = trainData, 
							coef = "Freund", 
							control = rpart.control(maxdepth = maxDepth))
	boost_predictions = predict(boost_result, testData)
	return(boost_predictions$error)
}


runAdaboost = function(foldedDataset, targetColumn){
	foldAmount = length(foldedDataset)
	
	maxDepths = 1:6 * 5
	
	foldIndexes = 1:foldAmount
	testErrors = rep(0, foldAmount)
	for(testFold in foldIndexes){
		cat(sprintf("Test fold: %d\n", testFold))
		validErrors = rep(0, length(maxDepths))
		for(validFold in foldIndexes){
			if(validFold != testFold){
				cat(sprintf("  - Validation fold: %d\n", validFold))
				trainData = mergeFolds(foldedDataset, foldIndexes[-c(testFold, validFold)])
				validData = mergeFolds(foldedDataset, c(validFold))
				for(depthIndex in 1 : length(maxDepths)){
					validErrors[depthIndex] = validErrors[depthIndex] +  
											  getAdaboostError(trainData, validData, maxDepths[depthIndex])
				}
			}
		}
				                        
		bestDepthIndex = which.min(validErrors)
		cat(sprintf("Best result: Depth %d.\nError: %f\n\n", 
		            maxDepths[bestDepthIndex], 
		            validErrors[bestDepthIndex]))

		trainData = mergeFolds(foldedDataset, foldIndexes[-c(testFold)])
		testData = mergeFolds(foldedDataset, c(testFold))
		testErrors[testFold] = getAdaboostError(trainData, testData, maxDepths[bestDepthIndex])
		cat(sprintf("Test error: %f\n\n", 
		            testErrors[testFold]))
	}
	
	cat(sprintf("\n\nTest errors:"))
	print(testErrors)
	cat(sprintf("Average: %f\n", mean(testErrors)))	
}


getPolySVMError = function(trainData, trainLabels, testData, testLabels, degree){
	svm_result = svm(trainData, 
	                 y = trainLabels, 
	                 scale = FALSE,
	                 type = "C-classification", 
	                 kernel = "polynomial",
	                 degree = degree)
	svm_predictions = predict(svm_result, testData)
	return(mean(testLabels != svm_predictions))
}


runPolySVM = function(foldedDataset, targetColumn){
	foldAmount = length(foldedDataset)
	
	degrees = 1:8
	
	foldIndexes = 1:foldAmount
	testErrors = rep(0, foldAmount)
	for(testFold in foldIndexes){
		cat(sprintf("Test fold: %d\n", testFold))
		validErrors = rep(0, length(degrees))
		for(validFold in foldIndexes){
			if(validFold != testFold){
				cat(sprintf("  - Validation fold: %d\n", validFold))
				trainData = mergeFolds(foldedDataset, foldIndexes[-c(testFold, validFold)])
				validData = mergeFolds(foldedDataset, c(validFold))
				for(degIndex in 1 : length(degrees)){
						validErrors[degIndex] = validErrors[degIndex] +  
						                        getPolySVMError(trainData[, -targetColumn], 
						                                        trainData[, targetColumn], 
						                                        validData[, -targetColumn], 
						                                        validData[, targetColumn], 
						                                        degrees[degIndex])
				}
			}
		}
		
		bestDegree = which.min(validErrors)
		cat(sprintf("Best result: Degree %d.\nError: %f\n\n", 
		            degrees[bestDegree], 
		            validErrors[bestDegree]))
		
		trainData = mergeFolds(foldedDataset, foldIndexes[-c(testFold)])
		testData = mergeFolds(foldedDataset, c(testFold))
		testErrors[testFold] = getPolySVMError(trainData[, -targetColumn], 
		                                       trainData[, targetColumn], 
		                                       testData[, -targetColumn], 
		                                       testData[, targetColumn], 
		                                       degrees[bestDegree])
	}
	
	cat(sprintf("\n\nTest errors:"))
	print(testErrors)
	cat(sprintf("Average: %f\n", mean(testErrors)))
}


getRBFSVMError = function(trainData, trainLabels, testData, testLabels, gamma){
	svm_result = svm(trainData, 
	                 y = trainLabels, 
	                 scale = FALSE,
	                 type = "C-classification", 
	                 kernel = "radial",
	                 gamma = gamma)
	svm_predictions = predict(svm_result, testData)
	return(mean(testLabels != svm_predictions))
}


runRBFSVM = function(foldedDataset, targetColumn){
	foldAmount = length(foldedDataset)
	
	gammas = c(0.0001, 0.0002, 0.0003, 0.0004, 0.0005, 0.001, 0.005, 0.01, 0.1, 1, 10)

	foldIndexes = 1:foldAmount
	testErrors = rep(0, foldAmount)
	for(testFold in foldIndexes){
		cat(sprintf("Test fold: %d\n", testFold))
		validErrors = rep(0, length(gammas))
		for(validFold in foldIndexes){
			if(validFold != testFold){
				cat(sprintf("  - Validation fold: %d\n", validFold))
				trainData = mergeFolds(foldedDataset, foldIndexes[-c(testFold, validFold)])
				validData = mergeFolds(foldedDataset, c(validFold))
				for(gammaIndex in 1 : length(gammas)){
						validErrors[gammaIndex] = validErrors[gammaIndex] +  
						                          getRBFSVMError(trainData[, -targetColumn], 
						                                         trainData[, targetColumn], 
						                                         validData[, -targetColumn], 
						                                         validData[, targetColumn], 
						                                         gammas[gammaIndex])
				}
			}
		}
		
		bestGamma = which.min(validErrors)
		cat(sprintf("Best result: Gamma %f.\nError: %f\n\n", 
		            gammas[bestGamma], 
		            validErrors[bestGamma]))
		
		trainData = mergeFolds(foldedDataset, foldIndexes[-c(testFold)])
		testData = mergeFolds(foldedDataset, c(testFold))
		testErrors[testFold] = getPolySVMError(trainData[, -targetColumn], 
		                                       trainData[, targetColumn], 
		                                       testData[, -targetColumn], 
		                                       testData[, targetColumn], 
		                                       gammas[bestGamma])
	}
	
	cat(sprintf("\n\nTest errors:"))
	print(testErrors)
	cat(sprintf("Average: %f\n", mean(testErrors)))
}


runAllMethods = function(){
	#~ 	Columnas cuyos valores son únicamente 0 o valores no numéricos.
	forbiddenColumns = c(6, 70, 94, 107, 114, 116, 120, 121, 122, 124, 130, 132, 133, 134, 142, 144)
	dataset = lampone[, -forbiddenColumns]
	foldedDataset = getKFolds(dataset, dim(dataset)[2], 10)
	
	cat(sprintf("-- RANDOM FOREST\n"))
	runRandomForest(foldedDataset, dim(dataset)[2])
	
	cat(sprintf("\n\n-- POLYNOMIAL SVM\n"))
	runPolySVM(foldedDataset, dim(dataset)[2])
	
	cat(sprintf("\n\n-- RADIAL BASIS SVM\n"))
	runRBFSVM(foldedDataset, dim(dataset)[2])
	
	cat(sprintf("\n\n-- ADABOOST\n"))
	runAdaboost(foldedDataset, dim(dataset)[2])
}
