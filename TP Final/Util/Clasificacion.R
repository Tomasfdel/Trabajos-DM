library("randomForest")
source("Util/K-folds.R")


getRFError = function(trainData, trainLabels, testData, testLabels, treeAmount, maxNodes){
	rf_result = randomForest(trainData,
	                         y = as.factor(trainLabels),
	                         xtest = testData,
	                         ntree = treeAmount,
	                         maxnodes = maxNodes)
	return(mean(testLabels != rf_result$test$predicted))
}


runRandomForest = function(){
	foldedDataset = getKFolds(dataset, 9, 10)
	
	trees = (1:5) * 200
	maxNodes = (1:5) * 200
	
	foldIndexes = 1:10
	testErrors = rep(0, 10)
	for(testFold in 1 : 10){
		cat(sprintf("Test fold: %d\n", testFold))
		validErrors = matrix(rep(0, length(trees) * length(maxNodes)),
		                     nrow = length(trees))
		for(validFold in 1 : 10){
			if(validFold != testFold){
				cat(sprintf("  - Validation fold: %d\n", validFold))
				trainData = mergeFolds(foldedDataset, (1:10)[-c(testFold, validFold)])
				validData = mergeFolds(foldedDataset, c(validFold))
				for(treeIndex in 1 : length(trees)){
					for(nodeIndex in 1 : length(maxNodes)){
						validErrors[treeIndex, nodeIndex] = validErrors[treeIndex, nodeIndex] +  
						                                    getRFError(trainData[, -9], trainData[, 9], validData[, -9], validData[, 9], trees[treeIndex], maxNodes[nodeIndex])
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

		
		trainData = mergeFolds(foldedDataset, (1:10)[-c(testFold)])
		testData = mergeFolds(foldedDataset, c(testFold))
		testErrors[testFold] = getRFError(trainData[, -9], trainData[, 9], testData[, -9], testData[, 9], trees[bestTreeIndex], maxNodes[bestNodeIndex])
	}
	
	cat(sprintf("\n\nTest errors:"))
	print(testErrors)
	cat(sprintf("Average: %f\n", mean(testErrors)))	
}
