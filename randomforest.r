require(randomForest)
require(caret)

# Create a clean random forest ready data frame
createRFData = function(data, outcomeColName, numericPredictors, categoricalPredictors) {
    rfData = data[,which(names(data) %in% numericPredictors)]
    for (i in 1:length(categoricalPredictors)) {
        rfData[, categoricalPredictors[i]] = as.factor(data[, categoricalPredictors[i]])
    }
    rfData[, outcomeColName] = as.factor(data[, outcomeColName])
    return(rfData)
}

# Function to find the important variables based on the mini Gini decrease
findImportantVariables = function(data, outcomeColName) {
    colNames = names(data)
    outcomeCol = which(colNames == outcomeColName)
    
    model <- randomForest(
        data[, outcomeCol] ~ .,
        data=data[, -outcomeCol],
        ntree=10)
    
    imp <- importance(initialModel)
    imp <- imp[order(-imp),]
    impDf <- data.frame(MeanDecreaseGini=imp)
    impDf$VariableName <- factor(names(imp), levels = rev(names(imp)))
    
    # varImpPlot(initialModel) produces the plot but I want prettier one:
    # impPlot <- ggplot(data=impDf, aes(x=VariableName, y=MeanDecreaseGini)) +
    #    geom_bar(fill="salmon", stat="identity") + coord_flip()
    
    # Also create log version to show where to cut off
    # logImpPlot <- ggplot(data=impDf, aes(x=VariableName, y=log(MeanDecreaseGini))) +
    #     geom_point(fill="salmon", stat="identity") +
    #     stat_smooth(
    #         method = "lm",
    #         data=impDf,
    #         aes(x=as.numeric(VariableName),
    #             y=log(MeanDecreaseGini))) +
    #     coord_flip()
    
    # Based on the mean Gini decrease analysis, use the most important predictors
    logImpDf = data.frame(x=as.numeric(impDf$VariableName), y=log(impDf$MeanDecreaseGini))
    lmLogImp = lm(y ~ x, data=logImpDf)
    newData = data.frame(x=c(nrow(logImpDf):1))
    predictedLogImp = predict(lmLogImp, newdata=newData)
    
    # Count until the predicted log(MeanDecreaseGini) becomes greater than actual
    numImp = 0
    for (i in 1:length(newData$x)) {
        if (predictedLogImp[i] > logImpDf[i,]$y) break
        numImp <- numImp + 1
    }
    numImp = length(newData$x)
    
    # Use top numImp predictors for this model
    predictorsUsed <- names(imp)[1:numImp]
    return (prdictorsUsed)
}

# Do K-fold cross-validation and return the confusion matrix
crossValidationConfusionMatrix = function(data, outcomeColName, k=10) {
    colNames = names(data)
    outcomeCol = which(colNames == outcomeColName)
 
    # Assign each observation to one of 10 folds
    # Note that this replace=TRUE is NOT replacing the observation when sampling
    id <- sample(1:k, nrow(data), replace=TRUE)
    list <- 1:k
    
    prediction <- data.frame()
    actual <- data.frame()
    
    for (i in 1:k){    
        # Create training set from all training date except i-th fold
        currentTraining <- subset(data, id %in% list[-i])
        currentTesting <- subset(data, id %in% c(i))
    
        currentModel <- randomForest(
            currentTraining[, outcomeCol] ~ .,
            data=currentTraining[, -outcomeCol],
            ntree=10)
    
        message(paste("Model ", i))
        # print(currentModel$confusion)
        
        currentPrediction <- as.data.frame(predict(currentModel, currentTesting))
        prediction <- rbind(prediction, currentPrediction)
        
        currentActual <- as.data.frame(currentTesting[, outcomeCol])
        actual <- rbind(actual, currentActual)
    }
    
    result <- cbind(actual[, 1], prediction)
    names(result) <- c("Actual", "Predicted")
    confusionMatrix = table(result)
    return(confusionMatrix)
}