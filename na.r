numNARows =
function(dataFrame, colName) {
    return (length(which(is.na(dataFrame[, colName]))))
}

colsWithNA =
function(dataFrame) {
    # List the columns who have NA
    colNames <- names(dataFrame)
    colWithNA <- c()
    for (i in 1:length(colNames)) {
        if (numNARows(dataFrame, colNames[i])) {
            colWithNA <- c(colWithNA, colNames[i])
        }
    }
    return(colWithNA)
}
