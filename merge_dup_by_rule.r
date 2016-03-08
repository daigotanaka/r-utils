mergeDupByRule = function(dataFrame, by="id", ruleFunc=NULL) {
    # Clean data by consolidating the dups with the rules
    colNames = names(dataFrame)
    if (!(by %in% colNames)) {
        stop(paste(by, " not present in the column names!", sep=" "))
    }
    # Copy schema only
    cleanDF= dataFrame[0,]
    for (i in 1:nrow(dataFrame)) {
        current = dataFrame[i,]
        currentId = current[by]
        prev = cleanDF[cleanDF[,by]==current[,by],]
        if (nrow(prev) == 0) {
            cleanDF = rbind(cleanDF, current)
            next()
        }
        for (j in 1:length(colNames)) {
            colName = colNames[j]
            if (colName == by) {
                next()
            }
            # General rule: Replace N/A with current
            if (is.na(prev[colName])) {
                prev[colName] = current[colName];
                next()
            }
            if (!is.null(ruleFunc)) {
                val = ruleFunc(colName, prev, current);
                if (!is.na(val)) {
                    prev[colName] = val
                    next()
                }
            }
            # Finally give a warning if not N/A and not equal
            if (!is.na(prev[colName]) && prev[colName] != current[colName]) {
                warning(paste("Dup entries with different values not treated.",
                              "id: ", prev[by], "\n",
                              "col: ", colName, "\n",
                              "val 1: ", prev[colName], "\n",
                              "val 2: ", current[colName], "\n", sep=""))
            }
        }
        # Commit the change
        cleanDF[cleanDF[,by]==prev[,by],] = prev
    }
    return (cleanDF)
}

testMergeDupByRule = function() {
    ruleFunc1 = function(colName, originalData, newData) {
        newVal = newData[colName]
        origVal = originalData[colName];
        if (colName == "val") {
            if (originalData[colName] < newData[colName]) {
                return (newVal)
            } else {
                return (origVal)
            }
        }
        if (colName == "price") {
            if (originalData[colName] > newData[colName]) {
                return (newVal)
            } else {
                return (origVal)
            }
        }
        if (colName == "id2") {
            return(origVal)
        }
        return(NA)
    }

    a = data.frame(
        id=c(1, 1, 1, 1, 2),
        val=c(NA, 1, 2, 0, 2),
        price=c(1, 1, 1, 2, 2),
        id2=c(1, 1, 2, 1, 2))
    expect = data.frame(id=c(1, 2), val=c(2, 2), price=c(1, 2), id2=c(1, 2))
    return (mergeDupByRule(a, by="id", ruleFunc=ruleFunc1))
}

testMergeDupByRule2 = function() {
    ruleFunc1 = function(colName, originalData, newData) {
        newVal = newData[colName];
        origVal = originalData[colName];
        if (colName == "val") {
            if (originalData[colName] < newData[colName]) {
                return (newVal);
            } else {
                return (origVal);
            }
        }
        if (colName == "price") {
            if (originalData[colName] < newData[colName]) {
                return (newVal);
            } else {
                return (origVal);
            }
        }
        if (colName == "id") {
            return(origVal);
        }
        return(NA);
    }

    a = data.frame(
        id=c(1, 1, 1, 1, 2),
        val=c(NA, 1, 1, 0, 2),
        price=c(1, 1, 1, 2, 2),
        id2=c(1, 1, 2, 1, 2))
    expect = data.frame(id=c(1, 1), val=c(1, 2), price=c(2, 2), id2=c(1, 2))
    return (mergeDupByRule(a, by="id2", ruleFunc=ruleFunc1));
}
