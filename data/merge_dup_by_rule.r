mergeDupByRule =
function(dataFrame,
         by="id",
         earlierEntry=c(),
         greater=c(),
         lesser=c(),
         dateNewer=c(),
         dateOlder=c(),
         customRuleFunc=NULL) {
    if (by %in%
        c(earlierEntry, greater, lesser, dateNewer, dateOlder)) {
        stop("column specified by 'by' cannot be used in conditions.")
    }
    isValid <- function(value) {
        if (is.na(value) || is.nan(value)) {
            return(FALSE)
        }
        return(TRUE)
    }
    isEmptyStr <- function(value) {
        if (is.character(value) && value == "") {
            return(TRUE)
        }
        return(FALSE)
    }
    takeEarlierEntry <- function(colName, originalData, newData) {
        return(originalData[,colName])
    }
    takeGreater <- function(colName, originalData, newData) {
        newVal <- newData[,colName]
        origVal <- originalData[,colName]

        if (is.na(newVal) || is.na(origVal) || is.nan(newVal) || is.nan(origVal)) {
            stop(paste("Missing value! Original: ", origVal, " New: ", newVal, sep=""))
        }
        if (origVal < newVal) {
            return (newVal)
        } else {
            return (origVal)
        }
    }
    takeLesser <- function(colName, originalData, newData) {
        newVal <- newData[,colName]
        origVal <- originalData[,colName]

        if (is.na(newVal) || is.na(origVal) || is.nan(newVal) || is.nan(origVal)) {
            stop(paste("Missing value! Original: ", origVal, " New: ", newVal, sep=""))
        }
        if (origVal > newVal) {
            return (newVal)
        } else {
            return (origVal)
        }
    }
    takeDateNewer <- function(colName, originalData, newData) {
        newVal <- newData[,colName]
        origVal <- originalData[,colName]

        if (as.Date(origVal) < as.Date(newVal)) {
            return (newVal)
        } else {
            return (origVal)
        }
    }
    takeDateOlder <- function(colName, originalData, newData) {
        newVal <- newData[,colName]
        origVal <- originalData[,colName]

        if (as.Date(origVal) > as.Date(newVal)) {
            return (newVal)
        } else {
            return (origVal)
        }
    }

    # Clean data by consolidating the dups with the rules
    colNames <- names(dataFrame)
    if (!(by %in% colNames)) {
        stop(paste(by, " not present in the column names!", sep=" "))
    }
    cleanDF <- dataFrame[0,]  # Copy schema only
    for (i in 1:nrow(dataFrame)) {
        current <- dataFrame[i,]
        if (nrow(cleanDF) == 0 ||
            nrow(cleanDF[cleanDF[,by] == as.matrix(current[,by])[1,1],]) == 0) {
            cleanDF <- rbind(cleanDF, current)
            next()
        }
        prev <- cleanDF[cleanDF[,by] == as.matrix(current[,by])[1,1],]
        for (j in 1:length(colNames)) {
            colName <- colNames[j]
            prevVal <- prev[, colName]
            currVal <- current[, colName]
            if (colName == by) {
                next()
            }

            if (!isValid(currVal)) {
                next()
            } else if (!isValid(prevVal)) {
                prev[,colName] <- currVal
                next()
            }

            if (isEmptyStr(currVal)) {
                next()
            } else if (isEmptyStr(prevVal)) {
                prev[,colName] <- currVal
                next()
            }

            ruleFunc <- NULL
            # Check for earlier entry (Order sensitive!)
            if (colName %in% earlierEntry) {
                ruleFunc <- takeEarlierEntry
            }
            # Check for quantity greater
            else if (colName %in% greater) {
                ruleFunc <- takeGreater
            }
            # Check for quantity lesser
            else if (colName %in% lesser) {
                ruleFunc <- takeLesser
            }
            # Check for dates newer
            else if (colName %in% dateNewer) {
                ruleFunc <- takeDateNewer
            }
            # Check for dates older
            else if (colName %in% dateOlder) {
                ruleFunc <- takeDateOlder
            }

            if (!is.null(ruleFunc)) {
                prev[, colName] <- ruleFunc(colName, prev, current)
                next()
            }

            # Custom rule function
            if (!is.null(customRuleFunc)) {
                # prev and current are single row data.frame
                val <- customRuleFunc(colName, prev, current)
                if (!is.na(val)) {
                    prev[,colName] <- val
                    next()
                }
            }

            # Finally give a warning if not N/A and not equal
            if (!is.na(prevVal) && prevVal != currVal) {
                warning(paste("Dup entries with different values not treated.",
                              "id: ", prev[by], "\n",
                              "col: ", colName, "\n",
                              "val 1: ", prev[colName], "\n",
                              "val 2: ", current[colName], "\n", sep=""))
            }
        }
        # Commit the change
        cleanDF[cleanDF[,by]==prev[,by],] <- prev
    }
    return (cleanDF)
}

mergeDupByRule.tests = function () {
    testMergeDupByRule <- function() {
        a <- data.frame(
            id=c(1, 1, 1, 1, 2),
            val=c(NA, 1, 2, 0, 2),
            price=c(1, 1, 1, 2, 2),
            id2=c(1, 1, 2, 1, 2))
        ret <- mergeDupByRule(a, by="id", earlierEntry=c("id2"), greater=c("val"), lesser=c("price"))
        expect <- data.frame(id=c(1, 2), val=c(2, 2), price=c(1, 2), id2=c(1, 2))
        if (!all.equal(expect, ret, check.attributes <- FALSE)) {
            stop("Test 1 failed")
        }
    }

    testMergeDupByRule2 <- function() {
        a <- data.frame(
            id=c(1, 1, 1, 1, 2),
            val=c(NA, 1, 1, 0, 2),
            price=c(1, 1, 1, 2, 2),
            id2=c(1, 1, 2, 1, 2))
        expect <- data.frame(id=c(1, 1), val=c(1, 2), price=c(2, 2), id2=c(1, 2))
        ret <- mergeDupByRule(a, by="id2", earlierEntry=c("id"), greater=c("val", "price"))
        if (!all.equal(expect, ret, check.attributes <- FALSE)) {
            stop("Test 2 failed")
        }
    }
    testMergeDupByRule()
    testMergeDupByRule2()
}
