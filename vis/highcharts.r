# The functions require rchart-helper.R preloaded

getLineRangePlot =
function(seriesNames, lines, ranges, rangeName, yLabel, verticalLineDate=NULL, colors, timezone="UTC") {
    series <- list()
    minValue <- 0
    for (i in 1:length(lines)) {
        series[[2 * (i - 1) + 1]] <-
            list(name=seriesNames[i], data=lines[[i]], zIndex=1, color=colors[i],
                 marker=list(fillColor="white", lineWidth=2, lineColor=colors[i]))
        series[[2 * i]] <- list(name=rangeName, data=ranges[[i]], zIndex=0,
                 type="arearange", color=colors[i], lineWidth=0, linkedTo=":previous", fillOpacity=0.3)
        minValue <- min(minValue, min(ranges[[i]][2,]))
    }

    chart <- Highcharts$new()
    xAxis <- list(type="datetime")
    if (!is.null(verticalLineDate)){
        date <- as.POSIXlt(strptime(as.character(verticalLineDate), "%Y-%m-%d", tz=timezone))
        xAxis[["plotLines"]] <- paste("[{color: 'red',",
                                     "value: Date.UTC(", date$year + 1900, ",", date$mon, ",", date$mday, "),",
                                     "width: 2}]", sep="")
    }
    chart$set(xAxis=xAxis)
    chart$yAxis(title=list(text=yLabel), min=minValue)
    chart$set(series=series)
    return(chart)
}

getMean1SigmaTimelapsePlot = function(data, names, colors, yLabel, colName="x", verticalLineDate=NULL, timezone="UTC") {
    col <- which(names(data[[1]])==colName)[1]
    dateFactors <- list()
    statsMean <- list()
    statsStds <- list()
    for (i in 1:length(data)) {
        data[[i]]$x = data[[i]][, col]
        stats = 
            group_by(data[[i]], date) %>% 
            summarize(mean=mean(x), stdLower=mean(x) - sd(x), stdUpper=(mean(x) + sd(x))) %>%
            arrange(date)
        stats = t(stats)
        # Timpstamp in miliseconds
        unixTimestamps <-
            1000 * as.numeric(as.POSIXct(sort(unique(data[[i]]$date)),
                                         origin="1970-01-01"))
        statsMean[[i]] <- as.data.frame(rbind(setNames(unixTimestamps, nm=NULL), 
                                        as.numeric(stats[2,])))
        names(statsMean[[i]]) <- NULL
        statsStds[[i]] <- as.data.frame(rbind(setNames(unixTimestamps, nm=NULL),
                                              as.numeric(stats[3,]),
                                              as.numeric(stats[4,])))
        names(statsStds[[i]]) <- NULL
    }
    return(getLineRangePlot(names, statsMean, statsStds, "1 sigma", yLabel, verticalLineDate, colors, timezone))
}


# getQ2TimelapsePlot
# data[[]]$x: Stats
# data[[]]$date: Date
getQ2TimelapsePlot = function(data, names, colors, yLabel, colName="x", verticalLineDate=NULL, timezone="UTC") {
    col <- which(names(data[[1]])==colName)[1]
    dateFactors <- list()
    statsMedian <- list()
    statsQ2 <- list()
    for (i in 1:length(data)) {
        dateFactors[[i]] <- as.factor(data[[i]]$date)
        boxplot <- boxplot(data[[i]][, col] ~ dateFactors[[i]],
                          data=data.frame(dateFactors[[i]], data[[i]][, col]), plot=FALSE)
        stats <- setNames(as.data.frame(boxplot$stats), nm=NULL)

        # Timpstamp in miliseconds
        unixTimestamps <-
            1000 * as.numeric(as.POSIXct(sort(unique(data[[i]]$date)),
                                         origin="1970-01-01"))
        statsMedian[[i]] <- rbind(setNames(unixTimestamps, nm=NULL), stats[3,])
        statsQ2[[i]] <- rbind(setNames(unixTimestamps, nm=NULL), stats[c(2, 4),])
    }

    return(getLineRangePlot(names, statsMedian, statsQ2, "50th quartile", yLabel, verticalLineDate, colors, timezone))
}

# Helper for creating histogram
getBinItemList = function(data, businesses, interval=100) {
    binItemList <- c()
    currentBin <- interval
    maxBin <- max(data$count) + interval
    while (currentBin < maxBin) {
        items <- filter(data, currentBin - interval <= count & count < currentBin)
        binItemList <- c(binItemList,
                        paste("< ", currentBin, "<br>",
                              paste(items$name, collapse="<br>, ")))
        currentBin <- currentBin + interval
    }
    return(binItemList)
}

# getStackedHistogram
# data[[]]$x
getStackedHistogram = function(data,
                               names,
                               xLabel,
                               yLabel=NULL,
                               colName="x",
                               minBin=NULL,
                               maxBin = NULL,
                               interval=100,
                               logScale=FALSE,
                               logBase=exp(1),
                               normalize=FALSE,
                               colors = c("#7cb5ec", "#000000")) {
    series <- list()
    plotLines <- list()
    col <- which(names(data[[1]])==colName)[1]
    actualInterval <- interval
    for (i in 1:length(data)) {
        maxBin <- max(maxBin, max(data[[i]][, col], na.rm=TRUE), na.rm=TRUE)
        minBin <- min(minBin, min(data[[i]][, col], na.rm=TRUE), na.rm=TRUE)
    }
    if (logScale) {
        maxBin <- log(maxBin + 1, base=logBase)
        minBin <- log(minBin + 1, base=logBase)
        actualInterval <- log(interval, base=logBase)
    }

    for (i in 1:length(data)){
        x <- as.vector(as.matrix(data[[i]][, col]))
        if (logScale) {
            x <- log(x + 1, base=logBase)
        }

        plotLines[[i * 2 - 1]] <-
            list(color=colors[i],
                 value=mean(x),
                 width=2,
                 label=list(text="mean", style=list(color=colors[i]), verticalAlign="middle"))
        plotLines[[i * 2]] <-
            list(color=colors[i],
                 value=median(x),
                 dashStyle="dash",
                 width=2,
                 label=list(text="median", style=list(color=colors[i]), verticalAlign="middle"))

        histogram <- hist(x, breaks=seq(minBin, maxBin + actualInterval, actualInterval), plot=FALSE)
        histNames <- getBinItemList(data[[i]], interval=actualInterval)

        nBins <- min(length(histogram$breaks), length(histogram$counts))
        counts <- histogram$counts[1:nBins]
        if (normalize) {
            counts <- 100 * counts / nrow(data[[i]])
        }
        breaks <- c(histogram$breaks[2:nBins], histogram$breaks[nBins] + actualInterval)
        bins <- getValues(
            breaks,
            counts,
            name=histNames)
        series[[i]] <- list(name=names[i], data=bins)
    }
   
    chart <- Highcharts$new()
    chart$chart(type="column")
    chart$plotOptions(
        column="{ grouping: false, pointPadding: 0, borderWidth: 0, groupPadding: 0, shadow: false}")
    chart$xAxis(title=paste("{text: '", xLabel, "'}", sep=""),
                plotLines=plotLines)
    if (is.null(yLabel)) {
        if (normalize) {
            yLabel <- "density (%)"
        } else {
            yLabel <- "count"
        }
    }
    chart$yAxis(title=paste("{text: '", yLabel, "'}", sep=""))
    chart$set(series=series)
    return(chart)
}

# getTimelapseLinePlot
# data[[]]$x: Stats
# data[[]]$date: Date
getTimelapseLinePlot = function(data, names, yLabel, colName="x", verticalLineDate=NULL, timezone="UTC") {
    series <- list()
    col <- which(names(data[[1]])==colName)[1]
    for (i in 1:length(data)){
        timelapseValues <- getTimelapseValues(
            as.POSIXlt(strptime(as.character(data[[i]]$date), "%Y-%m-%d", tz=timezone)),
            data[[i]][, col])
        series[[i]] <- list(name=names[i], data=timelapseValues)
    }


    chart <- Highcharts$new()
    xAxis <- list(type="datetime")
    if (!is.null(verticalLineDate)){
        date <- as.POSIXlt(strptime(as.character(verticalLineDate), "%Y-%m-%d", tz=timezone))
        xAxis[["plotLines"]] <- paste("[{color: 'red',",
                                     "value: Date.UTC(", date$year + 1900, ",", date$mon, ",", date$mday, "),",
                                     "width: 2}]", sep="")
    }
    chart$set(xAxis=xAxis)
    chart$yAxis(title=paste("{text: '", yLabel, "'}", sep=""), gridLineColor="#FFFFFF")
    chart$set(series=series)
    return(chart)
}


# Difference-in-difference plot
# Use with DiffInDiffAggregate function
diffInDiffPlot = function(data,
                     idCol,
                     xCol,
                     idLabelCol=NULL,
                     xLabel="period",
                     yLabel="change",
                     periodNames=NULL,
                     legendStyle=list(align="right", verticalAlign="top", layout="vertical")
                     ) {
    dataChart <- Highcharts$new()
    ids <- unique(data[, idCol])
    numPeriod <- 0
    if (is.null(idLabelCol)) {
        idLabelCol = idCol
    }
    for (i in 1:length(ids)) {
        current <- data[data[, idCol] == ids[i],]
        numPeriod <- nrow(current)
        name <- current[1,][, idLabelCol]
        x <- seq(0, numPeriod - 1, 1)
        y <- current[, xCol]
        z <- current[, xCol]
        
        seriesData <- getValues(x, y, z, name)
        visible <- TRUE
        dataChart$series(name=name,
                         data=seriesData,
                         showInLegend=TRUE,
                         visible=visible)
    }
    if (is.null(periodNames)) {
        periodNames <- paste("period", x)
    }
    dataChart$xAxis(categories=periodNames)
    dataChart$yAxis(title=list(text=yLabel), gridLineColor="#FFFFFF")
    do.call(dataChart$legend, c(legendStyle))
    dataChart$tooltip(pointFormat=getPointFormat(y=yLabel, z=NULL))
    return (dataChart)
}

