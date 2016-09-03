# The functions require rchart-helper.R preloaded

# getQ2TimelapsePlot
# data[[]]$x: Stats
# data[[]]$date: Date
getQ2TimelapsePlot = function(data, names, colors, yLabel, verticalLineDate=NULL, timezone="UTC") {
    series = list()
    dateFactors = list()
    for (i in 1:length(data)) {
        dateFactors[[i]] = as.factor(data[[i]]$date)
        boxplot = boxplot(data[[i]]$x ~ dateFactors[[i]],
                          data=data.frame(dateFactors[[i]], data[[i]]$x), plot=FALSE)
        stats = setNames(as.data.frame(boxplot$stats), nm=NULL)

        # Timpstamp in miliseconds
        unixTimestamps =
            1000 * as.numeric(as.POSIXct(sort(unique(data[[i]]$date)),
                                         origin="1970-01-01"))
        statsMedian = rbind(setNames(unixTimestamps, nm=NULL), stats[3,])
        statsQ2 = rbind(setNames(unixTimestamps, nm=NULL), stats[c(2, 4),])

        series[[2 * (i - 1) + 1]] =
            list(name=names[i], data=statsMedian, zIndex=1, color=colors[i],
                 marker=list(fillColor="white", lineWidth=2, lineColor=colors[i]))
        series[[2 * i]] = list(name="50th quartile", data=statsQ2, zIndex=0,
                 type="arearange", color=colors[i], lineWidth=0, linkedTo=":previous", fillOpacity=0.3)
    }

    chart = Highcharts$new()
    xAxis = list(type="datetime")
    if (!is.null(verticalLineDate)){
        date = as.POSIXlt(strptime(as.character(verticalLineDate), "%Y-%m-%d", tz=timezone))
        xAxis[["plotLines"]] = paste("[{color: 'red',",
                                     "value: Date.UTC(", date$year + 1900, ",", date$mon, ",", date$mday, "),",
                                     "width: 2}]", sep="")
    }
    chart$set(xAxis=xAxis)
    chart$yAxis(title=list(text=yLabel), min=0)
    chart$set(series=series)
    return(chart)
}

# Helper for creating histogram
getBinItemList = function(data, businesses, interval=100) {
    binItemList = c()
    currentBin = interval
    maxBin = max(data$count) + interval
    while (currentBin < maxBin) {
        items = filter(data, currentBin - interval <= count & count < currentBin)
        binItemList = c(binItemList,
                        paste("< ", currentBin, "<br>",
                              paste(items$name, collapse="<br>, ")))
        currentBin = currentBin + interval
    }
    return(binItemList)
}

# getStackedHistogram
# data[[]]$x
getStackedHistogram = function(data,
                               names,
                               xLabel,
                               minBin=NULL,
                               maxBin = NULL,
                               interval=100,
                               logScale=FALSE,
                               logBase=exp(1),
                               normalize=FALSE,
                               colors = c("#7cb5ec", "#000000")) {
    series = list()
    plotLines = list()

    actualInterval = interval
    for (i in 1:length(data)) {
        maxBin = max(maxBin, max(data[[i]]$x, na.rm=TRUE), na.rm=TRUE)
        minBin = min(minBin, min(data[[i]]$x, na.rm=TRUE), na.rm=TRUE)
    }
    if (logScale) {
        maxBin = log(maxBin + 1, base=logBase)
        minBin = log(minBin + 1, base=logBase)
        actualInterval = log(interval, base=logBase)
    }

    for (i in 1:length(data)){
        x = data[[i]]$x
        if (logScale) {
            x = log(x + 1, base=logBase)
        }

        plotLines[[i * 2 - 1]] =
            list(color=colors[i],
                 value=mean(x),
                 width=2,
                 label=list(text="mean", style=list(color=colors[i]), verticalAlign="middle"))
        plotLines[[i * 2]] =
            list(color=colors[i],
                 value=median(x),
                 dashStyle="dash",
                 width=2,
                 label=list(text="median", style=list(color=colors[i]), verticalAlign="middle"))

        histogram = hist(x, breaks=seq(minBin, maxBin + actualInterval, actualInterval), plot=FALSE)
        histNames = getBinItemList(data[[i]], interval=actualInterval)

        nBins = min(length(histogram$breaks), length(histogram$counts))
        counts = histogram$counts[1:nBins]
        if (normalize) {
            counts = 100 * counts / nrow(data[[i]])
        }
        breaks = c(histogram$breaks[2:nBins], histogram$breaks[nBins] + actualInterval)
        bins = getValues(
            breaks,
            counts,
            name=histNames)
        series[[i]] = list(name=names[i], data=bins)
    }
   
    chart <- Highcharts$new()
    chart$chart(type="column")
    chart$plotOptions(
        column="{ grouping: false, pointPadding: 0, borderWidth: 0, groupPadding: 0, shadow: false}")
    chart$xAxis(title=paste("{text: '", xLabel, "'}", sep=""),
                plotLines=plotLines)
    yLabel = "count"
    if (normalize) {
        yLabel = "density (%)"
    }
    chart$yAxis(title=paste("{text: '", yLabel, "'}", sep=""))
    chart$set(series=series)
    return(chart)
}

# getTimelapseLinePlot
# data[[]]$x: Stats
# data[[]]$date: Date
getTimelapseLinePlot = function(data, names, yLabel, verticalLineDate=NULL, timezone="UTC") {
    series = list()
    for (i in 1:length(data)){
        timelapseValues = getTimelapseValues(
            as.POSIXlt(strptime(as.character(data[[i]]$date), "%Y-%m-%d", tz=timezone)),
            data[[i]]$x)
        series[[i]] = list(name=names[i], data=timelapseValues)
    }


    chart = Highcharts$new()
    xAxis = list(type="datetime")
    if (!is.null(verticalLineDate)){
        date = as.POSIXlt(strptime(as.character(verticalLineDate), "%Y-%m-%d", tz=timezone))
        xAxis[["plotLines"]] = paste("[{color: 'red',",
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
diffInDiffPlot <- function(data,
                     idCol,
                     xCol,
                     yLabel="change",
                     periodNames=NULL
                     ) {
    dataChart <- Highcharts$new()
    ids = unique(data[, idCol])
    numPeriod = 0
    for (i in 1:length(ids)) {
        current = data[data[, idCol] == ids[i],]
        numPeriod = nrow(current)
        name = current[1,]$name
        x = seq(0, numPeriod - 1, 1)
        y = current[, xCol]
        z = current[, xCol]
        
        seriesData = getValues(x, y, z, name)
        visible = TRUE
        dataChart$series(name=name,
                         data=seriesData,
                         showInLegend=TRUE,
                         visible=visible)
    }
    if (is.null(periodNames)) {
        periodNames = paste("period", x)
    }
    dataChart$xAxis(categories=periodNames)
    dataChart$yAxis(title=list(text=yLabel), gridLineColor="#FFFFFF")
    dataChart$legend(align="right", verticalAlign="top", layout="vertical")
    dataChart$tooltip(pointFormat=getPointFormat(y=yLabel, z=xCol))
    return (dataChart)
}


