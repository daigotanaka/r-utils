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
        unixTimestamps = 1000 * as.numeric(as.POSIXct( sort(unique(allMetrics[[1]]$date))))
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
getStackedHistogram = function(data, names, xLabel, interval=100, logScale=FALSE, logBase=exp(1), normalize=FALSE) {
    series = list()
    for (i in 1:length(data)){
        x = data[[i]]$x
        maxBin = max(data[[i]]$x)
        actualInterval = interval

        if (logScale) {
            x = log(x + 1, base=logBase)
            maxBin = log(maxBin + 1, base=logBase)
            actualInterval = log(interval, base=logBase)
        }
        histogram = hist(x, breaks=seq(0, maxBin + actualInterval, actualInterval), plot=FALSE)
        histNames = getBinItemList(data[[i]], interval=actualInterval)

        nBins = min(length(histogram$breaks), length(histogram$counts))
        counts = histogram$counts[1:nBins]
        if (normalize) {
            counts = counts / nrow(data[[i]])
        }
        bins = getValues(
            histogram$breaks[1:nBins],
            counts,
            name=histNames)
        series[[i]] = list(name=names[i], data=bins)
    }
   
    chart <- Highcharts$new()
    chart$chart(type="column")
    chart$plotOptions(
        column="{ grouping: false, pointPadding: 0, borderWidth: 0, groupPadding: 0, shadow: false}")
    chart$xAxis(title=paste("{text: '", xLabel, "'}", sep=""))
    yLabel = "frequency"
    if (normalize) {
        yLabel = paste(yLabel, "(normalized)")
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
