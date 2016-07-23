# The functions require rchart-helper.R preloaded

# getQ2TimelapsePlot
# data[[]]$x: Stats
# data[[]]$date: Date
getQ2TimelapsePlot = function(data, names, colors, yLabel, verticalLineDate=NULL) {
    series = list()
    for (i in 1:length(data)) {
        dateFactor = as.factor(data[[i]]$date)
        boxplot = boxplot(data[[i]]$x ~ dateFactor,
                          data=data.frame(dateFactor, date[[i]]$x), plot=FALSE)
        stats = setNames(as.data.frame(boxplot$stats), nm=NULL)

        series[[2 * (i - 1) + 1]] =
            list(name=names[i], data=stats[3,], zIndex=1,
                 marker=list(fillColor="white", lineWidth=2, lineColor=colors[i]))
        series[[2 * i]] = list(name="50th quartile", data=stats[c(2,4),], zIndex=0,
                 type="arearange", color=colors[i], lineWidth=0, linkedTo=":previous", fillOpacity=0.3)
    }

    chart = Highcharts$new()
    if (!is.null(verticalLineDate)){
        verticalLinePos = which(levels(v3DateFactor)==verticalLineDate) - 1
        chart$xAxis(
            categories=levels(v2DateFactor), 
            title=list(text = "Date"),
            plotLines=list(list(
                color="red",
                value=verticalLinePos,
                width=2)))
    } else {
        chart$xAxis(
            categories=levels(v2DateFactor), 
            title=list(text = "Date"))
    }
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
# data[[]]$count
# data[[]]$name
getStackedHistogram = function(data, names, xLabel, interval=100) {
    series = list()
    for (i in 1:length(data)){
        histogram = hist(data[[i]]$count, breaks=seq(0, max(data[[i]]$count) + interval, interval), plot=FALSE)
        histNames = getBinItemList(data[[i]], interval=interval)

        nBins = min(length(histogram$breaks), length(histogram$counts))
        bins = getValues(
            histogram$breaks[1:nBins],
            histogram$counts[1:nBins],
            name=histNames)
        series[[i]] = list(name=names[i], data=bins)
    }
   
    chart <- Highcharts$new()
    chart$chart(type="column")
    chart$plotOptions(
        column="{ pointPadding: 0, borderWidth: 0, groupPadding: 0, shadow: false}")
    chart$xAxis(title=paste("{text: '", xLabel, "'}", sep=""))
    chart$yAxis(title="{text: 'frequency'}")
    chart$set(series=series)
    return(chart)
}

getTimelapseLinePlot = function(data, names, yLabel, timezone="UTC") {
    series = list()
    for (i = 1:length(data)){
        timelapseValues = getTimelapseValues(
            as.POSIXlt(strptime(as.character(data[[i]]$date), "%Y-%m-%d", tz=timezone)),
            data[[i]]$x)
        series[[i]] = list(name=names[i], data=timelapseValues)
    }

    chart <- Highcharts$new()
    chart$xAxis(type="datetime")
    chart$yAxis(title=paste("{text: '", yLabel, "'}", sep=""), gridLineColor="#FFFFFF")
    chart$set(series=series)
    return(chart)
}
