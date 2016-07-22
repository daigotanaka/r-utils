# Modify stdout by rChart so I can customize tooltip and etc
require(rCharts)

renderChart = function (ct, chartId, include_assets = F, ...) 
{
    add_ext_widgets(ct$lib)
    assetHTML <- ifelse(include_assets, paste(paste(add_lib_assets(ct$lib, 
        ...), collapse = "\n"), "\n", add_style_(ct$params$width, 
        ct$params$height), collapse = "\n"), "")
    chartDiv = render_template(ct$templates$chartDiv, list(chartId = chartId, 
        lib = ct$LIB$name, container = ct$container))
    selfHtml = ct$html(chartId)
    selfHtml = sub('\\"function([^"]*)\\"', 'function\\1', selfHtml)
    selfHtml = gsub('\\"\\{([^"]*)\\}\\"', '\\{\\1\\}', selfHtml)
    selfHtml = gsub('\\"\\[([^"]*)\\]\\"', '\\[\\1\\]', selfHtml)
    writeLines(c(assetHTML, chartDiv, selfHtml))
}

getValues = function(x, y, z=NULL, name=NULL, color=NULL){
    values = paste("{ x:", x, ", y:", y, sep="")
    if (!is.null(z)) {
        values = paste(values, ", z:", z, sep="")
    }
    if (!is.null(name)) {
        values = paste(values, ", name:'", gsub('"', "", gsub("'", "", name)),
                       "'", sep="")
    }
    if (!is.null(color)) {
        values = paste(values, ", marker: { fillColor: '", color, "'}", sep="")
    }
    values = paste(values, "}", sep="")
    return(values)
}

getTimelapseValues = function(date, y, z=NULL, name=NULL){
    values = paste("{ x: Date.UTC(", date$year + 1900, ",", date$mon, ",", date$mday, ")",
       ", y:", y)
    if (!is.null(z)) {
        values = paste(values, ", z:", z)

    }
    if (!is.null(name)) {
        values = paste(values, ", name:'", gsub('"', "", gsub("'", "", name)), "'")
    }
    values = paste(values, "}")
    return(values)
}

getPointFormat = function(y, z){
    paste("<div>{point.name}<br/>",
          y, ":{point.y}<br/>",
          z, ": {point.z}</div>")
}
