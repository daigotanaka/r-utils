require(ggplot2)
require(ggthemes)
require(GGally)

addTheme =
function(myPlot) {
    return(myPlot +
               theme_economist() +
               scale_color_solarized("blue") +
               scale_fill_solarized("blue"))
}

summaryStatsLine =
function(baseHist, summaryStats, statsName) {
    summaryStats$x = summaryStats[,statsName]
    return (
        baseHist +
        geom_rect(data=summaryStats,
                  aes(xmin=x - xMax * 0.002,
                      xmax=x + xMax * 0.002,
                      ymin=0,
                      ymax=Inf,
                      fill=group)) +
        geom_rect(data=summaryStats,
                  aes(xmin=x,
                      xmax=x + xMax * 0.06,
                      ymin=labelPosY + 10 - 0.15,
                      ymax=labelPosY + 10 + 0.15,
                      fill=group)) +
        geom_text(data=summaryStats,
                  aes(x=x + xMax * 0.075,
                      y=labelPosY + 10 + 1,
                      label=label,
                      hjust=0,
                      vjust=1))
    )
}

getDensityPlot =
function(data,
         metric,
         idVarName,
         groupVarName,
         title="",
         xLab="",
         yLab="",
         groupNames=NULL) {
    variations = sort(unique(data[,groupVarName]))
    groups = list()
    baseNums = c()
    if (is.null(groupNames)) {
        groupNames = variations
    }
    for (index in 1:length(variations)) {
        var = variations[index]
        groupName = groupNames[index]
        groups[[groupName]] =
            filter_(data, paste(groupVarName, "== '", var, "'", sep="")) %>%
            select_(idVarName, metric) %>%
            arrange_(idVarName)
        groups[[groupName]]$metric = groups[[groupName]][,metric]
        groups[[groupName]]$group = groupName 
        baseNums[groupName] = nrow(groups[[groupName]])
    }
    
    # Calculate summary stats from the entire data first
    stats = groups[[groupNames[1]]][0,]
    for (groupName in groupNames) {
        stats = rbind(stats, groups[[groupName]])
    }
    
    medians = 
        group_by(stats, group) %>% 
        summarise(median=median(metric))
    means = 
        group_by(stats, group) %>% 
        summarise(mean=mean(metric))
    
    # Truncate    
    inc = 10
    qRange = c(0, 0.85)
    xLims = c(0, ceiling(quantile(stats$metric, qRange[2]) / inc) * inc)
    
    for (groupName in groupNames) {
        groups[[groupName]] =
            filter(groups[[groupName]], metric < xLims[2] + 0.5 * inc)
        groups[[groupName]]$scale = nrow(groups[[groupName]]) / baseNums[groupName] 
    }
    
    stats = groups[[groupNames[1]]][0,]
    for (groupName in groupNames) {
        stats = rbind(stats, groups[[groupName]])
    }
    
    stats$group =
        factor(stats$group, levels=groupNames)    
    
    xInc = ifelse(xLims[2] <= inc, inc / 10,
                  inc^floor(log(xLims[2] - 1, inc)))
    interval = xInc * ceiling(xLims[2] / 10 / xInc)
    
    histDatas = list()
    densDatas = list()
    for (groupName in groupNames) {
        histDatas[[groupName]] =
            hist(groups[[groupName]]$metric,
                 breaks=seq(0,
                            xLims[2] + inc,
                            interval),
                 plot=FALSE)
    
        densDatas[[groupName]] =
            as.data.frame(
                spline(histDatas[[groupName]]$breaks[1:length(histDatas[[groupName]]$counts)],
                       histDatas[[groupName]]$counts / baseNums[groupName],
                       n=1000))
        densDatas[[groupName]]$group = groupName
    }
    
    densData = densDatas[[groupNames[1]]][0,]
    for (groupName in groupNames) {
        densData =
            rbind(densData, densDatas[[groupName]])
    }
    
    densData$group = 
        factor(densData$group, levels=groupNames)
    
    densityPlot =
        ggplot() +
        geom_area(data=densData,
                  aes(x=x, y=y * 100, fill=group),
                  alpha=0.5,
                  position="identity") +
        geom_line(data=densData,
                  aes(x=x, y=y * 100, group=group)) +
        labs(title=title,
             x=xLab,
             y=yLab) +
        scale_x_continuous(limits=c(0, xLims[2]),
                           breaks=seq(0, xLims[2], interval))
    
    yLims = c(min(densData$y) * 100, max(densData$y) * 100)
    height = yLims[2]
    medians$label = paste("median \n(", medians$group, ")", sep="")
    medians$labelPosY = c(height * 0.8, height * 0.6)
    medians$xMax = c(xLims[2], xLims[2])  # Same value for both ends
    medians$yMax = c(yLims[2], yLims[2])  # Same value for both ends
    
    means$label = paste("mean \n(", means$group, ")", sep="")   
    means$labelPosY = c(height * 0.45, height * 0.25)
    means$xMax = c(xLims[2], xLims[2])  # Same value for both ends
    means$yMax = c(yLims[2], yLims[2])  # Same value for both ends 
    
    densityPlot = summaryStatsLine(densityPlot, medians, "median")
    densityPlot = summaryStatsLine(densityPlot, means, "mean")
    
    densityPlot = addTheme(densityPlot)
    return (densityPlot)
}

densityGeom = function(mean, sd) {
    h <- hist(rnorm(10000, mean, sd), plot=FALSE)
    d <- as.data.frame(
        spline(x=h$breaks[1:length(h$counts)],
               y=h$counts / 10000,
               n=10000))
    return(d)
}

regressionEstimatePlot =
function(model,
         title,
         outcomeCaption,
         predNames,
         predCaptions=NULL,
         logit=FALSE,
         z=1.96 # 95% c.i. 1.28 for 80%
         ) {        
    if (is.null(predCaptions)) {
        predCaptions = predNames
    }
    if (is.null(names(predCaptions))){
        names(predCaptions) = predNames
    }
    coeffs = summary(model)$coefficients
    coeffs = coeffs[predNames,]
    predNames = predNames[order(coeffs[,1], decreasing=FALSE)]
    predCaptions = predCaptions[order(coeffs[,1], decreasing=FALSE)]
    estimate = data.frame(estimate=c(), conf.low=c(), conf.high=c(), term=c())
    scale = ifelse(logit, 100, 1)
    for (predName in predNames) {
        coeff = coeffs[predName, ]
        lower = scale * (coeff[1] - z * coeff[2])
        upper = scale * (coeff[1] + z * coeff[2])
        estimate =
            rbind(estimate,
                  data.frame(estimate=scale * coeff[1],
                             conf.low=lower, conf.high=upper,
                             term=predCaptions[predName]))
    }
    estimate$term <- factor(estimate$term, estimate$term)
    coeffPlot = ggcoef(estimate, exponentiate = FALSE)
    coeffPlot =
        coeffPlot +
            geom_text(data=estimate,
              aes(x=estimate + 2,
                  y=term,
                  label=signif(estimate, 2),
                  hjust=0,
                  vjust=1),
              nudge_y=0.4
              ) +
        scale_alpha_continuous(range=c(0.0, 1.0), trans="identity") +
        guides(alpha=FALSE) +
        labs(title=title,
             x=outcomeCaption,
             y="")
}

regressionEstimatePlot1 =
function(model,  # Regression model
         title,
         outcomeCaption,
         predNames,
         predCaptions=NULL) {
     if (is.null(predCaptions)) {
        predCaptions = predNames
        names(predCaptions) = predNames
    }
    coeffs = summary(model)$coefficients
    coeffs = coeffs[predNames,]
    predNames = predNames[order(coeffs[,1], decreasing=FALSE)]
    predCaptions = predCaptions[order(coeffs[,1], decreasing=FALSE)]
    pdfs = data.frame(effect=c(), predictor=c())
    estimate = data.frame(mean=c(), lower=c(), upper=c(), predictor=c())
    for (predName in predNames) {
        coeff = coeffs[predName, ]
        z = 1.96 # 95% c.i.
        # z = 1.28 # 80% c.i.
        lower = 100 * (coeff[1] - z * coeff[2])
        upper = 100 * (coeff[1] + z * coeff[2])
        pdf = data.frame(effect=rnorm(10000, coeff[1] * 100, coeff[2] * 100),
                             predictor=rep(predCaptions[predName], 10000))
        # pdf = filter(pdf, lower <= effect & effect <= upper)
        pdfs = rbind(pdfs, pdf)
        estimate =
            rbind(estimate,
                  data.frame(mean=100 * coeff[1],
                             lower=lower, upper=upper,
                             predictor=predCaptions[predName]))
    }
    pdfs$predictor = factor(pdfs$predictor, levels=predCaptions)

    coeffPlot =
        ggplot(data=pdfs,
               aes(x=effect, predictor)) +
        stat_density(aes(alpha=..density..),
                     geom ="raster",
                     position = "identity",
                     kernel="gaussian",
                     adjust=1,
                     bw=5
                     ) +
        geom_point(data=estimate, aes(x=mean, y=predictor), color="white", size=3) +
        geom_point(data=estimate, aes(x=mean, y=predictor), color="black", size=2) +
        # geom_point(data=estimate, aes(x=lower, y=predictor), color="black", size=1) +
        # geom_point(data=estimate, aes(x=upper, y=predictor), color="black", size=1) +
        geom_text(data=estimate,
              aes(x=mean + 6,
                  y=predictor,
                  label=paste(signif(mean, 3), "%", sep=""),
                  hjust=0,
                  vjust=1),
              nudge_y=0.4
              ) +
        scale_alpha_continuous(range=c(0.0, 1.0), trans="identity") +
        guides(alpha=FALSE) +
        labs(title=title,
             x=outcomeCaption,
             y="") +
        theme_economist_white()
    return(coeffPlot)
}

regressionEstimatePlot2 =
function(model,  # Regression model
         title,
         outcomeCaption,
         predNames,
         predCaptions=NULL,
         yInterval=1,
         xLims=c(-20, 50)) {
    if (is.null(predCaptions)) {
        predCaptions = predNames
        names(predCaptions) = predNames
    }
    coeffs = summary(model)$coefficients
    coeffs = coeffs[order(coeffs[,1], decreasing=TRUE),]
    
    numPredictors = length(predNames)
    yOffsets = seq(0, yInterval * (numPredictors - 1), yInterval)
    coeffPlot = ggplot()
    densities = list()
    labels = list()
    
    i = 0
    for (predName in predNames) {
        i = i + 1
        coeff = coeffs[predName, ]
        densities[[predName]] = densityGeom(coeff[1] * 100, coeff[2] * 100)
        densities[[predName]]$yOffset = rep(yOffsets[i], nrow(densities[[i]]))
        labels[[predName]] =
            data.frame(yOffset=yOffsets[i],
                       predictor=predCaptions[predName],
                       estimate=coeffs[predName, "Estimate"] * 100)
        coeffPlot = coeffPlot +
            geom_polygon(data=densities[[predName]],
                        aes(x=x, y=y + yOffset)) +
            geom_text(data=labels[[predName]],
                      aes(x=xLims[1],
                          y=yOffset + 0.5,
                          label=predictor,
                          hjust=0,
                          vjust=1)) +
            geom_text(data=labels[[predName]],
                      aes(x=estimate,
                          y=yOffset + 0.7,
                          label=paste(round(estimate), "%"),
                          hjust=0,
                          vjust=1))
    }
    
    coeffPlot = coeffPlot + 
        geom_rect(xmin=-1,
              xmax=1,
              ymin=-Inf,
              ymax=Inf) +
        scale_x_continuous(limits=c(xLims[1], xLims[2])) +
        scale_y_continuous(limits=c(0, numPredictors * yInterval),
                           breaks=seq(0, numPredictors, 1)) +
        labs(title=title,
             x=outcomeCaption,
             y="") +
        theme_economist() + theme(axis.text.y=element_blank())
    
    return(coeffPlot)
}
