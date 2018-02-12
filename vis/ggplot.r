require(ggplot2)
require(ggthemes)
require(GGally)

addTheme =
function(myPlot) {
    return(myPlot +
               theme_economist() # +
#                scale_color_solarized("blue") +
#                scale_fill_solarized("blue")
    )
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
         groupLab="",
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
    for (index in 1:length(variations)) {
        groupName = groupNames[index]
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

    for (index in 1:length(variations)) {
        groupName = groupNames[index]
        groups[[groupName]] =
            filter(groups[[groupName]], metric < xLims[2] + 0.5 * inc)
        groups[[groupName]]$scale = nrow(groups[[groupName]]) / baseNums[groupName]
    }

    stats = groups[[groupNames[1]]][0,]
    for (index in 1:length(variations)) {
        groupName = groupNames[index]
        stats = rbind(stats, groups[[groupName]])
    }

    stats$group =
        factor(stats$group, levels=groupNames)

    xInc = ifelse(xLims[2] <= inc, inc / 10,
                  inc^floor(log(xLims[2] - 1, inc)))
    interval = xInc * ceiling(xLims[2] / 10 / xInc)

    histDatas = list()
    densDatas = list()

    for (index in 1:length(variations)) {
        groupName = groupNames[index]
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


    if (FALSE) {
        for (index in 1:length(variations)) {
            groupName = groupNames[index]
            if (index == 1) {
                densData =
                    data.frame(breaks=histDatas[[groupName]]$breaks,
                               density=histDatas[[groupName]]$count / baseNums[groupName])
            } else {
                curDensData =
                    data.frame(breaks=histDatas[[groupName]]$breaks,
                               density=histDatas[[groupName]]$count / baseNums[groupName])
                densData = rbind(densData, curDensData)
            }
        }
        densityPlot =
            ggplot(data=densData, aes(x=breaks, y=density * 100)) + geom_bar(position="identity")
    } else {

    densData = densDatas[[groupNames[1]]][0,]
    for (index in 1:length(variations)) {
        groupName = groupNames[index]
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
             y=yLab,
             fill=groupLab) +
        scale_x_continuous(limits=c(0, xLims[2]),
                           breaks=seq(0, xLims[2], interval))
    yLims = c(min(densData$y) * 100, max(densData$y) * 100)
    }

    height = yLims[2]
    medians$label = paste("group=", medians$group, "\n",
                          "median=", medians$median, sep="")
    medians$labelPosY = height * (1.0 - 0.2 * seq(1, length(variations), 1))
    medians$xMax = xLims[2]  # Same value for both ends
    medians$yMax = yLims[2]  # Same value for both ends

    means$label = paste("group=", means$group, "\n",
                        "mean=", means$mean, sep="")
    means$labelPosY = height * (0.65 - 0.2 * seq(1, length(variations), 1))
    means$xMax = xLims[2]  # Same value for both ends
    means$yMax = yLims[2]  # Same value for both ends

    densityPlot = summaryStatsLine(densityPlot, medians, "median")
    densityPlot = summaryStatsLine(densityPlot, means, "mean")

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
    meanPred = mean(predict(model, type="response"))
    for (predName in predNames) {
        coeff = coeffs[predName, ]
        curEst = coeff[1]
        lower = coeff[1] - z * coeff[2]
        upper = coeff[1] + z * coeff[2]
        if (logit) {
            curEst = 100 * (gtools::inv.logit(gtools::logit(meanPred) + curEst) - meanPred)
            lower = 100 * (gtools::inv.logit(gtools::logit(meanPred) + lower) - meanPred)
            upper = 100 * (gtools::inv.logit(gtools::logit(meanPred) + upper) - meanPred)
        }
        estimate =
            rbind(estimate,
                  data.frame(estimate=curEst,
                             conf.low=lower, conf.high=upper,
                             term=predCaptions[predName]))
    }
    estimate$term <- factor(estimate$term, estimate$term)
    coeffPlot = ggcoef(estimate, exponentiate = FALSE)
    coeffPlot =
        coeffPlot +
            geom_text(data=estimate,
              aes(x=estimate + .02,
                  y=term,
                  label=signif(estimate, 2),
                  hjust=0,
                  vjust=1),
              nudge_y=0.2
              ) +
        scale_alpha_continuous(range=c(0.0, 1.0), trans="identity") +
        guides(alpha=FALSE) +
        labs(title=title,
             x=outcomeCaption,
             y="")
#     if (logit) {
#        coeffPlot = coeffPlot + scale_x_continuous(limits=c(-100, 100))
#    }
}
