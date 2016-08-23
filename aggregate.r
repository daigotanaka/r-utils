# For diff-in-diff data, first group by ID, then aggregate by
# period. Each period has the length of N rows
diffInDiffAggregate =
    function(data,
             idCol="id",  # Identifier of the group
             xCol="x",  # Data to aggregate
             timeCol="date",
             interval=7,  # 1 interval=N rows: Each period has N rows
             normalizeByPeriod=NULL, # Set N-th period (1, 2, 3...)
             FUNC=sum) {
        require(dplyr)
        
        totalAggData = NULL
        ids  = unique(data[,idCol])
        for (i in 1:length(ids)){
            current = data[data[, idCol] == ids[i],]
            current = current[order(current[,timeCol]),]
            aggData = NULL
            for (j in 1:(nrow(current) / interval)) {
                x = FUNC(current[seq((j - 1) * interval + 1,
                                    min(nrow(current), j * interval),
                                    1),
                                xCol])
                time = current[(j - 1) * interval + 1, timeCol]
                agg = data.frame(id=ids[i], time=time, x=x)
                names(agg) = c(idCol, timeCol, xCol)
                
                if (is.null(aggData)) {
                    aggData = agg
                } else {
                    aggData = rbind(aggData, agg)
                }
            }
           
            if (!is.null(normalizeByPeriod)) {
                base = aggData[normalizeByPeriod, xCol]
                if (is.null(base) | is.na(base) | base == 0) {
                    message(paste("Skipped", aggData[1, idCol], "because of normalization by zero"))
                    next()
                }
                aggData[, xCol] = aggData[, xCol] / base
            }
            
            if (is.null(totalAggData)) {
                totalAggData = aggData
            } else {
                totalAggData = rbind(totalAggData, aggData)
            }
        }
        return (totalAggData)
}
