# Caches the query result in rds file under `getwd()`/psql_cache.
# It assumes pg_host, pg_port, pg_dbname, pg_user, pg_password variables in the env.
# Remove the cache files if the update value is needed.
# Expire:
#     positive integer n: Expire if the existing cache is older than n minutes
#     0: Expire now
#     -1: Always use the existing cache if it exists
cachedPsqlQuery = function(queryName, query, expire=-1, cachePath="./psql_cache") {
    require(RPostgreSQL)
    
    if (!dir.exists(cachePath)) {
        dir.create(cachePath)
    }
    
    rdsFileName <- paste(cachePath, "/", queryName, ".rds", sep="")
    if (file.exists(rdsFileName)) {
        modifiedAt <- file.mtime(rdsFileName)
        mins <- as.integer(difftime(Sys.time(), modifiedAt, units <- "mins"))
        if (expire < 0 || mins < expire) {
            return(readRDS(rdsFileName))
        }
    }
    
    message(paste(queryName, "not cached. Fetching from the database."))
    
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, host=pg_host, dbname=pg_dbname, port=pg_port, user=pg_user,
                     password=pg_password)
    tryCatch(
        {
            result <- dbSendQuery(con, query)
            df <- fetch(result, n=-1)
        },
        finally={
            message("Disconnecting from the database server.")
            dbDisconnect(con)
        }
    )
    
    saveRDS(df, rdsFileName)
    return(df)
}
