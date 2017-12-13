# Caches the query result in rds file under `getwd()`/oracle_cache.
# It assumes
#   oracle_host, oracle_port, oracle_sid, oracle_dbname,
#   oracle_user, oracle_password
# variables in the env.
# Remove the cache files if the update value is needed.
# Expire:
#     positive integer n: Expire if the existing cache is older than n minutes
#     0: Expire now
#     -1: Always use the existing cache if it exists
cachedOracleQuery = function(queryName, query, expire=-1, cachePath="./oracle_cache") {
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
    
    require(ROracle)
    drv <- Oracle()
    connect.string <- paste(
    "(DESCRIPTION=",
    "(ADDRESS=(PROTOCOL=tcp)(HOST=", oracle_host, ")(PORT=", oracle_port, "))",
    "(CONNECT_DATA=(SID=", oracle_sid, ")))", sep = "")
    con <- dbConnect(drv, username=oracle_user, password=oracle_password,
                     dbname=connect.string)    
    tryCatch(
        {
            df <- dbGetQuery(con, query)
        },
        finally={
            message("Disconnecting from the database server.")
            dbDisconnect(con)
        }
    )
    
    saveRDS(df, rdsFileName)
    return(df)
}
