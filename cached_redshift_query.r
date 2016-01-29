# Caches the query result in rds file under `getwd()`/redshift_cache.
# It assumes
#     redshiftJdbcURL
#     redshiftJdbcPort
#     redshiftDatabase
#     redshiftUsername
#     redshiftPassword
# variables in the env.
# Remove the cache files if the update value is needed.
# Expire:
#     positive integer n: Expire if the existing cache is older than n minutes
#     0: Expire now
#     -1: Always use the existing cache if it exists
cachedRedshiftQuery <-
    function(
        queryName,
        query,
        expire=-1,
        redshiftDriverPath=NULL,
        redshiftDriverUrl="http://s3.amazonaws.com/redshift-downloads/drivers/RedshiftJDBC41-1.1.9.1009.jar",
        cachePath="./redshift_cache") {
    require(RJDBC)
        
    if (is.null(redshiftDriverPath)) {
        redshiftDriverPath = getwd()
    } else if (!dir.exists(redshiftDriverPath)) {
        dir.create(redshiftDriverPath)
    }
    redshiftDriverFile = regmatches(redshiftDriverUrl, regexpr("[a-zA-Z0-9.-]*jar", redshiftDriverUrl))
    redshiftDriverFullPath = paste(redshiftDriverPath, redshiftDriverFile, sep="/")

    message(paste("Driver file: ", redshiftDriverFullPath, sep=""))

    if (!file.exists(redshiftDriverFullPath)) {
        download.file(redshiftDriverUrl, redshiftDriverFullPath)
    }
    if (!dir.exists(cachePath)) {
        dir.create(cachePath)
    }
    
    rdsFileName = paste(cachePath, "/", queryName, ".rds", sep="")
    if (file.exists(rdsFileName)) {
        modifiedAt = file.mtime(rdsFileName)
        mins = as.integer(difftime(Sys.time(), modifiedAt, units = "mins"))
        if (expire < 0 || mins < expire) {
            return(readRDS(rdsFileName))
        }
    }
    
    message(paste(queryName, "not cached. Fetching from the database."))
    
    driver <- JDBC("com.amazon.redshift.jdbc41.Driver", redshiftDriverFullPath, identifier.quote="`")

    redshiftJdbcUrl <- paste(
        "jdbc:redshift://", redshiftJdbcURL,
        ":", redshiftJdbcPort,
        "/", redshiftDatabase,
        "?user=", redshiftUsername,
        "&password=", redshiftPassword, sep="")
    conn <- dbConnect(driver, redshiftJdbcUrl)

    tryCatch(
        {
            result <- dbSendQuery(conn, query)
            df = fetch(result, n=-1)
        },
        finally={
            message("Disconnecting from the database server.")
            dbDisconnect(conn)
        }
    )
    
    saveRDS(df, rdsFileName)
    return(df)
}
