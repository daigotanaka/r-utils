# Caches the query result in rds file under `getwd()`/treasureData_cache.
# It assumes
#     treasureDataUsername
#     treasureDataPassword
# variables in the env.
# Remove the cache files if the update value is needed.
# Expire:
#     positive integer n: Expire if the existing cache is older than n minutes
#     0: Expire now
#     -1: Always use the existing cache if it exists
cachedTreasureDataQuery =
    function(
        queryName,
        query,
        expire=-1,
        treasureDataDriverFullPath=NULL,
        treasureDataDriverPath=NULL,
        treasureDataDriverUrl="http://central.maven.org/maven2/com/treasuredata/td-jdbc/0.5.9/td-jdbc-0.5.9-jar-with-dependencies.jar",
        cachePath="./treasure_data_cache") {
    require(RJDBC)
        
    if (is.null(treasureDataDriverPath)) {
        treasureDataDriverPath <- getwd()
    } else if (!dir.exists(treasureDataDriverPath)) {
        dir.create(treasureDataDriverPath)
    }
    if (is.null(treasureDataDriverFullPath)) {
        treasureDataDriverFile <- regmatches(treasureDataDriverUrl, regexpr("[a-zA-Z0-9.-]*jar", treasureDataDriverUrl))
        treasureDataDriverFullPath <- paste(treasureDataDriverPath, treasureDataDriverFile, sep="/")
    }
    message(paste("Driver file: ", treasureDataDriverFullPath, sep=""))

    if (!file.exists(treasureDataDriverFullPath)) {
        download.file(treasureDataDriverUrl, treasureDataDriverFullPath)
    }
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
    
    driver <- JDBC("com.treasuredata.jdbc.TreasureDataDriver",
                   treasureDataDriverFullPath,
                   identifier.quote="`")

    conn <- dbConnect(driver, 
                      "jdbc:td://api.treasuredata.com/postgres;type=presto;useSSL=true",
                      treasureDataUsername,
                      treasureDataPassword)

    tryCatch(
        {
            result <- dbSendQuery(conn, query)
            df <- fetch(result, n=-1)
        },
        error = function(e) {
            message(e)
        },
        finally={
            message("Disconnecting from the database server.")
            dbDisconnect(conn)
        }
    )
    
    saveRDS(df, rdsFileName)
    return(df)
}
