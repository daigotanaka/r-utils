# Caches the query result in rds file under `getwd()`/mp_cache.
# It assumes mp_api_key and mp_api_secret  variables in the env.
# Remove the cache files if the update value is needed.
# events: A vector of mixpanel event names to be fetched
# args: Params except for events
#       See https://mixpanel.com/docs/api-documentation/data-export-api#events-default
# tokenExpireInMin: # of minutes before expiring mixpanel token from now
# cacheExpire:
#     positive integer n: Expire if the existing cache is older than n minutes
#     0: Expire now
#     -1: Always use the existing cache if it exists
cachedMixpanelEvents <- function(queryName,
                                 events,
                                 args,
                                 tokenExpireInMin=60*15,
                                 cacheExpire=-1,
                                 cachePath="./mp_cache") {
    require(RCurl)
    require(rjson)
    require(digest)
    require(httr)
 
    if (!dir.exists(cachePath)) {
        dir.create(cachePath)
    }
    
    mpFileName = paste(cachePath, "/", queryName, ".rds", sep="")
    if (file.exists(mpFileName)) {
        modifiedAt = file.mtime(mpFileName)
        mins = as.integer(difftime(Sys.time(), modifiedAt, units = "mins"))
        if (cacheExpire < 0 || mins < cacheExpire) {
            return(readRDS(mpFileName))
        }
    }
    
    message(paste(queryName, "not cached. Fetching from Mixpanel."))
 
    ## Set the arguments  
    ## Set the expiry time for the API link as 15 minutes
    tokenExpireAt <- as.integer(as.numeric(as.POSIXlt(Sys.time()))) + tokenExpireInMin

    eventArg = paste('["', paste(events, collapse='","'), '"]', sep="")
    allArgs = args
    
    # Overwrite even if given
    allArgs["event"] = eventArg
    allArgs["expire"] = tokenExpireAt
    allArgs["format"] = "json"
    
    argKeys = sort(names(allArgs))
    argsSig = argsUrl = paste(argKeys[1], "=", allArgs[argKeys[1]], sep="")
    for (i in 2:length(argKeys)) {
        argsSig = paste(argsSig, argKeys[i], "=", allArgs[argKeys[i]], sep="")
        argsUrl = paste(argsUrl, "&", argKeys[i], "=", allArgs[argKeys[i]], sep="")
    }

    message(argsSig)
    message(argsUrl)

    ## Create the hashed Signature
    sig <- paste("api_key=", mp_api_key, argsSig, mp_api_secret, sep="")
    hashedSig <- digest(sig, algo="md5", serialize = FALSE)
            
    ## Create the URL with the full authorization string
    url <- paste("http://mixpanel.com/api/2.0/events/?",
                 "api_key=", mp_api_key,
                 "&", argsUrl,
                 "&sig=", hashedSig,
                 sep="")

    ## Connect to the Mixpanel API and save data
    json = content(GET(url), as="text")
    df <- fromJSON(json)
    saveRDS(df, mpFileName)
    return(df)
}
