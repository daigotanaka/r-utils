# Caches the query result in rds file under `getwd()`/mp_cache.
# It assumes mp_api_key and mp_api_secret  variables in the env.
# Remove the cache files if the update value is needed.
# events: A vector of mixpanel event names to be fetched
# args: Params other than event, type, and unit
#       See https://mixpanel.com/docs/api-documentation/data-export-api#event-default
# tokenExpireInMin: # of minutes before expiring mixpanel token from now
# cacheExpire:
#     positive integer n: Expire if the existing cache is older than n minutes
#     0: Expire now
#     -1: Always use the existing cache if it exists
#
# Example:
# events = c("Some Event Name", "Another Event Name")
# args = c(# Dates are inclusive
#          from_date="2016-03-09",
#          to_date="2016-03-09")
# df = cachedMixpanelEvent("exampleQuery",
#                           events=events,
#                           type="general",
#                           unit="hour",
#                           args=args,
#                           cacheExpire=0)
cachedMixpanelEvent <- function(queryName,
                                events,
                                type="general",
                                unit="day",
                                args=c(),
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
    allArgs["type"] = type
    allArgs["unit"] = unit

    argKeys = sort(names(allArgs))
    argsSig = argsUrl = paste(argKeys[1], "=", allArgs[argKeys[1]], sep="")
    for (i in 2:length(argKeys)) {
        argsSig = paste(argsSig, argKeys[i], "=", allArgs[argKeys[i]], sep="")
        argsUrl = paste(argsUrl, "&", argKeys[i], "=", allArgs[argKeys[i]], sep="")
    }

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



# Caches the query result in rds file under `getwd()`/mp_cache.
# It assumes mp_api_key and mp_api_secret  variables in the env.
# Remove the cache files if the update value is needed.
# events: A vector of mixpanel event names to be fetched (optional)
# args: Params except for events
#       https://mixpanel.com/docs/api-documentation/exporting-raw-data-you-inserted-into-mixpanel#export
# tokenExpireInMin: # of minutes before expiring mixpanel token from now
# cacheExpire:
#     positive integer n: Expire if the existing cache is older than n minutes
#     0: Expire now
#     -1: Always use the existing cache if it exists
# httpTimeoutSec: Number of seconds for httr::GET timeout.
#                 Longer timeout is generally recommended for export method.
cachedMixpanelExport <- function(queryName,
                                 fromDate,
                                 toDate,
                                 args=c(),
                                 events=NULL,
                                 tokenExpireInMin=60*15,
                                 cacheExpire=-1,
                                 cachePath="./mp_cache",
                                 httpTimeoutSec=60) {
    require(RCurl)
    require(rjson)
    require(digest)
    require(httr)
    require(stringr)

    flatten <- function(x) {
        repeat {
            if(!any(vapply(x, is.list, logical(1)))) return(x)
            x <- Reduce(c, x)
        }
    }

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

    allArgs = args

    # Overwrite even if given
    allArgs["expire"] = tokenExpireAt
    allArgs["from_date"] = fromDate
    allArgs["to_date"] = toDate

    if (!is.null(events)) {
        eventArg = paste('["', paste(events, collapse='","'), '"]', sep="")
        allArgs["event"] = eventArg
    }

    argKeys = sort(names(allArgs))
    argsSig = argsUrl = paste(argKeys[1], "=", allArgs[argKeys[1]], sep="")
    for (i in 2:length(argKeys)) {
        argsSig = paste(argsSig, argKeys[i], "=", allArgs[argKeys[i]], sep="")
        argsUrl = paste(argsUrl, "&", argKeys[i], "=", allArgs[argKeys[i]], sep="")
    }

    ## Create the hashed Signature
    sig <- paste("api_key=", mp_api_key, argsSig, mp_api_secret, sep="")
    hashedSig <- digest(sig, algo="md5", serialize = FALSE)

    ## Create the URL with the full authorization string
    url <- paste("http://data.mixpanel.com/api/2.0/export/?",
                 "api_key=", mp_api_key,
                 "&", argsUrl,
                 "&sig=", hashedSig,
                 sep="")

    ## Connect to the Mixpanel API and save data
    jsonl = content(GET(url), as="text", timeout(httpTimeoutSec))  # JSONL format
    jsons = str_split(jsonl, "\n")[[1]]

    allEvents = list()
    for (i in 1:(length(jsons) - 1)) {
        json = jsons[i]
        # Last line may be empty. So tryCatch
        tryCatch({
            parsed <- fromJSON(json)
            event = parsed$event
            props = parsed$properties
            data = as.data.frame(flatten(props), stringsAsFactors=FALSE)
            colnames(data) = names(unlist(props))
            if (is.null(allEvents[[event]])) {
                allEvents[[event]] = data
            } else {
                colNamesOld = names(allEvents[[events]])
                colNamesNew = names(data)
                missingFromNew = which(!(colNamesOld %in% colNamesNew))
                missingFromOld = which(!(colNamesNew %in% colNamesOld))

                if (length(missingFromNew) > 0) {
                    data[, colNamesOld[missingFromNew]] = NA
                }
                if (length(missingFromOld) > 0) {
                    allEvents[[event]][, colNamesNew[missingFromOld]] = NA
                }
                allEvents[[event]] = rbind(allEvents[[event]], data)
            }
        })
    }
    saveRDS(allEvents, mpFileName)
    return(allEvents)
}