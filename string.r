# %format% by G. Grothendieck from http://stackoverflow.com/a/17476306/3423480
library(gsubfn)
`%format%` <- function(fmt, list) {
        pat <- "%\\(([^)]*)\\)"
    fmt2 <- gsub(pat, "%", fmt)
        list2 <- list[strapplyc(fmt, pat)[[1]]]
        do.call("sprintf", c(fmt2, list2))
}

sprintf_ <- function(format, ...) {
    inputs <- list(...)
    if (is.list(inputs[[1]])) {
        return (format %format% inputs[[1]])
    }
    return (sprintf(format, ...))
}

sprintf_.test <- function() {
    # If an arg list is given, function as normal sprintf
    print(sprintf_("%s: %s", "author", "Daigo Tanaka"))
    # If the param is given as a list, work like
    # print("%(key)s: %(value)d" % {"key": "rank", "value": 1})
    # in Python
    print(sprintf_("%(key)s: %(value)d", list(key="rank", value=1)))
}

