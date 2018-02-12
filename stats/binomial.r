marginOfError =
function(prob,  # sample probability (or response rate)
         n,  # sample size
         N=NULL,
         conf.level=0.95  # Confidence interval
         ) {
    z <- qnorm(p=1.0 - (1.0 - conf.level) * 0.5)
    B <- prob * (1 - prob)
    moe <- z * sqrt(B / n)

    if (!is.null(N)) {
        # tmp <- z ^ 2 * (prob * (1 - prob)) / moe^2
        # n <- tmp / (1 + tmp / N)
        # n + n/N * tmp <- tmp
        # n <- tmp * (1 - n/N)
        fpcf <- sqrt((N - n) / (N - 1))
        moe <-  moe * fpcf
    }
    return(moe)
}

sampleSize =
function(prob,
         moe,  # Margin of error
         N=NULL,  # Population size
         conf.level=0.95
         ) {
    z <- qnorm(p=1.0 - (1.0 - conf.level) * 0.5)
    B <- prob * (1 - prob)
    n <- z ^ 2 * B / moe^2
    if (!is.null(N)) {
        n <- n * N / (n + N - 1)
    }
    return(n)
}


binom.test =
function() {
    prob <- 0.15
    moe <- 0.025
    N <- 1000
    n <- sampleSize(prob=prob, moe=moe, N=N)
    message(n)
    message("Expected margin of error: ", moe)
    moe <- marginOfError(prob=prob, n=n, N=N)
    message(moe)
}
