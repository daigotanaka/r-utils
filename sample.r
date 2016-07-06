getTTestSampleSize = function(x, minDetectDiffPercent, sigLevel=0.05, power=0.8, type="two.sample", alternative="two.sided") {
    # Get the required sample size for t-test
    # minDetectDiffPercent: Percent increase or decrease from the base mean you want to detect
    # type = c("two.sample", "one.sample", "paired")
    # alternative = c("two.sided", "less", "greater")
    # Return: pwr.t.test object with n (sample size) filled
    # Note: pooled standard deviation is approximated by the SD of x
    require(pwr)
    powerAnalysis =
        pwr.t.test(
            d=mean(x) * (minDetectDiffPercent / 100) / sd(x),
            sig.level=sigLevel,
            power=power,
            type=type,
            alternative=alternative)
    return(powerAnalysis)
}
