# Summarize, display, and visualize results, and make further computations, based on the results.
# x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-
# MOST IMPORTANT: First, run the R script nsduhStart.R
# x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-
# Read all custom or modified functions from separate script
source(paste0(nsduhPath, "nsduhFuns.R"), echo = FALSE)
#
# -------------------------------------------------
# BEWARE: In order for this R script (nsduhResults.R) to be used in isolation, two things are important:
# 1. The results of the script nsduhMain.R must have been saved as rds files and these rds files must be in the same directory where all the R scripts are (see nsduhStart.R, script line 20).
# 1.1 rds files are saved in nsduhMain.R:
# script lines 121-128, 181-182, and 262-266.
# -------------------------------------------------
#
# IMPORTANT:
# Select here, one specific part of the overall results. Two selections are required:
# Select a number between 1 and 15 (risk ratio, risk difference).
sel1To15 <- 8
#
# ---------------------------------------
# 1. Risk ratio
# 2. risk difference
# ---------------------------------------
#
# ---------
# DuplStart
runRiskDf <- data.frame(
    p0 = rep(c(.12, .11, .1, .11, .12), times=3),
    p1 = rep(c(.24, .22, .2, .2, .2), times=3),
    a=c(rep(.05, times=5), rep(.01, times=5), rep(.005, times=5))
)

exactN <- c()
for(i in 1:nrow(runRiskDf)) {
    exactN <- c(exactN,
                pwrss::pwrss.z.logreg(p0 = runRiskDf$p0[i],
                                      p1 = runRiskDf$p1[i],
                                      r2.other.x = 0,
                                      power = 1-runRiskDf$a[i],
                                      alpha = runRiskDf$a[i], 
                                      dist="binomial",
                                      alternative = "greater",
                                      verbose=FALSE)$n)
}
runRiskDf$exactN <- exactN
runRiskDf$requiredN <- ceiling(runRiskDf$exactN)
runRiskDf$power <- 1-runRiskDf$a
runRiskDf$czVal <- qnorm(p=1-runRiskDf$a)
runRiskDf$rrH0 <- 1.2
runRiskDf$rdH0 <- .02
runRiskDf$orH0 <- 1.2
# DuplEnd
# ---------

# Read saved results
# collectRiskResLs <- readRDS(file=paste0(nsduhPath, "collectRiskResLs.rds"))
# names(collectRiskResLs)
# resDfVec <- names(collectRiskResLs)[seq(1,length(collectRiskResLs),by=4)]

# Use same code for 2-sided tests, to produce the multiplots
collectRiskRes2SidedLs <- readRDS(file=paste0(nsduhPath, "collectRiskRes2SidedLs.rds"))
resDfVec <- names(collectRiskRes2SidedLs)
collectRiskResLs <- collectRiskRes2SidedLs

# obtyI: observed type I error; sH0: specified null hypothesis
obtyIrr <- obtyIrrsH0 <- obtyIrd <- obtyIrdsH0 <- c()
lowerCI <- rep(c("l95", "l99", "l995"), each=5)
# obtyI: observed type I error, CI, use compatibility interval, instead of empirical p value.
obtyIrrCI <- obtyIrrsH0CI <- obtyIrdCI <- obtyIrdsH0CI <- c()
for(p in 1:nrow(runRiskDf)) {
    
    # Total number of conducted tests
    tnoct <- nrow(collectRiskResLs[[resDfVec[p]]])
    
    # Risk ratio
    obtyIrr <- c(obtyIrr, length(which(collectRiskResLs[[resDfVec[p]]]$rrp >= runRiskDf$a[p]))/tnoct)
    obtyIrrsH0 <- c(obtyIrrsH0, length(which(collectRiskResLs[[resDfVec[p]]]$rrpsH0 >= runRiskDf$a[p]))/tnoct)
    
    # Risk difference
    obtyIrd <- c(obtyIrd, length(which(collectRiskResLs[[resDfVec[p]]]$rdp >= runRiskDf$a[p]))/tnoct)
    obtyIrdsH0 <- c(obtyIrdsH0, length(which(collectRiskResLs[[resDfVec[p]]]$rdpsH0 >= runRiskDf$a[p]))/tnoct)
    
    # ------------------------------
    # Re-check by using the observed 'success' rate: Use compatibility interval, ask how often its lower bound is greater than 1.0 and 1.2, respectively.
    # The observed error and success rate must add up to 1.
    # ------------------------------
    
    # Risk ratio
    obtyIrrCI <- c(obtyIrrCI, length(which(collectRiskResLs[[resDfVec[p]]][,paste0("rr", lowerCI[p])] > 1.0))/tnoct)
    obtyIrrsH0CI <- c(obtyIrrsH0CI, length(which(collectRiskResLs[[resDfVec[p]]][,paste0("rr", lowerCI[p])] > 1.2))/tnoct)
    
    # Risk difference
    obtyIrdCI <- c(obtyIrdCI, length(which(collectRiskResLs[[resDfVec[p]]][,paste0("rd", lowerCI[p])] > 0))/tnoct)
    obtyIrdsH0CI <- c(obtyIrdsH0CI, length(which(collectRiskResLs[[resDfVec[p]]][,paste0("rd", lowerCI[p])] > .02))/tnoct)
}

# When empirical p-value >= designated alpha significance level, this means that, given the a priori expected effect size, the alpha level, and the power, resulting in the computed minimum sample size, the designated null effect was part of the compatibility interval in x% of all conducted tests.

# Append to runRiskDf:
runRiskDf <- cbind(runRiskDf,
                   data.frame(
                       obtyIrr, obtyIrrsH0,
                       obtyIrd, obtyIrdsH0,
                       obtyIrrCI, obtyIrrsH0CI,
                       obtyIrdCI, obtyIrdsH0CI)
)
# Display complete runRiskDf in R console:
runRiskDf
runRiskDf[,c("p0", "p1", "a", "requiredN", "power", "obtyIrr", "obtyIrrsH0", "obtyIrd", "obtyIrdsH0")]
# Run the check, whether the observed error rate, using the empirical p-value, and whether the 'success' rate of the conventional and the specified null hypothesis add up to 1.
# Check conventional H0
all((runRiskDf$obtyIrr + runRiskDf$obtyIrrCI)==1)
all((runRiskDf$obtyIrd + runRiskDf$obtyIrdCI)==1)
# Same with specific H0 (sH0)
all((runRiskDf$obtyIrrsH0 + runRiskDf$obtyIrrsH0CI)==1)
all((runRiskDf$obtyIrdsH0 + runRiskDf$obtyIrdsH0CI)==1)