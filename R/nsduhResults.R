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
# 1.2 One more rds file is saved in this R script, see script lines 694-695.
# 2. Some of the code from another R script has to be duplicated. This code is shown below by nesting the code chunks between # DuplStart and # DuplEnd, for example see script lines 35 and 59.
# -------------------------------------------------
#
# IMPORTANT:
# Select here, one specific part of the overall results. Two selections are required:
# Select a number between 1 and 15 (risk ratio, risk difference, and odds ratio).
sel1To15 <- 8
# Select a number between 1 and 9 (hazard ratio).
sel1To9 <- 5
#
# Three more selections are possible in this script, see script lines:
# 160, 167, 342 (see  S E L E C T  in the comments below).
#
# ---------------------------------------
# 1. Risk ratio
# 2. risk difference
# 3. odds ratio
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
collectRiskResLs <- readRDS(file=paste0(nsduhPath, "collectRiskResLs.rds"))
names(collectRiskResLs)

resDfVec <- names(collectRiskResLs)[seq(1,length(collectRiskResLs),by=4)]
# actyI: actual type I error; sH0: specified null hypothesis
actyIrr <- actyIrrsH0 <- actyIrd <- actyIrdsH0 <- actyIor <- actyIorsH0 <- c()
lowerCI <- rep(c("l95", "l99", "l995"), each=5)
# actyI: actual type I error, CI, use compatibility interval, instead of empirical p value.
actyIrrCI <- actyIrrsH0CI <- actyIrdCI <- actyIrdsH0CI <- actyIorCI <- actyIorsH0CI <- c()
for(p in 1:nrow(runRiskDf)) {
    
    # Total number of conducted tests
    tnoct <- nrow(collectRiskResLs[[resDfVec[p]]])
    
    # Risk ratio
    actyIrr <- c(actyIrr, length(which(collectRiskResLs[[resDfVec[p]]]$rrp >= runRiskDf$a[p]))/tnoct)
    actyIrrsH0 <- c(actyIrrsH0, length(which(collectRiskResLs[[resDfVec[p]]]$rrpsH0 >= runRiskDf$a[p]))/tnoct)
    
    # Risk difference
    actyIrd <- c(actyIrd, length(which(collectRiskResLs[[resDfVec[p]]]$rdp >= runRiskDf$a[p]))/tnoct)
    actyIrdsH0 <- c(actyIrdsH0, length(which(collectRiskResLs[[resDfVec[p]]]$rdpsH0 >= runRiskDf$a[p]))/tnoct)
    
    # Odds ratio
    actyIor <- c(actyIor, length(which(collectRiskResLs[[resDfVec[p]]]$orp >= runRiskDf$a[p]))/tnoct)
    actyIorsH0 <- c(actyIorsH0, length(which(collectRiskResLs[[resDfVec[p]]]$orpsH0 >= runRiskDf$a[p]))/tnoct)
    
    # ------------------------------
    # Re-check by using the observed 'success' rate: Use compatibility interval, ask how often its lower bound is greater than 1.0 and 1.2, respectively.
    # The observed error and success rate must add up to 1.
    # ------------------------------
    
    # Risk ratio
    actyIrrCI <- c(actyIrrCI, length(which(collectRiskResLs[[resDfVec[p]]][,paste0("rr", lowerCI[p])] > 1.0))/tnoct)
    actyIrrsH0CI <- c(actyIrrsH0CI, length(which(collectRiskResLs[[resDfVec[p]]][,paste0("rr", lowerCI[p])] > 1.2))/tnoct)
    
    # Risk difference
    actyIrdCI <- c(actyIrdCI, length(which(collectRiskResLs[[resDfVec[p]]][,paste0("rd", lowerCI[p])] > 0))/tnoct)
    actyIrdsH0CI <- c(actyIrdsH0CI, length(which(collectRiskResLs[[resDfVec[p]]][,paste0("rd", lowerCI[p])] > .02))/tnoct)
    
    # Odds ratio
    actyIorCI <- c(actyIorCI, length(which(collectRiskResLs[[resDfVec[p]]][,paste0("or", lowerCI[p])] > 1.0))/tnoct)
    actyIorsH0CI <- c(actyIorsH0CI, length(which(collectRiskResLs[[resDfVec[p]]][,paste0("or", lowerCI[p])] > 1.2))/tnoct)
}

# When empirical p-value >= designated alpha significance level, this means that, given the a priori expected effect size, the alpha level, and the power, resulting in the computed minimum sample size, the designated null effect was part of the compatibility interval in x% of all conducted tests.

# Append to runRiskDf:
runRiskDf <- cbind(runRiskDf,
                   data.frame(
                       actyIrr, actyIrrsH0,
                       actyIrd, actyIrdsH0,
                       actyIor, actyIorsH0,
                       actyIrrCI, actyIrrsH0CI,
                       actyIrdCI, actyIrdsH0CI,
                       actyIorCI, actyIorsH0CI)
                   )
# Display complete runRiskDf in R console:
runRiskDf
runRiskDf[,c("p0", "p1", "a", "requiredN", "power", "actyIrr", "actyIrd", "actyIor")]
# Run the check, whether the observed error rate, using the empirical p-value, and whether the 'success' rate of the conventional and the specified null hypothesis add up to 1.
# Check conventional H0
all((runRiskDf$actyIrr + runRiskDf$actyIrrCI)==1)
all((runRiskDf$actyIrd + runRiskDf$actyIrdCI)==1)
all((runRiskDf$actyIor + runRiskDf$actyIorCI)==1)
# Same with specific H0 (sH0)
all((runRiskDf$actyIrrsH0 + runRiskDf$actyIrrsH0CI)==1)
all((runRiskDf$actyIrdsH0 + runRiskDf$actyIrdsH0CI)==1)
all((runRiskDf$actyIorsH0 + runRiskDf$actyIorsH0CI)==1)

# Want to visualize?
# ------------------

# What do you want to visualize and how?

# Boxplots of p values from various stat. tests.
source(paste0(nsduhPath, "nsduhPValBoxplots.R"), echo = FALSE)
# Display boxplots in R console
pvalBox

# Barplot of one specific p value, select the test first, from which you want to visualze the p value distribution with a barplot.
# Assign one of these 3 p values to specificP: "rrp", "rdp", "orp".
# rr = risk ratio, rd = risk difference, or = odds ratio.
specificP <- "rrp"
source(paste0(nsduhPath, "nsduhPValBarplot.R"), echo = FALSE)
# Display barplot in R console
pvalBar

# Boxplots of p values from various stat. tests.
source(paste0(nsduhPath, "nsduhSValBoxplots.R"), echo = FALSE)
# Display boxplots in R console
svalBox

# Barplot of one specific S value, select the test first, from which you want to visualze the S value distribution with a barplot.
# S E L E C T  one of these 3 p values to specificP: "rrp", "rdp", "orp".
# rr = risk ratio, rd = risk difference, or = odds ratio.
# The p value will be converted to the S value.
specificP <- "rrp"
source(paste0(nsduhPath, "nsduhSValBarplot.R"), echo = FALSE)
# Display barplot in R console
svalBar

# Forest plot of one of these three measures: rr, rd, or
# S E L E C T  one of these three measures: "rr", "rd", "or"
selRiskMeas <- "rr"
source(paste0(nsduhPath, "nsduhRRRDORForestPlot.R"), echo = FALSE)
# Display forest plot in R console
plotRisk
# Adapt x-axis. BEWARE: RR and OR requires a log-transformed x-axis, RD does not.
# Use range information, when adapting the x-axis:
readRDS(file=paste0(nsduhPath, "rangeEstimatesLs.rds"))$rr_rd_or[sel1To15,]
# If plotRisk shows RR or OR, adapt x-axis like this:
(plotRisk <- plotRisk +
    scale_x_continuous(trans='log2',
                       # Range information, to guide the breaks:
                       # sel1To15 = 8: risk ratio lower interval minimum = .67, upper interval maximum = Inf, point risk ratio minimum = .93, maximum = 2.49.
                       breaks = c(.7, 1, 1.3, 1.5, 2, 2.5)))
# If plotRisk shows RD, adapt x like this:
plotRisk <- plotRisk +
    # Range information, to guide the breaks:
    # sel1To15 = 8: risk difference lower interval minimum = -.06, upper interval maximum = 1, point risk ratio minimum = -.01, maximum = .14.
    scale_x_continuous(breaks = c(-.05, 0, .05, .15, .3, .5, 1))
# -------------------------------------------------

# ---------------------------------------
# Collapsibility problem
# ---------------------------------------

collectCollapsedLs <- readRDS(file=paste0(nsduhPath, "collectCollapsedLs.rds"))
collapseVec <- names(collectCollapsedLs)

# Risk ratio, no collapsibility problem, all results equal 1 (good).
collectCollapsedLs[[collapseVec[sel1To15]]]$riskCollapsed/collectCollapsedLs[[collapseVec[sel1To15]]]$absRiskTotal

# Odds ratio, collapsibility problem, all results are greater than 1 (not good).
collectCollapsedLs[[collapseVec[sel1To15]]]$oddsCollapsed/collectCollapsedLs[[collapseVec[sel1To15]]]$oddsTotal
# -------------------------------------------------
# -------------------------------------------------
#
# ---------------------------------------
# 4. Hazard ratio
# ---------------------------------------
#
# ---------
# DuplStart
runCoxDf <- data.frame(hr=rep(c(2, 1.8, 1.6), each=3),
                       a=rep(c(.05, .01, .005), times=3))

smpSze <- c()
for(s in 1:nrow(runCoxDf)) {
    smpSze <- c(smpSze,
                get_n_onesided(
                    hr=runCoxDf$hr[s],
                    eventprob = .1645,
                    rsquare = 0,
                    stddev = .5,
                    sig_level = runCoxDf$a[s],
                    power = 1-runCoxDf$a[s]
                ))
}
runCoxDf$power <- 1-runCoxDf$a
runCoxDf$smpSize <- smpSze
runCoxDf$requiredN <- ceiling(runCoxDf$smpSize)
runCoxDf$czVal <- qnorm(p=runCoxDf$a, lower.tail = FALSE)
runCoxDf$hrH0 <- 1.2
# DuplEnd
# ---------

collectCoxResLs <- readRDS(file=paste0(nsduhPath, "collectCoxResLs.rds"))

resDfVec <- names(collectCoxResLs)[seq(1,length(collectCoxResLs),by=2)]
lowerCI <- rep(c("l95", "l99", "l995"), times=3)
# actyI: actual type I error; sH0: specified null hypothesis
actyIhr <- actyIhrsH0 <- actyIhrCI <- actyIhrsH0CI <- c()
for(p in 1:nrow(runCoxDf)) {
    
    # Total number of conducted tests
    tnoct <- nrow(collectCoxResLs[[resDfVec[p]]])
    
    actyIhr <- c(actyIhr, length(which(collectCoxResLs[[resDfVec[p]]]$hrp >= runCoxDf$a[p]))/tnoct)
    actyIhrsH0 <- c(actyIhrsH0, length(which(collectCoxResLs[[resDfVec[p]]]$hrpsH0 >= runCoxDf$a[p]))/tnoct)
    
    actyIhrCI <- c(actyIhrCI, length(which(collectCoxResLs[[resDfVec[p]]][,paste0("hr", lowerCI[p])] > 1.0))/tnoct)
    actyIhrsH0CI <- c(actyIhrsH0CI, length(which(collectCoxResLs[[resDfVec[p]]][,paste0("hr", lowerCI[p])] > 1.2))/tnoct)
    
}
# Append to runCoxDf:
runCoxDf <- cbind(runCoxDf,
                  data.frame(
                      actyIhr, actyIhrsH0,
                      actyIhrCI, actyIhrsH0CI)
                  )
# Display in R console
runCoxDf

# Run the check, whether the observed error rate, using the empirical p-value, and whether the 'success' rate of the conventional and the specified null hypothesis add up to 1.
# Check conventional H0
all((runCoxDf$actyIhr + runCoxDf$actyIhrCI)==1)
# Same with specific H0 (sH0)
all((runCoxDf$actyIhrsH0 + runCoxDf$actyIhrsH0CI)==1)

# Want to visualize?
# ------------------
source(paste0(nsduhPath, "nsduhHRForestPlot.R"), echo = FALSE)
# Display forest plot in R console
plotHazard

# Adapt x-axis. BEWARE: RR and OR requires a log-transformed x-axis, RD does not.
# Use range information, when adapting the x-axis:
readRDS(file=paste0(nsduhPath, "rangeEstimatesLs.rds"))$hr[sel1To9,]
(plotHazard <- plotHazard +
        scale_x_continuous(trans='log2',
                           # Range information, to guide the breaks:
                           # sel1To9 = 5: hazard ratio lower interval minimum = .85, upper interval maximum = Inf, point hazard ratio minimum = 1.15, maximum = 2.37.
                           breaks = c(.8, 1, 1.3, 1.6, 2, 2.4)))

# Proportional hazards (PH) test assumption
summary(collectCoxResLs[[resDfVec[sel1To9]]]$phTestp)
# Number of stat. non-significant PH test results
(failedPH <- length(which(collectCoxResLs[[resDfVec[sel1To9]]]$phTestp <= .05)))
# Number of stat. significant PH test result
(successPH <- length(which(collectCoxResLs[[resDfVec[sel1To9]]]$phTestp > .05)))
# -------------------------------------------------
# -------------------------------------------------
#
# ----------------------------------------
# 5. Difference between two independent proportions
# 5.1 Unequal and equal sample size in both groups.
# ----------------------------------------
#
# ---------
# DuplStart
(n1n2Ratio <- (100-53.9)/(100-(100-53.9))) # male to female ratio

runPropTstDf0 <- data.frame(
    p0 = rep(c(.12, .11, .1, .11, .12), times=3),
    p1 = rep(c(.24, .22, .2, .2, .2), times=3),
    a=c(rep(.05, times=5), rep(.01, times=5), rep(.005, times=5))
)
runPropTstDf0$power <- 1-runPropTstDf0$a

exactN.855 <- exactN1 <- c()
for(i in 1:nrow(runPropTstDf0)) {
    exactN.855 <- c(exactN.855,
                    pwrss::pwrss.z.2props(p1 = runPropTstDf0$p0[i],
                                          p2 = runPropTstDf0$p1[i],
                                          alpha = runPropTstDf0$a[i],
                                          power = 1-runPropTstDf0$a[i], 
                                          kappa = n1n2Ratio,
                                          alternative = "greater",
                                          verbose = FALSE)$n)
    
    exactN1 <- c(exactN1,
                 pwrss::pwrss.z.2props(p1 = runPropTstDf0$p0[i],
                                       p2 = runPropTstDf0$p1[i],
                                       alpha = runPropTstDf0$a[i],
                                       power = 1-runPropTstDf0$a[i], 
                                       kappa = 1,
                                       alternative = "greater",
                                       verbose = FALSE)$n)
}

mat.855 <- data.frame(ceiling(matrix(exactN.855, ncol=2, byrow = TRUE)))
colnames(mat.855) <- c("n1.855", "n2.855")
mat1 <- data.frame(ceiling(matrix(exactN1, ncol=2, byrow = TRUE)))
colnames(mat1) <- c("n1", "n2")

runPropTstDf <- cbind(runPropTstDf0, mat.855, mat1)
# DuplEnd
# ---------

propTests <- c(
    paste0(nsduhPath, "collectPropTestResLsNeq.rds"),
    paste0(nsduhPath, "collectPropTestResLsEq.rds")
)

nameRes <- c("neqSampleSize", "equalSampleSize")
propTestResLs <- list()

for(pr in 1:length(propTests)) {
    collectPrpResLs <- readRDS(file=propTests[pr])
    prpTestDfVec <- names(collectPrpResLs)
    prpTstErrorRate <- c()
    for(r in 1:nrow(runPropTstDf)) {
        
        prpTstErrorRate <- c(prpTstErrorRate,
                             length(which(collectPrpResLs[[prpTestDfVec[r]]]$conf.low<0))/nrow(collectPrpResLs[[prpTestDfVec[r]]]))
        
    }
    runPropTstDf$prpTestActyI <- prpTstErrorRate
    runPropTstDf$prpTestFreqAlpha <- runPropTstDf$prpTestActyI < runPropTstDf$a
    
    propTestResLs[[nameRes[pr]]] <- runPropTstDf
}

# propTests[1] = not equal sample size
collectPrpResLs1 <- readRDS(file=propTests[1])
# propTests[2] = equal sample size
collectPrpResLs2 <- readRDS(file=propTests[2])

# S E L E C T  either collectPrpResLs1 or collectPrpResLs2, by assigning it to variable name 'collectPrpResLs'.
collectPrpResLs <- collectPrpResLs1
#
source(paste0(nsduhPath, "nsduhPropTestForestPlot.R"), echo = FALSE)
# Display forest plot in R console
plotProp
# -------------------------------------------------
# -------------------------------------------------

# ----------------------------------------
# 6. Likelihood ratio test (risk difference, risk ratio, odds ratio)
# ----------------------------------------

collectRiskResLs <- readRDS(file=paste0(nsduhPath, "collectRiskResLs.rds"))
names(collectRiskResLs)

# Relevant results from collectRiskResLs
resDfVec <- names(collectRiskResLs)[seq(1,length(collectRiskResLs),by=4)]

llkDf <- collectRiskResLs[[resDfVec[sel1To15]]][,c("rrChisq", "rdChisq", "orChisq")]
llkDf$allEqual <- apply(llkDf, MARGIN=1, function(x) {
    # Round to 10 decimals, else (>= 11 decimals) equality is not true.
    all(round(x[1], 10) == round(x[2:3], 10))
})
all(llkDf$allEqual)

head(llkDf, n=10)
# -------------------------------------------------
# -------------------------------------------------

# ----------------------------------------
# 7. Multilevel analyses (risk difference, risk ratio, odds ratio)
# ----------------------------------------

# For alpha significance level of the single studies, see runRiskDf
runRiskDf[,c("a", "requiredN", "power")]

collectRisklme4Ls <- readRDS(file=paste0(nsduhPath, "collectRisklme4Ls.rds"))
lme4DfVec <- names(collectRisklme4Ls)
resLme4 <- collectRisklme4Ls[[lme4DfVec[sel1To15]]]

# Display all 15 results in R console
for(m in 1:15) {
    print(m)
    print(collectRisklme4Ls[[lme4DfVec[m]]])
    cat("------------------------\n\n")
}
# -------------------------------------------------
# -------------------------------------------------

# ----------------------------------------
# 8. Meta-Analysis of conventional two-sided logistic regressions.
# ----------------------------------------

# For alpha significance level of the single studies, see runRiskDf
runRiskDf[sel1To15, c("a", "requiredN", "power")]

# Risk Ratio (rr)
# ---------------
rrMeta <- readRDS(paste0(nsduhPath, "collectRiskResLs.rds"))[[paste0("Result", sel1To15,"rrDf")]]
# Display input data in R console
head(rrMeta)

# According to the selected value for sel1To15, the corresponding option is used, from these three options: rrl95, rrl99, rrl995 (same for upper, e.g., rru99).
if(any((1:5) %in% sel1To15)) {
    meta.rr <- meta::metagen(TE=log(rr), lower = log(rrl95), upper = log(rru95), sm="RR", prediction = TRUE, data = rrMeta)
} else if(any((6:10) %in% sel1To15)) {
    meta.rr <- meta::metagen(TE=log(rr), lower = log(rrl99), upper = log(rru99), sm="RR", prediction = TRUE, data = rrMeta)
} else if(any((11:15) %in% sel1To15)) {
    meta.rr <- meta::metagen(TE=log(rr), lower = log(rrl995), upper = log(rru995), sm="RR", prediction = TRUE, data = rrMeta)
}
# summary(meta.rr)

# Extract five results: The meta effect estimate, the lower bound of the compatibility interval (from the random effects model and the prediction interval), the upper bound of the compatibility interval (same specification as with the lower bound).
metaResRR <- c(meta.rr$TE.random,
               meta.rr$lower.random, meta.rr$upper.random,
               meta.rr$lower.predict, meta.rr$upper.predict)
# Display results as risk ratios in the R console:
exp(metaResRR)

# -----------------------------------------

# Risk Difference (rd)
# --------------------
rdMeta <- readRDS(paste0(nsduhPath, "collectRiskResLs.rds"))[[paste0("Result", sel1To15,"rdDf")]]
# Display input data in R console
head(rdMeta)

# According to the selected value for sel1To15, the corresponding option is used, from these three options: rdl95, rdl99, rdl995 (same for upper, e.g., rdu99).
if(any((1:5) %in% sel1To15)) {
    meta.rd <- meta::metagen(TE=rd, lower = rdl95, upper = rdu95, sm="RD", prediction = TRUE, data = rdMeta)
} else if(any((6:10) %in% sel1To15)) {
    meta.rd <- meta::metagen(TE=rd, lower = rdl99, upper = rdu99, sm="RD", prediction = TRUE, data = rdMeta)
} else if(any((11:15) %in% sel1To15)) {
    meta.rd <- meta::metagen(TE=rd, lower = rdl995, upper = rdu995, sm="RD", prediction = TRUE, data = rdMeta)
}
# summary(meta.rd)

# Extract five results: The meta effect estimate, the lower bound of the compatibility interval (from the random effects model and the prediction interval), the upper bound of the compatibility interval (same specification as with the lower bound).
metaResRD <- c(meta.rd$TE.random,
               meta.rd$lower.random, meta.rd$upper.random,
               meta.rd$lower.predict, meta.rd$upper.predict)
# Display results (risk differences) in the R console:
metaResRD

# -----------------------------------------

# Odds Ratio (or)
# ---------------
orMeta <- readRDS(paste0(nsduhPath, "collectRiskResLs.rds"))[[paste0("Result", sel1To15,"orDf")]]
# Display input data in R console
head(orMeta)

# According to the selected value for sel1To15, the corresponding option is used, from these three options: orl95, orl99, orl995 (same for upper, e.g., oru99).
if(any((1:5) %in% sel1To15)) {
    meta.or <- meta::metagen(TE=log(or), lower = log(orl95), upper = log(oru95), sm="RR", prediction = TRUE, data = orMeta)
} else if(any((6:10) %in% sel1To15)) {
    meta.or <- meta::metagen(TE=log(or), lower = log(orl99), upper = log(oru99), sm="RR", prediction = TRUE, data = orMeta)
} else if(any((11:15) %in% sel1To15)) {
    meta.or <- meta::metagen(TE=log(or), lower = log(orl995), upper = log(oru995), sm="RR", prediction = TRUE, data = orMeta)
}
# summary(meta.or)

# Extract five results: The meta effect estimate, the lower bound of the compatibility interval (from the random effects model and the prediction interval), the upper bound of the compatibility interval (same specification as with the lower bound).
metaResOR <- c(meta.or$TE.random,
               meta.or$lower.random, meta.or$upper.random,
               meta.or$lower.predict, meta.or$upper.predict)
# Display results as odds ratios in the R console:
exp(metaResOR)

# -----------------------------------------

# Hazard Ratio (hr)
# -----------------
# 
runCoxDf[sel1To9, c("a", "requiredN", "power")]

hrMeta <- readRDS(file=paste0(nsduhPath, "collectCoxResLs.rds"))[[paste0("Result", sel1To9,"hrDf")]]
# Display input data in R console
head(hrMeta)

# According to the selected value for sel1To9, the corresponding option is used, from these three options: hrl95, hrl99, hrl995 (same for upper, e.g., hru99).
if(any(c(1,4,7) %in% sel1To9)) {
    meta.hr <- meta::metagen(TE=log(hr), lower = log(hrl95), upper = log(hru95), sm="HR", prediction = TRUE, data = hrMeta)
} else if(any(c(2,5,8) %in% sel1To9)) {
    meta.hr <- meta::metagen(TE=log(hr), lower = log(hrl99), upper = log(hru99), sm="HR", prediction = TRUE, data = hrMeta)
} else if(any(c(3,6,9) %in% sel1To9)) {
    meta.hr <- meta::metagen(TE=log(hr), lower = log(hrl995), upper = log(hru995), sm="HR", prediction = TRUE, data = hrMeta)
}
# summary(meta.hr)

# Extract five results: The meta effect estimate, the lower bound of the compatibility interval (from the random effects model and the prediction interval), the upper bound of the compatibility interval (same specification as with the lower bound).
metaResHR <- c(meta.hr$TE.random,
               meta.hr$lower.random, meta.hr$upper.random,
               meta.hr$lower.predict, meta.hr$upper.predict)
# Display results as hazard ratios in the R console:
exp(metaResHR)
# -------------------------------------------------

# Run the following lines, only after you have run the code lines above (418-519).
#
fpDf0 <- data.frame(
    model = c("rrMeta", "rrLme4", "rdMeta", "rdLme4", "orMeta", "orLme4"),
    est=c(exp(metaResRR)[1], as.numeric(resLme4["RR",1]),
          metaResRD[1], as.numeric(resLme4["RD",1]),
          exp(metaResOR)[1], as.numeric(resLme4["OR",1])),
    lci=c(exp(metaResRR)[4], as.numeric(resLme4["RR",2]),
          metaResRD[4], as.numeric(resLme4["RD",2]),
          exp(metaResOR)[4], as.numeric(resLme4["OR",2])),
    uci=c(exp(metaResRR)[5], as.numeric(resLme4["RR",3]),
          metaResRD[5], as.numeric(resLme4["RD",3]),
          exp(metaResOR)[5], as.numeric(resLme4["OR",3]))
)

fpDfRD <- fpDf0[grepl("rd", fpDf0$model),]
fpDfRD$model <- forcats::as_factor(fpDfRD$model)
# Want to visualize?
# ------------------
source(paste0(nsduhPath, "nsduhRDMetaForestPlot.R"), echo = FALSE)
# Display in R console
rdForest

fpDfRROR <- fpDf0[!grepl("rd", fpDf0$model),]
fpDfRRORHR <- rbind(fpDfRROR,
                    data.frame(
                        model="hrMeta",
                        est=exp(metaResHR[1]),
                        lci=exp(metaResHR[4]),
                        uci=exp(metaResHR[5])
                    ))
fpDfRRORHR$model <- forcats::as_factor(fpDfRRORHR$model)
# Want to visualize?
# ------------------
source(paste0(nsduhPath, "nsduhRRORHRMetaForestPlot.R"), echo = FALSE)
# Display in R console
rrorhrForest
# Adjust x-axis (show tick marks 1, 1.2, 1.5, 1.7, and 2)
(rrorhrForest <- rrorhrForest +
        scale_x_continuous(trans = "log2",
            breaks = c(1, 1.2, 1.5, 1.7, 2)))
# -------------------------------------------------
#
# -----------------------------------------------
# 9. Bayes logistic regression (brms package)
# -------------------------
# See Hamra et al. (2013; https://doi.org/10.1093/ije/dyt043)
# ----------------------------------------

# Read the collected risk results (risk ratio, risk difference, odds ratio)
collectRiskResLs <- readRDS(file=paste0(nsduhPath, "collectRiskResLs.rds"))
# Extract the collected risk ratio results.
rrVec <- grep("rr", names(collectRiskResLs))

# Overview of the range of risk ratio (rr) results across all analyses.
summary(collectRiskResLs[[rrVec[sel1To15]]]$rr)
# Select risk ratio results between 1.64 and 1.66 (median and mean rr for sel1To15 = 8)
idx1.64.66 <- collectRiskResLs[[rrVec[sel1To15]]]$rr >= 1.64 & collectRiskResLs[[rrVec[sel1To15]]]$rr <= 1.66
# Display in R console
collectRiskResLs[[rrVec[sel1To15]]][idx1.64.66,]

# Run code that is necessary to execute Bayes Logistic regression.
source(paste0(nsduhPath, "nsduhBayesSetup.R"), echo = FALSE)

i <- 194
# Conventional frequentist logistic regression
rrMod_i <- glm(ltmde ~ sex, family=binomial(link="log"), data=riskData1[pick==i,])
summary(rrMod_i)

fit <- brm(ltmde ~ sex,
           data = riskData1[pick==i,],
           family = bernoulli(link = "log"),
           refresh = 0)
summary(fit)

bayesianLogreg <- plot(fit)

# ggsave(filename="BayesianLogreg.png", plot = bayesianLogreg[[1]], path = ggsavePath, device = "png", width=8, height=7, units="in", dpi=300)
#
# modelposterior is needed to compute autocorrelation results and to execute the Gelman-Rubin Divergence Diagnostic.
modelposterior <- brms::as.mcmc(fit)
#
# Autocorrelation
coda::autocorr(x=modelposterior)
# Autocorrelation diagnostics
coda::autocorr.diag(mcmc.obj = modelposterior)
# Autocorrelation plots
coda::autocorr.plot(x = modelposterior)
# 
# Gelman-Rubin Divergence Diagnostic.
# Approximate convergence is diagnosed when the upper limit is close to 1.
coda::gelman.diag(modelposterior[,1:4])
# Gelman-Rubin Divergence Diagnostic, plots.
# Diplay the evolution of Gelman and Rubin’s shrink factor as the number of iterations increases.
coda::gelman.plot(modelposterior[,1:4])
# -----------------------------------------------
#
# ----------------------------------------
# 10. Outcome misclassification bias adjustment
# ----------------------------------------

collectRiskResLs <- readRDS(file=paste0(nsduhPath, "collectRiskResLs.rds"))

# Argument bias_params (for outcome misclassification):
# 1. Sensitivity of outcome classification among those with the exposure (with exposure = females).
# 2. Sensitivity of outcome classification among those without the exposure (without exposure = males).
# 3. Specificity of outcome classification among those with the exposure.
# 4. Specificity of outcome classification among those without the exposure.

resDfVec <- names(collectRiskResLs)[seq(1,length(collectRiskResLs),by=4)]
rrDfVec <- names(collectRiskResLs)[seq(3,length(collectRiskResLs),by=4)]

# Outcome misclassification bias adjustment, selected setting.
se1Set <- .7; se0Set <- .45; sp1Set <- .95; sp0Set <- sp1Set

# Collect results in these four lists:
adjLs <- posnegLRLs <- lkhrTestLs <- collapseLs <- list()
for(r in 1:nrow(runRiskDf)) {
    
    #
    df2x2 <- collectRiskResLs[[rrDfVec[r]]]
    
    # 
    getResults <- c()
    getLikelihoodRatios <- getLikelihoodRatioTest <- c()
    clpsNull <- clps2x2 <- clps2x2Adj <- c()
    # a <- 1
    for(a in 1:nrow(df2x2)) {
        
        #
        adj2x2 <- outcomeMiscAdj2x2(
            a = df2x2$TP[a], b = df2x2$FN[a],
            c = df2x2$FP[a], d = df2x2$TN[a],
            se1=se1Set, se0=se0Set, sp1=sp1Set, sp0=sp0Set)
        
        rrrdRes <- adjusted_rrrd(adj2x2=adj2x2, runRiskDf=runRiskDf, r=r)
        #
        getResults <- c(getResults, as.numeric(c(
            rrrdRes$adjRR, rrrdRes$adjRRSE,
            rrrdRes$adjRRlci, rrrdRes$adjRRuci,
            rrrdRes$adjRD, rrrdRes$adjRDSE,
            rrrdRes$adjRDlci, rrrdRes$adjRDuci,
            rrrdRes$z)))
        
        # Get likelihood ratios, based on 2x2 table:
        tbl2x2 <- matrix(data=c(df2x2$TN[a],
                                df2x2$FN[a],
                                df2x2$FP[a],
                                df2x2$TP[a]), nrow = 2, byrow = TRUE)
        
        nullTbl <- makeNullTbl(tbl2x2 = tbl2x2)
        
        tbl2x2Adj <- DescTools::Rev(t(adj2x2))
        
        getLikelihoodRatios <- c(getLikelihoodRatios,
                                 as.numeric(posnegLR(tbl2x2 = nullTbl)),
                                 as.numeric(posnegLR(tbl2x2 = tbl2x2)),
                                 as.numeric(posnegLR(tbl2x2 = tbl2x2Adj)))
        
        # Instead of g.test, use 'LR_test_2x2Modified'
        # -------------------------------------------
        lrNull <- LR_test_2x2Modified(nullTbl)
        lr2x2 <- LR_test_2x2Modified(tbl2x2)
        lr2x2Adj <- LR_test_2x2Modified(tbl2x2Adj)
        
        getLikelihoodRatioTest <- c(getLikelihoodRatioTest,
                                    lrNull, lr2x2, lr2x2Adj)
        
        # Check collapsibility
        # --------------------
        clpsNull <- c(clpsNull,
                      as.numeric(checkCollapsibility(tbl2x2 = nullTbl)$mainRes))
        clps2x2 <- c(clps2x2,
                     as.numeric(checkCollapsibility(tbl2x2 = tbl2x2)$mainRes))
        clps2x2Adj <- c(clps2x2Adj,
                        as.numeric(checkCollapsibility(tbl2x2 = tbl2x2Adj)$mainRes))
        
    }
    # 
    getResultsDf <- data.frame(matrix(data=getResults, nrow = nrow(df2x2), byrow = TRUE))
    colnames(getResultsDf) <- c(
        "adjrr", "adjrrSE", "adjrrLci", "adjrrUci",
        "adjrd", "adjrdSE", "adjrdLci", "adjrdUci", "zval")
    adjLs[[resDfVec[r]]] <- data.frame(getResultsDf)
    
    #
    getLikelihoodRatiosDf <- data.frame(matrix(data=getLikelihoodRatios, nrow = nrow(df2x2), byrow = TRUE))
    colnames(getLikelihoodRatiosDf) <- c(
        "posLRNull", "negLRNull", "posLR2x2", "negLR2x2",
        "posLR2x2Adj", "negLR2x2Adj")
    
    posnegLRLs[[resDfVec[r]]] <- getLikelihoodRatiosDf
    
    #
    getLikelihoodRatioTestDf <- data.frame(matrix(data=getLikelihoodRatioTest, nrow = nrow(df2x2), byrow = TRUE))
    colnames(getLikelihoodRatioTestDf) <- c(
        "statNull", "dfNull", "pNull",
        "stat2x2", "df2x2", "p2x2",
        "stat2x2Adj", "df2x2Adj", "p2x2Adj")
    
    lkhrTestLs[[resDfVec[r]]] <- getLikelihoodRatioTestDf
    
    # Get collapsibility results
    # --------------------------
    nullCollapseDf <- data.frame(matrix(data=clpsNull, nrow=nrow(df2x2), byrow = TRUE))
    tbl2x2CollapseDf <- data.frame(matrix(data=clps2x2, nrow=nrow(df2x2), byrow = TRUE))
    tbl2x2AdjDf <- data.frame(matrix(data=clps2x2Adj, nrow=nrow(df2x2), byrow = TRUE))
    colnames(nullCollapseDf) <- colnames(tbl2x2CollapseDf) <- colnames(tbl2x2AdjDf) <- c("absRiskTotal", "riskCollapsed", "oddsTotal", "oddsCollapsed")
    
    collapseLs[[resDfVec[r]]]$nullClps <- nullCollapseDf
    collapseLs[[resDfVec[r]]]$tbl2x2Clps <- tbl2x2CollapseDf
    collapseLs[[resDfVec[r]]]$tbl2x2AdjClps <- tbl2x2AdjDf
}

# =========================
# Save results as rds file:
# -------------------------
saveRDS(object=adjLs,
        file=paste0(nsduhPath, "misclassifAdjustedLs.rds"))
# =========================

df2x2Plr <- data.frame(
    name=c(rep(c("tbl2x2indep", "tbl2x2", "tbl2x2adj"),
    each=nrow(posnegLRLs[[paste0("Result", sel1To15, "resDf")]]))),
    plr=c(
        posnegLRLs[[paste0("Result", sel1To15, "resDf")]]$posLRNull,
        posnegLRLs[[paste0("Result", sel1To15, "resDf")]]$posLR2x2,
        posnegLRLs[[paste0("Result", sel1To15, "resDf")]]$posLR2x2Adj))
df2x2Plr$name <- factor(df2x2Plr$name, levels = c("tbl2x2indep", "tbl2x2", "tbl2x2adj"))

source(paste0(nsduhPath, "nsduhPLRplot.R"), echo = FALSE)
# Display in R console
plrPlot


df2x2Nlr <- data.frame(
    name=c(rep(c("tbl2x2indep", "tbl2x2", "tbl2x2adj"),
    each=nrow(posnegLRLs[[paste0("Result", sel1To15, "resDf")]]))),
    nlr=c(
        posnegLRLs[[paste0("Result", sel1To15, "resDf")]]$negLRNull,
        posnegLRLs[[paste0("Result", sel1To15, "resDf")]]$negLR2x2,
        posnegLRLs[[paste0("Result", sel1To15, "resDf")]]$negLR2x2Adj))
df2x2Nlr$name <- factor(df2x2Nlr$name, levels = c("tbl2x2indep", "tbl2x2", "tbl2x2adj"))

# Want to visualize?
# ------------------
source(paste0(nsduhPath, "nsduhNLRplot.R"), echo = FALSE)
# Display in R console
nlrPlot

# Equality of chi squared statistic between using logistic regression model difference (null and hypothesized model) and when instead using the 2x2 matrix, to compute this chi squared statistic.
dplyr::near(
    x=collectRiskResLs[[paste0("Result", sel1To15, "resDf")]][,"rrChisq"],
    y=lkhrTestLs[[paste0("Result", sel1To15, "resDf")]]$stat2x2)

checkNullCollapse(nullClps = collapseLs[[paste0("Result", sel1To15, "resDf")]]$nullClps)

clps <- prepareCollapse(
    tbl2x2Clps = collapseLs[[paste0("Result", sel1To15, "resDf")]]$tbl2x2Clps,
    tbl2x2AdjClps = collapseLs[[paste0("Result", sel1To15, "resDf")]]$tbl2x2AdjClps)
clps$name <- factor(clps$name, levels = c("tbl2x2", "tbl2x2Adj"))

# Want to visualize?
# ------------------
source(paste0(nsduhPath, "nsduhCollapsePlot.R"), echo = FALSE)
# Display in R console
clpsPlot
# -------------------------------------------------
# -------------------------------------------------

# ----------------------------------------
# Meta-Analysis of outcome misclassif. bias adjusted associative stat. tests
# ----------------------------------------

adjLs <- readRDS(file=paste0(nsduhPath, "misclassifAdjustedLs.rds"))

resDfVec <- names(collectRiskResLs)[seq(1,length(collectRiskResLs),by=4)]

# Risk Ratio (rr)
# ---------------
(meta.rrAdj <- meta::metagen(TE=log(adjrr), lower = log(adjrrLci), upper = log(adjrrUci), sm="RR", prediction = TRUE, data = adjLs[[resDfVec[sel1To15]]], level.ci = 1-runRiskDf$a[sel1To15]))

# Extract five results: The meta effect estimate, the lower bound of the compatibility interval (from the random effects model and the prediction interval), the upper bound of the compatibility interval (same specification as with the lower bound).
metaResRRadj <- c(meta.rrAdj$TE.random,
                  meta.rrAdj$lower.random, meta.rrAdj$upper.random,
                  meta.rrAdj$lower.predict, meta.rrAdj$upper.predict)
# Display results as risk ratios in the R console:
exp(metaResRRadj)

# Make data.frame for plotting:
(adjrrDf <- data.frame(model="adjRRMeta",
                       est=exp(metaResRRadj)[1],
                       lci=exp(metaResRRadj)[4],
                       uci=exp(metaResRRadj)[5]))

# Want to visualize?
# BEWARE: First, you must run the code of steps 7 (Multilevel regression) and 8 (conventional meta-analysis), before running this code line (820).
# ------------------
source(paste0(nsduhPath, "nsduhRRMetaAdjForestPlot.R"), echo = FALSE)
# Display in R console
plotRRAdjForest
# Adjust x-axis (show tick marks 1, 1.2, 1.5, 1.7, and 2)
(plotRRAdjForest <- plotRRAdjForest +
        scale_x_continuous(trans = "log2",
                           breaks = c(1, 1.2, 1.5, 1.7, 2)))

# Want to visualize differently?
# Adjustment = Empirical adjustment of outcome misclassification bias.
# ------------------
source(paste0(nsduhPath, "nsduhRRAdjForestPlot.R"), echo = FALSE)
# Display in R console
plotRisk
# Adjust x-axis (log scaled)
(plotRisk <- plotRisk + scale_x_continuous(trans = "log2"))
# --------------------

# Risk Difference (rd)
# --------------------
(meta.rdAdj <- meta::metagen(TE=adjrd, lower = adjrdLci, upper = adjrdUci, sm="RD", prediction = TRUE, data = adjLs[[resDfVec[sel1To15]]], level.ci = 1-runRiskDf$a[sel1To15]))

# Extract five results: The meta effect estimate, the lower bound of the compatibility interval (from the random effects model and the prediction interval), the upper bound of the compatibility interval (same specification as with the lower bound).
metaResRDadj <- c(meta.rdAdj$TE.random,
                  meta.rdAdj$lower.random, meta.rdAdj$upper.random,
                  meta.rdAdj$lower.predict, meta.rdAdj$upper.predict)
# Display results (risk difference) in the R console:
metaResRDadj

# Make data.frame for plotting:
(adjrdDf <- data.frame(model="adjRDMeta",
                      est=metaResRDadj[1],
                      lci=metaResRDadj[4],
                      uci=metaResRDadj[5]))

# Want to visualize?
# ------------------
# BEWARE: First, you must run the code of steps 7 (Multilevel regression) and 8 (conventional meta-analysis), before running this code line (858).
source(paste0(nsduhPath, "nsduhRDMetaAdjForestPlot.R"), echo = FALSE)
# Display in R console
plotRDAdjForest

# Want to visualize differently?
# Adjustment = Empirical adjustment of outcome misclassification bias.
# ------------------
source(paste0(nsduhPath, "nsduhRDAdjForestPlot.R"), echo = FALSE)
# Display in R console
plotRisk
# --------------------------------------------------------------