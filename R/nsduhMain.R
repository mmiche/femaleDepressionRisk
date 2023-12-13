# Data: National Survey on Drug Use and Health (NSDUH) 2012-2021
# --------------------------------------------------------------
#
timeAllStart <- Sys.time()
# x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x
# MOST IMPORTANT: First, open and run the R script nsduhStart.R.
# It contains packages and custom functions that are required in
# this script and in the script nsduhResults.R.
# x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x-x
#
# Read all custom or modified functions from separate script
source(paste0(nsduhPath, "nsduhFuns.R"), echo = FALSE)
#
# Read the data.
datAll <- readRDS(file = paste0(nsduhPath, "nsduh1221.rds"))
#
# Compare codebooks that are available from the same website, from which the NSDUH data can be downloaded.
# irsex = 1 (males), irsex = 2 (females)
# catage (age categories: 12-17, 18-25, 26-34, 35+)
# amdelt: page 468 of Codebook 2015
colnames(datAll)[1:3] <- c("ltmde", "sex", "ageClass")
# Age class 1 (12-17 years) have no diagnosis of MDE (missing data), therefore remove age class 1.
datAll <- datAll[!is.na(datAll$ltmde),]
# Re-code variables ltmde, sex and ageClass, by subtracting 1.
# ltmde (1 = yes, 2 = no; new: 0 = no, 1 = yes)
datAll$ltmde <- abs(datAll$ltmde - 2)
# sex (1 = male, 2 = female; new: 0 = male, 1 = female)
datAll$sex <- datAll$sex - 1
# ageClass (2 = 18-25 years, 3 = 26-34, 4 = 35+)
datAll$ageClass <- datAll$ageClass - 1
# Are there any more missing values across the selected columns?
idxNA <- apply(datAll[,c("ltmde", "sex", "ageClass")], 1, function(x) any(is.na(x)))
# How many missing values?
length(which(idxNA)) # 0, meaning no more missing values.
# ----------------------------
# Rename the data; then remove datAll from workspace. 
riskData <- datAll; rm(datAll)
dim(riskData) # 399751 rows and 4 columns
mean(riskData$ltmde) # Outcome prevalence ltMDE = 16.45%
# Exposure variable is sex (0 = male, 1 = female).
mean(riskData$sex) # 54% females.
# Age classes: 18-25 years = 34.9%, 26-34 years = 19.4%, 35+ years = 45.7%
prop.table(table(riskData$ageClass))
# ----------------------------
# Prepare analyzing the data: Investigate the empirical claim of females having twice the risk to develop a lifetime major depressive episode, compared to men. This is a directed (right-sided) hypothesis.
#
# ---------------------------------------
# Risk ratio, risk difference, odds ratio
# ---------------------------------------
#
runRiskDf <- data.frame(
    p0 = rep(c(.12, .11, .1, .11, .12), times=3),
    p1 = rep(c(.24, .22, .2, .2, .2), times=3),
    a=c(rep(.05, times=5), rep(.01, times=5), rep(.005, times=5))
)
# Compute sample size for different choices of a priori effect size, alpha level, and power.
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
# Compute the required sample size
runRiskDf$requiredN <- ceiling(runRiskDf$exactN)
# Select power to be 1 minus the alpha level (we prefer very high power!)
runRiskDf$power <- 1-runRiskDf$a
# Determine the critical z value (standard normal distribution (mu = 0, sd = 1)) for each alpha level.
runRiskDf$czVal <- qnorm(p=1-runRiskDf$a)
# In addition to the conventional null hypothesis, we test specific null hypotheses, which express a smallest effect size of interest (SESOI).
# For SESOI, compare Lakens et al., 2018, https://doi.org/10.1177/2515245918770963
runRiskDf$rrH0 <- 1.2
runRiskDf$rdH0 <- .02
runRiskDf$orH0 <- 1.2
#
# ---------------------------------------
# 1. Risk ratio
# 2. risk difference
# 3. odds ratio
# ---------------------------------------
#
# Run the analyses, use separate R script 'runRR_RD_OR.R'
# Collect results in these three lists:
collectRiskResLs <- collectRisklme4Ls <- collectCollapsedLs <- list()
# r <- 1 # Set r to a value between 1 and 15, in order to run a single regression setup (setup = rows of runRiskDf). If you want to run a single setup, you will have to mark the code inside the for-loop (script lines 94-117), then execute it.
for(r in 1:nrow(runRiskDf)) {
    
    print(paste0("Start of run ", r))
    
    requiredN <- runRiskDf$requiredN[r]
    czVal <- runRiskDf$czVal[r]
    rrH0 <- runRiskDf$rrH0[r]
    rdH0 <- runRiskDf$rdH0[r]
    orH0 <- runRiskDf$orH0[r]
    
    confLevel <- 1-runRiskDf$a[r]
    ciString <- as.character(1-runRiskDf$a[r])
    ciXX <- stringr::str_sub(ciString, 3, nchar(ciString))
    
    source(paste0(nsduhPath, "runRR_RD_OR.R"), echo = FALSE)
    
    collectRiskResLs[[paste0("Result",r, "resDf")]] <- resDf
    collectRiskResLs[[paste0("Result",r, "rrDf")]] <- rrDf
    collectRiskResLs[[paste0("Result",r, "rdDf")]] <- rdDf
    collectRiskResLs[[paste0("Result",r, "orDf")]] <- orDf
    
    # collectRisklme4Ls[[paste0("Result",r, "lme4")]] <- resLme4
    
    collectCollapsedLs[[paste0("Result", r, "collapsedDf")]] <- collapsibilityDf
    
    print(paste0("End of run ", r))
}
#
# =========================
# Save results as rds file:
# -------------------------
saveRDS(object=collectRiskResLs,
        file=paste0(nsduhPath, "collectRiskResLs.rds"))

saveRDS(object=collectRisklme4Ls,
        file=paste0(nsduhPath, "collectRisklme4Ls.rds"))

saveRDS(object=collectCollapsedLs,
        file=paste0(nsduhPath, "collectCollapsedLs.rds"))
# =========================

# ---------------------------------------
# 4. Hazard ratio
# ---------------------------------------

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
# Collect results in this list:
collectCoxResLs <- list()
# r <- 1 # Set r to a value between 1 and 9, in order to run a single Cox regression setup (setup = rows of runCoxDf). If you want to run a single setup, you will have to mark the code inside the for-loop (script lines 162-177), then execute it.
for(r in 1:nrow(runCoxDf)) {
    
    print(paste0("Start of run ", r))
    
    requiredN <- runCoxDf$requiredN[r]
    czVal <- runCoxDf$czVal[r]
    hrH0 <- runCoxDf$hrH0[r]
    
    confLevel <- 1-runCoxDf$a[r]
    ciString <- as.character(1-runCoxDf$a[r])
    ciXX <- stringr::str_sub(ciString, 3, nchar(ciString))
    
    source(paste0(nsduhPath, "runHR.R"), echo = FALSE)
    
    collectCoxResLs[[paste0("Result",r, "resDf")]] <- resDf
    collectCoxResLs[[paste0("Result",r, "hrDf")]] <- hrDf
    
    print(paste0("End of run ", r))
}

# =========================
# Save results as rds file:
# -------------------------
saveRDS(object=collectCoxResLs,
        file=paste0(nsduhPath, "collectCoxResLs.rds"))
# =========================

# ----------------------------------------
# 5. Difference between two independent proportions
# 5.1 Unequal and equal sample size in both groups.
# ----------------------------------------

# -------------------------------------
# Test assumptions for z-Test of two independent proportions.
# https://statkat.com/stat-tests/z-test-for-the-difference-between-two-proportions.php#4
# 1. Sample size is large enough for z to be approximately normally distributed.
# Yes.
#
# 2. Within and between groups, observations are independent of one another.
# Yes.
# -------------------------------------
n1n2Ratio <- (100-53.9)/(100-(100-53.9))

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
# Collect results in these two lists:
collectPrpResLsNeq <- collectPrpResLsEq <- list()
for(r in 1:nrow(runPropTstDf)) {
    
    print(paste0("Start of run ", r))
    
    confLevel <- 1-runPropTstDf$a[r]
    
    # Ratio of male to female sample size = 0.855:1 (see n1n2Ratio)
    kappa <- "unequalSampleSize"
    requiredN1 <- runPropTstDf$n1.855[r]
    requiredN2 <- runPropTstDf$n2.855[r]
    source(paste0(nsduhPath, "runProportionsTest.R"), echo = FALSE)
    collectPrpResLsNeq[[paste0("Result",r, "prpTstDf")]] <- propRes
    
    # Ratio of male to female sample size = 1:1
    kappa <- "sameSampleSize"
    requiredNEach <- runPropTstDf$n1[r]
    source(paste0(nsduhPath, "runProportionsTest.R"), echo = FALSE)
    collectPrpResLsEq[[paste0("Result",r, "prpTstDf")]] <- propRes
    
    print(paste0("End of run ", r))
}

# =========================
# Save results as rds file:
# -------------------------
saveRDS(object=collectPrpResLsNeq,
        file=paste0(nsduhPath, "collectPropTestResLsNeq.rds"))

saveRDS(object=collectPrpResLsEq,
        file=paste0(nsduhPath, "collectPropTestResLsEq.rds"))
# =========================
# -------------------------------------------------
timeAllEnd <- Sys.time()
difftime(timeAllEnd, timeAllStart)
# -------------------------------------------------
# Bayes logistic regression (brms package)
# -------------------------
# See R script nsduhResult.R, script lines 520-570.
# -------------------------------------------------