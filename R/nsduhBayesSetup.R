# Run code that is necessary to execute the Bayes Logistic regression.

# --------------------------------------
# This code (script lines 5-23) is the exact same code as in R script nsduhMain, script lines 51-71.
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
# --------------------------------------

# --------------------------------------
# This code (script lines 28-37) is the exact same code as in R script nsduhMain, script lines 12-37.
source(paste0(nsduhPath, "nsduhFuns.R"), echo = FALSE)
datAll <- readRDS(file = paste0(nsduhPath, "nsduh1221.rds"))
colnames(datAll)[1:3] <- c("ltmde", "sex", "ageClass")
datAll <- datAll[!is.na(datAll$ltmde),]
datAll$ltmde <- abs(datAll$ltmde - 2)
datAll$sex <- datAll$sex - 1
datAll$ageClass <- datAll$ageClass - 1
idxNA <- apply(datAll[,c("ltmde", "sex", "ageClass")], 1, function(x) any(is.na(x)))
length(which(idxNA)) # 0, meaning no more missing values.
riskData <- datAll; rm(datAll)
# --------------------------------------
#
# --------------------------------------
# This code (script lines 42-50) is the exact same code as in R script runRR_RD_OR, script lines 2-15.
requiredN <- runRiskDf$requiredN[sel1To15]
(possibleSamples <- nrow(riskData)/requiredN)
(remainingN <- nrow(riskData)-(floor(possibleSamples)*requiredN))
pick <- rep(1:floor(possibleSamples), each=requiredN)
set.seed(259)
smpl <- sample(pick)
set.seed(4)
riskData1 <- riskData[-sample(x=1:nrow(riskData), size = remainingN),]
riskData1$smpl <- smpl
# --------------------------------------