# Run code that is necessary to execute the Bayes Logistic regression.

# --------------------------------------
# This code (script lines 5-14) is the exact same code as in R script nsduhMain, script lines 12-37.
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
# This code (script lines 19-27) is the exact same code as in R script runRR_RD_OR, script lines 2-15.
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