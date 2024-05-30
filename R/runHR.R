# Compute number of samples, each of requiredN many individuals.
(possibleSamples <- nrow(riskData)/requiredN)
(remainingN <- nrow(riskData)-(floor(possibleSamples)*requiredN))

# Pick floor(possibleSamples) many samples, each having requiredN many individuals
pick <- rep(1:floor(possibleSamples), each=requiredN)
# Produce a random order of these floor(possibleSamples) many samples (guarantee reproducibility, set.seed(259))
set.seed(259)
smpl <- sample(pick)
# Remove remainingN many individuals from the total valid sample
set.seed(4)
riskData1 <- riskData[-sample(x=1:nrow(riskData), size = remainingN),]
dim(riskData1)
length(smpl)
smpl[1:5]
riskData1$smpl <- smpl

# hazard ratio collect (results)
hr_clct <- c()
hrMeta <- c()
start <- Sys.time()
# i <- 1
for(i in 1:floor(possibleSamples)) {
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # ----------
    # Hazard ratio
    # ----------
    
    hrModNull <- survival::coxph(Surv(ageClass, ltmde) ~ 1, data=riskData1[smpl==i,])
    hrMod_i <- survival::coxph(Surv(ageClass, ltmde) ~ sex, data=riskData1[smpl==i,])
    
    # https://adibender.github.io/pammtools/articles/strata.html
    hrMod_i <- survival::coxph(Surv(ageClass, ltmde) ~ strata(sex), data=riskData1[smpl==i,])
    fithaz <- basehaz(hrMod_i)
    ggplot(data=fithaz, aes(x=time)) + geom_step(aes(y=hazard, group=strata))# + ylim(0,1)
    
    # Cox regression test assumptions
    # -------------------------------
    # ----------------
    # Test proportional hazards (statistical test should not be stat. significant, which indicates that the proportional hazards assumption may be valid.)
    test.ph <- cox.zph(hrMod_i)
    phTestp <- test.ph$table[1,"p"]
    # # Want to plot the proportional hazards assumption test result: The survminer package must be installed.
    # # ?survminer::ggcoxzph
    # survminer::ggcoxzph(test.ph, df=3)
    # # ----------------
    # # ggcoxdiagnostics, also from the survminer package:
    # # Data points should be roughly symmetrical around the zero horizontal line.
    # # Positive values = Individuals that experienced the event sooner than expected, based on the modelled expected survival times.
    # # Negative values = Individuals that experienced the event later than expected, based on the modelled expected survival times.
    # # Values far away from the zero horizontal: These individuals were poorly predicted by the model.
    # survminer::ggcoxdiagnostics(fit=hrMod_i, type="deviance", linear.predictions = FALSE)
    # # ----------------
    
    hr_i <- as.numeric(exp(hrMod_i$coefficients["sex"]))
    hr_clct <- c(hr_clct, hr_i)
    
    # Confidence intervals for coefficients: (normality-based)
    hr_coefs <- coefficients(summary(hrMod_i))
    # Left 95 CI of one-sided test
    hrl95_i <- as.numeric(exp(hrMod_i$coefficients["sex"] - czVal * hr_coefs["sex", "se(coef)"]))
    # # Right 95 CI of one-sided test does not exist: NA (= Inf)
    hru95_i <- Inf
    
    hr_clct <- c(hr_clct, hrl95_i, hru95_i)
    
    # One-sided p value
    hrMod_iP <- pnorm(hr_coefs["sex","z"], lower.tail = FALSE)
    
    hr_clct <- c(hr_clct, hrMod_iP, phTestp)
    
    # Specific null hypothesis, against the directed, yet unspecified alternative hypothesis.
    # ------------------------
    smryhrMod_i <- summary(hrMod_i)
    hr0 <- log(hrH0)
    hr_q_i <- as.numeric((hrMod_i$coefficients["sex"] - hr0)/coefficients(smryhrMod_i)[,"se(coef)"])
    hrMod_iP_minH0 <- as.numeric(pnorm(q=hr_q_i, lower.tail = FALSE))
    
    hr_clct <- c(hr_clct, hr_q_i, hrMod_iP_minH0)
    
    # # --------------
    # # log-likelihood for hazard ratio
    # # --------------
    hr_lgkNull <- logLik(hrModNull)
    hr_lgkMod <- logLik(hrMod_i)
    hr_chisqVal <- -2*(as.numeric(logLik(hrModNull)) - as.numeric(logLik(hrMod_i)))
    hr_lrTestp <- 1 - pchisq(hr_chisqVal, df=1)
    
    hr_clct <- c(hr_clct, as.numeric(c(hr_lgkNull, hr_lgkMod)), hr_chisqVal, 1, hr_lrTestp)
    # # --------------------------------------------
    
    # Usual (two-sided) risk ratio logistic regression
    # Cells of 2x2 table in this order, when as.vector: TN, FP, FN, TP.
    hrci_i <- as.numeric(exp(confint.default(hrMod_i)))
    hrMeta <- c(hrMeta, hr_i, hrci_i)
    
}
stop <- Sys.time()
print(difftime(stop, start))

resDf <- data.frame(matrix(data=hr_clct, nrow = floor(possibleSamples), byrow = TRUE))

colnames(resDf) <- c(
    
    "hr", paste0("hrl", ciXX), paste0("hru", ciXX),
    "hrp", "phTestp", "hrzsH0", "hrpsH0",
    "hrllkNull", "hrllkMod", "hrChisq", "hrdf", "hrllkp"
)

hrDf <- data.frame(matrix(data=hrMeta, nrow = floor(possibleSamples), byrow = TRUE))
summary(hrDf$hr)
colnames(hrDf) <- c("hr", paste0("hrl", ciXX), paste0("hru", ciXX))