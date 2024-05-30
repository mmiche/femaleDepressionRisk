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
# dim(riskData1)
# length(smpl)
riskData1$smpl <- smpl

i <- 1
rr_rd <- c()
start <- Sys.time()
for(i in 1:floor(possibleSamples)) {
    
    tbl2x2 <- table(riskData1[smpl==i,c("sex", "ltmde")])
    
    # # -------------------------------------------------------
    # # Prepare manual computations of RR, RD, and OR, based on 2x2 table.
    # # See below: Manually compute standard error of RR and RD, and X% CI.
    # # -------------------------------------------------------
    
    tbl2x2Full <- addmargins(tbl2x2)
    wt0 <- tbl2x2Full[1,3]/tbl2x2Full[3,3]
    wt1 <- tbl2x2Full[2,3]/tbl2x2Full[3,3]
    proptbl2x2 <- addmargins(prop.table(tbl2x2))
    absRiskTotal <- proptbl2x2[3,2]
    oddsTotal <- proptbl2x2[3,2]/proptbl2x2[3,1]
    
    perspective2 <- addmargins(prop.table(tbl2x2, margin=1), margin=2)
    # absrisk: absolute risk of developing the outcome;
    # 0 = among males; 1 = among females.
    absrisk0 <- perspective2[1,2]
    absrisk1 <- perspective2[2,2]
    
    # Collect total outcome risk and conditional outcome risks
    rr_rd <- c(rr_rd, absRiskTotal, absrisk0, absrisk1)
    
    # # -------------------------------------------------------
    # -------------------------------------
    # Logistic regression test assumptions:
    # -------------------------------------
    # http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/#logistic-regression-assumptions
    #
    # 1. Outcome is binary:
    # Diagnosis of lifetime major depressive episode yes versus no.
    #
    # 2. Linear relationship between the logit of the outcome and each predictor variable.
    # Only predictor variable is sex (male versus female). Linear relationship is assumed, based on the absence of sufficient evidence of a non-linear relationship.
    #
    # 3. No influential values (extreme values or outliers) in the continuous predictors.   #
    # Does not apply in this study, as there are no continuous predictors.
    #
    # 4. No high intercorrelations (i.e. multicollinearity) among the predictors.
    # Does not apply in this study, as there is just one predictor.
    # -------------------------------------
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # ----------
    # Risk ratio (see Naimi et al., 2020, Table 2, standard errors = model based [one other option])
    # ----------
    rrModNull <- glm(ltmde ~ 1, family=binomial(link="log"), data=riskData1[smpl==i,])
    rrMod_i <- glm(ltmde ~ sex, family=binomial(link="log"), data=riskData1[smpl==i,])
    
    rr_i <- exp(rrMod_i$coefficients["sex"])
    rr_rd <- c(rr_rd, rr_i)
    
    # Confidence intervals for coefficients: (normality-based)
    rr_coefs <- coefficients(summary(rrMod_i))
    # Left 95 CI of two-sided test
    czVal2sided <- qnorm(runRiskDf$a[r]/2, lower.tail = FALSE)
    rrlCI_i <- exp(rrMod_i$coefficients["sex"] - czVal2sided * rr_coefs["sex", "Std. Error"])
    # # Right 95 CI of two-sided test
    rruCI_i <- exp(rrMod_i$coefficients["sex"] + czVal2sided * rr_coefs["sex", "Std. Error"])
    
    rr_rd <- c(rr_rd, rrlCI_i, rruCI_i)
    
    # Empirical p value
    if(rr_i < 1) {
        rrMod_iP <- pnorm(rr_coefs["sex","z value"], lower.tail = TRUE)*2
    } else if(rr_i > 1) {
        rrMod_iP <- pnorm(rr_coefs["sex","z value"], lower.tail = FALSE)*2
    } else {
        rrMod_iP <- 1
    }
    
    rr_rd <- c(rr_rd, rrMod_iP)
    
    # Specific null hypothesis, against the directed, yet unspecified alternative hypothesis.
    # ------------------------
    smryrrMod_i <- summary(rrMod_i)
    rr0 <- log(rrH0)
    rr_q_i <- (rrMod_i$coefficients["sex"] - rr0)/coefficients(smryrrMod_i)[2,"Std. Error"]
    if(rr_q_i < 0) {
        rrMod_iP_minH0 <- pnorm(q=rr_q_i, lower.tail = TRUE)*2
    } else if(rr_q_i > 0) {
        rrMod_iP_minH0 <- pnorm(q=rr_q_i, lower.tail = FALSE)*2
    } else {
        rrMod_iP_minH0
    }
    
    
    rr_rd <- c(rr_rd, rr_q_i, rrMod_iP_minH0)
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # ---------------
    # Risk difference (see Naimi et al., 2020, Table 2, standard errors = model based [two other options])
    # ---------------
    rdModNull <- glm(ltmde ~ 1, family=binomial(link="identity"), data=riskData1[smpl==i,])
    rdMod_i <- glm(ltmde ~ sex, family=binomial(link="identity"), data=riskData1[smpl==i,])
    
    rd_i <- rdMod_i$coefficients["sex"]
    rr_rd <- c(rr_rd, rd_i)
    
    rd_coefs <- coefficients(summary(rdMod_i))
    # Left XX CI of two-sided test
    czVal2sided <- qnorm(runRiskDf$a[r]/2, lower.tail = FALSE)
    rdlCI_i <- rdMod_i$coefficients["sex"] - czVal2sided * rd_coefs["sex", "Std. Error"]
    # Right XX CI of two-sided test
    rduCI_i <- rdMod_i$coefficients["sex"] + czVal2sided * rd_coefs["sex", "Std. Error"]
    
    rr_rd <- c(rr_rd, rdlCI_i, rduCI_i)
    
    # # Two-sided p value from regression model.
    # rdMod_iP <- coefficients(summary(rdMod_i))["sex","Pr(>|z|)"]
    
    # Empirical p value
    if(rd_i < 0) {
        rdMod_iP <- pnorm(rd_coefs["sex","z value"], lower.tail = TRUE)*2
    } else if(rd_i > 0) {
        rdMod_iP <- pnorm(rd_coefs["sex","z value"], lower.tail = FALSE)*2
    } else {
        rdMod_iP <- 1
    }
    
    rr_rd <- c(rr_rd, rdMod_iP)
    
    # Specific null hypothesis, against the directed, yet unspecified alternative hypothesis.
    # ------------------------
    smryrdMod_i <- summary(rdMod_i)
    rd0 <- rdH0
    rd_q_i <- (rdMod_i$coefficients["sex"] - rd0)/coefficients(smryrdMod_i)[2,"Std. Error"]
    if(rd_q_i < 0) {
        rdMod_iP_minH0 <- pnorm(q=rd_q_i, lower.tail = TRUE)*2
    } else if(rd_q_i > 0) {
        rdMod_iP_minH0 <- pnorm(q=rd_q_i, lower.tail = FALSE)*2
    } else {
        rdMod_iP_minH0
    }
    
    rr_rd <- c(rr_rd, rd_q_i, rdMod_iP_minH0)
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    nM <- tbl2x2Full[1,3] # Number of males
    nF <- tbl2x2Full[2,3] # Number of females
    rr_rd <- c(rr_rd, nM, nF)
    
    # print(floor(possibleSamples)-i)
}
stop <- Sys.time()
print(difftime(stop, start))

resDf <- data.frame(matrix(data=rr_rd, nrow = floor(possibleSamples), byrow = TRUE))

colnames(resDf) <- c(
    
    "ltmde", "rskM", "rskF",
    
    "rr",
    paste0("rrl", ciXX), paste0("rru", ciXX),
    "rrp", "rrzsH0", "rrpsH0",
    
    "rd",
    paste0("rdl", ciXX), paste0("rdu", ciXX),
    "rdp", "rdzsH0", "rdpsH0",
    
    "nMale", "nFemale"
)
# -------------------------------------------------
