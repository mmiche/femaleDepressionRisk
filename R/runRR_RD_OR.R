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

# i <- 1
rr_rd_or <- c()
rrMeta <- rdMeta <- orMeta <- c()
risk_oddsCollapsed <- c()
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
    rr_rd_or <- c(rr_rd_or, absRiskTotal, absrisk0, absrisk1)

    # # Odds
    odds0 <- absrisk0/(1-absrisk0)
    odds1 <- absrisk1/(1-absrisk1)
    
    rrCollapsed <- wt0*absrisk0 + wt1*absrisk1
    orCollapsed <- wt0*odds0 + wt1*odds1
    risk_oddsCollapsed <- c(risk_oddsCollapsed,
                            absRiskTotal, rrCollapsed,
                            oddsTotal, orCollapsed)
    
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
    rr_rd_or <- c(rr_rd_or, rr_i)
    
    # -------------------------------------------------------
    # Manually, as shown in Yorlets et al. 2023 -------------
    selnrr <- seLnRR(tbl2x2=t(DescTools::Rev(tbl2x2)))
    rrlCI_im <- exp(rrMod_i$coefficients["sex"] - czVal * selnrr)
    # Manually, as shown in Yorlets et al. 2023 -------------
    # -------------------------------------------------------
    
    # Confidence intervals for coefficients: (normality-based)
    rr_coefs <- coefficients(summary(rrMod_i))
    # Left 95 CI of one-sided test
    rrlCI_i <- exp(rrMod_i$coefficients["sex"] - czVal * rr_coefs["sex", "Std. Error"])
    # # Right 95 CI of one-sided test
    rruCI_i <- Inf
    
    rr_rd_or <- c(rr_rd_or, rrlCI_i, rrlCI_im, rruCI_i)
    
    # One-sided p value
    rrMod_iP <- pnorm(rr_coefs["sex","z value"], lower.tail = FALSE)
    rr_rd_or <- c(rr_rd_or, rrMod_iP)
    
    # Specific null hypothesis, against the directed, yet unspecified alternative hypothesis.
    # ------------------------
    smryrrMod_i <- summary(rrMod_i)
    rr0 <- log(rrH0)
    rr_q_i <- (rrMod_i$coefficients["sex"] - rr0)/coefficients(smryrrMod_i)[2,"Std. Error"]
    rrMod_iP_minH0 <- pnorm(q=rr_q_i, lower.tail = FALSE)
    
    rr_rd_or <- c(rr_rd_or, rr_q_i, rrMod_iP_minH0)
    
    # --------------
    # log-likelihood for risk ratio
    # --------------
    rr_lgkNull <- logLik(rrModNull)
    rr_lgkMod <- logLik(rrMod_i)
    rr_chisqVal <- -2*(as.numeric(logLik(rrModNull)) - as.numeric(logLik(rrMod_i)))
    rr_lrTestp <- 1 - pchisq(rr_chisqVal, df=1)
    
    rr_rd_or <- c(rr_rd_or, as.numeric(c(rr_lgkNull, rr_lgkMod)), rr_chisqVal, 1, rr_lrTestp)
    # --------------------------------------------
    
    # Usual (two-sided) risk ratio logistic regression
    # Cells of 2x2 table in this order, when as.vector: TN, FP, FN, TP.
    rrci_i <- as.numeric(exp(confint.default(rrMod_i)[2,]))
    rrMeta <- c(rrMeta, as.vector(tbl2x2), rr_i, rrci_i)
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # ---------------
    # Risk difference (see Naimi et al., 2020, Table 2, standard errors = model based [two other options])
    # ---------------
    rdModNull <- glm(ltmde ~ 1, family=binomial(link="identity"), data=riskData1[smpl==i,])
    rdMod_i <- glm(ltmde ~ sex, family=binomial(link="identity"), data=riskData1[smpl==i,])
    
    rd_i <- rdMod_i$coefficients["sex"]
    rr_rd_or <- c(rr_rd_or, rd_i)
    
    # -------------------------------------------------------
    # Manually, as shown in Yorlets et al. 2023 -------------
    selnrd <- seRD(tbl2x2=t(DescTools::Rev(tbl2x2)))
    rdlCI_im <- rdMod_i$coefficients["sex"] - czVal * selnrd
    # Manually, as shown in Yorlets et al. 2023 -------------
    # -------------------------------------------------------
    
    rd_coefs <- coefficients(summary(rdMod_i))
    # Left 95 CI of one-sided test
    rdlCI_i <- rdMod_i$coefficients["sex"] - czVal * rd_coefs["sex", "Std. Error"]
    # # Right 95 CI of one-sided test does not exist: NA (= Inf)
    rduCI_i <- 1
    
    rr_rd_or <- c(rr_rd_or, rdlCI_i, rdlCI_im, rduCI_i)
    
    # # Two-sided p value from regression model.
    # rdMod_iP <- coefficients(summary(rdMod_i))["sex","Pr(>|z|)"]
    
    # One-sided p value
    rdMod_iP <- pnorm(rd_coefs["sex","z value"], lower.tail = FALSE)
    rr_rd_or <- c(rr_rd_or, rdMod_iP)
    
    # Specific null hypothesis, against the directed, yet unspecified alternative hypothesis.
    # ------------------------
    smryrdMod_i <- summary(rdMod_i)
    rd0 <- rdH0
    rd_q_i <- (rdMod_i$coefficients["sex"] - rd0)/coefficients(smryrdMod_i)[2,"Std. Error"]
    rdMod_iP_minH0 <- pnorm(q=rd_q_i, lower.tail = FALSE)
    
    rr_rd_or <- c(rr_rd_or, rd_q_i, rdMod_iP_minH0)
    
    # --------------
    # log-likelihood for risk difference
    # --------------
    rd_lgkNull <- logLik(rdModNull)
    rd_lgkMod <- logLik(rdMod_i)
    rd_chisqVal <- -2*(as.numeric(logLik(rdModNull)) - as.numeric(logLik(rdMod_i)))
    rd_lrTestp <- 1 - pchisq(rd_chisqVal, df=1)
    
    rr_rd_or <- c(rr_rd_or, as.numeric(c(rd_lgkNull, rd_lgkMod)), rd_chisqVal, 1, rd_lrTestp)
    # --------------------------------------------
    
    # Usual (two-sided) risk ratio logistic regression
    # Cells of 2x2 table in this order, when as.vector: TN, FP, FN, TP.
    rdci_i <- as.numeric(confint.default(rrMod_i)[2,])
    rdMeta <- c(rdMeta, as.vector(tbl2x2), rd_i, rdci_i)
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # ----------
    # Odds Ratio (see Naimi et al., 2020, Table 2, standard errors = model based [no other option])
    # ----------
    orModNull <- glm(ltmde ~ 1, family=binomial(link="logit"), data=riskData1[smpl==i,])
    orMod_i <- glm(ltmde ~ sex, family=binomial(link="logit"), data=riskData1[smpl==i,])
    
    or_i <- exp(orMod_i$coefficients["sex"])
    rr_rd_or <- c(rr_rd_or, or_i)
    
    or_coefs <- coefficients(summary(orMod_i))
    # Left 95 CI of one-sided test
    orlCI_i <- exp(orMod_i$coefficients["sex"] - czVal * or_coefs["sex", "Std. Error"])
    # # Right 95 CI of one-sided test does not exist: NA (= Inf)
    oruCI_i <- Inf
    
    rr_rd_or <- c(rr_rd_or, orlCI_i, oruCI_i)
    
    # One-sided p value
    orMod_iP <- pnorm(or_coefs["sex","z value"], lower.tail = FALSE)
    rr_rd_or <- c(rr_rd_or, orMod_iP)
    
    # Specific null hypothesis, against the directed, yet unspecified alternative hypothesis.
    # ------------------------
    smryorMod_i <- summary(orMod_i)
    or0 <- log(orH0)
    or_q_i <- (orMod_i$coefficients["sex"] - or0)/coefficients(smryorMod_i)[2,"Std. Error"]
    orMod_iP_minH0 <- pnorm(q=or_q_i, lower.tail = FALSE)
    
    rr_rd_or <- c(rr_rd_or, or_q_i, orMod_iP_minH0)
    
    # --------------
    # log-likelihood for odds ratio
    # --------------
    or_lgkNull <- logLik(orModNull)
    or_lgkMod <- logLik(orMod_i)
    or_chisqVal <- -2*(as.numeric(logLik(orModNull)) - as.numeric(logLik(orMod_i)))
    or_lrTestp <- 1 - pchisq(or_chisqVal, df=1)
    
    rr_rd_or <- c(rr_rd_or, as.numeric(c(or_lgkNull, or_lgkMod)), or_chisqVal, 1, or_lrTestp)
    # --------------------------------------------
    
    # Usual (two-sided) odds ratio logistic regression
    # Cells of 2x2 table in this order, when as.vector: TN, FP, FN, TP.
    orci_i <- as.numeric(exp(confint.default(orMod_i)[2,]))
    orMeta <- c(orMeta, as.vector(tbl2x2), or_i, orci_i)
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    nM <- tbl2x2Full[1,3] # Number of males
    nF <- tbl2x2Full[2,3] # Number of females
    rr_rd_or <- c(rr_rd_or, nM, nF)
    
    # print(floor(possibleSamples)-i)
}
stop <- Sys.time()
print(difftime(stop, start))

resDf <- data.frame(matrix(data=rr_rd_or, nrow = floor(possibleSamples), byrow = TRUE))

colnames(resDf) <- c(
    
    "ltmde", "rskM", "rskF",
    
    "rr",
    paste0("rrl", ciXX), paste0("rrl", ciXX, "m"), paste0("rru", ciXX),
    "rrp", "rrzsH0", "rrpsH0",
    "rrllkNull", "rrllkMod", "rrChisq", "rrdf", "rrllkp",
    
    "rd",
    paste0("rdl", ciXX), paste0("rdl", ciXX, "m"), paste0("rdu", ciXX),
    "rdp", "rdzsH0", "rdpsH0",
    "rdllkNull", "rdllkMod", "rdChisq", "rddf", "rdllkp",
    
    "or", paste0("orl", ciXX), paste0("oru", ciXX), "orp", "orzsH0", "orpsH0",
    "orllkNull", "orllkMod", "orChisq", "ordf", "orllkp",
    
    "nMale", "nFemale"
)

rrDf <- data.frame(matrix(data=rrMeta, nrow = floor(possibleSamples), byrow = TRUE))
summary(rrDf$rr)
colnames(rrDf) <- c("TN", "FP", "FN", "TP", "rr", paste0("rrl", ciXX), paste0("rru", ciXX))
rrDf$nE <- apply(rrDf[,c("FP", "TP")], 1, sum)
rrDf$nC <- apply(rrDf[,c("TN", "FN")], 1, sum)

rdDf <- data.frame(matrix(data=rdMeta, nrow = floor(possibleSamples), byrow = TRUE))
summary(rdDf$rd)
colnames(rdDf) <- c("TN", "FP", "FN", "TP", "rd", paste0("rdl", ciXX), paste0("rdu", ciXX))
rdDf$nE <- apply(rdDf[,c("FP", "TP")], 1, sum)
rdDf$nC <- apply(rdDf[,c("TN", "FN")], 1, sum)

orDf <- data.frame(matrix(data=orMeta, nrow = floor(possibleSamples), byrow = TRUE))
summary(orDf$or)
colnames(orDf) <- c("TN", "FP", "FN", "TP", "or", paste0("orl", ciXX), paste0("oru", ciXX))
orDf$nE <- apply(orDf[,c("FP", "TP")], 1, sum)
orDf$nC <- apply(orDf[,c("TN", "FN")], 1, sum)
# -------------------------------------------------

# Collapsibility (this collects only the results from tbl2x2; not the null and not the adjusted tbl2x2, this happens in nsduhRisk0.R)
collapsibilityDf <- data.frame(matrix(data=risk_oddsCollapsed, ncol=4, byrow = TRUE))
colnames(collapsibilityDf) <- c("absRiskTotal", "riskCollapsed",
                                "oddsTotal", "oddsCollapsed")

# -------------------------------------------------
# BEWARE: It takes up a considerable amount of time to compute the multilevel models below. It does not make sense to do that, because the results are in the end very similar. They must be quite similar, because always almost the same total sample size is used, only the size of the single samples differ, according to the sample size computation, based on the a priori effect size, alpha level, and power. Since in multilevel analyses, the results (intercepts and/or slopes) from all single samples are integrated, the overall estimates cannot vary a lot, across our 15 different setups.
# Conclusion: We ran the 15 analyses, the multilevel summary estimates were very similar, we would not do it again, and we therefore recommend to not waste your computer's time and energy.
# -------------------------------------------------
# makeCIlme4 <- function(lme4summaryOutput=NULL, alphaLevel=NULL) {
#     
#     effest <- lme4summaryOutput$coefficients["sex","Estimate"]
#     effSE <- lme4summaryOutput$coefficients["sex","Std. Error"]
#     # Use current alpha level to compute critical z value for two.sided test.
#     # czVal2Sided <- qnorm(p=1-runRiskDf$a[r]/2, lower.tail = TRUE)
#     czVal2Sided <- qnorm(p=1-alphaLevel/2, lower.tail = TRUE)
#     effLci <- effest - effSE * czVal2Sided
#     effUci <- effest + effSE * czVal2Sided
#     return(c(effest, effLci, effUci))
# }
# 
# # Risk difference (random intercept, random slope)
# multModRD <- lme4::glmer(ltmde ~ sex + (sex|smpl), family = binomial(link="identity"), data=riskData1)
# smryModRD <- summary(multModRD)
# 
# # effect estimate with x% compatibility interval
# resRD <- makeCIlme4(lme4summaryOutput = smryModRD, alphaLevel = runRiskDf$a[r])
# 
# # - - - - - - - - - - - - - - - - - - - - - - - -
# 
# # Risk ratio (random intercept, random slope)
# multModRR <- lme4::glmer(ltmde ~ sex + (sex|smpl), family = binomial(link="log"), data=riskData1)
# smryModRR <- summary(multModRR)
# 
# # effect estimate with x% compatibility interval
# resRR <- exp(makeCIlme4(lme4summaryOutput = smryModRR, alphaLevel = runRiskDf$a[r]))
# 
# # - - - - - - - - - - - - - - - - - - - - - - - -
# 
# # Odds ratio (random intercept, random slope)
# multModOR <- lme4::glmer(ltmde ~ sex + (sex|smpl), family = binomial(link="logit"), data=riskData1)
# smryModOR <- summary(multModOR)
# 
# # effect estimate with x% compatibility interval
# resOR <- exp(makeCIlme4(lme4summaryOutput = smryModOR, alphaLevel = runRiskDf$a[r]))
# 
# # - - - - - - - - - - - - - - - - - - - - - - - -
# resLme4 <- data.frame(matrix(data=c(resRD, resRR, resOR), nrow=3, byrow = TRUE))
# colnames(resLme4) <- c("estimate", "lci", "uci")
# rownames(resLme4) <- c("RD", "RR", "OR")