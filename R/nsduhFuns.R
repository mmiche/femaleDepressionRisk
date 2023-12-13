# All custom functions or modified functions from other pepole, which are used to analyze the nsduh data.


# package survivalpwr, functions pwr_coxph, get_n_onesided and get_power
# Use the helper function 'get_n_onesided' from the package pwr_coxph directly, because there is currently a bug in the function pwr_coxph, when setting the argument alternative = "greater".
get_n_onesided <- function (hr, eventprob, rsquare, stddev, sig_level, power) 
{
    ((stats::qnorm(power, lower.tail = TRUE) + stats::qnorm(1 - sig_level))^2)/(eventprob * (1 - rsquare) * stddev^2 * log(hr)^2)
}

# From a given 2x2 table, produce a 2x2 table with perfect independence. That is, multiply the margin sums and divide by total sample size, to fill each of the four cells.
makeNullTbl <- function(tbl2x2=NULL) {
    # tm: table 2x2 marginal sums
    tm <- addmargins(tbl2x2)
    return(
        matrix(data=c(tm[3,1]*tm[1,3]/tm[3,3],
                      tm[3,2]*tm[1,3]/tm[3,3],
                      tm[3,1]*tm[2,3]/tm[3,3],
                      tm[3,2]*tm[2,3]/tm[3,3]),
               nrow=2, byrow = TRUE)
    )
}

# ct: cut
ct <- function(vals=NULL, setBreaks=NULL) {
    cut(vals, breaks = setBreaks, include.lowest = TRUE)
}

# For a given 2x2 table, compute the positive and the negative likelihood ratio.
posnegLR <- function(tbl2x2=NULL) {
    sens <- tbl2x2[2,2]/sum(tbl2x2[,2])
    spec <- tbl2x2[1,1]/sum(tbl2x2[,1])
    return(c(posLR=sens/(1-spec),
             negLR=(1-sens)/spec))
}

# For a given 2x2 table, run the likelihood ratio test. Use the function LR_test_2x2 from the package contingencytables, but modify it, so that any counts in the cells, not just integers, will be accepted.
# Note. There is another custom function (g.test, by Pete Hurd) available on the internet, which also permits running the likelihood ratio test for a 2x2 table, which we would not have to modify for our purposes. We used the g.test function to validate that it produces the same result (it does). However, the g.test raw code is much longer than the below code, because it provides some extra features, like Yates correction.
LR_test_2x2Modified <- function (n) 
{
    # # Modified: Do not check whether cells contain integers.
    # validateArguments(mget(ls()))
    N <- sum(n)
    m <- outer(apply(n, 1, sum), apply(n, 2, sum))/N
    T0 <- 0
    for (i in 1:2) {
        for (j in 1:2) {
            if (n[i, j] > 0) {
                T0 <- T0 + n[i, j] * log(n[i, j]/m[i, j])
            }
        }
    }
    T0 <- 2 * T0
    df <- 1
    P <- 1 - pchisq(T0, df)
    if (is.na(P)) {
        P <- 1
    }
    # # Modified: Function contingencytables_result not needed.
    # return(contingencytables_result(list(p.value = P, statistic = T0,
    #                                      df = df), sprintf("The likelihood ratio test: P = %7.5f, T = %5.3f (df = %i)", 
    #                                                        P, T0, df)))
    # Modified: Return the results numerically (no print out, no formatting).
    return(c(T0, df, P))
}

# For a given 2x2 table, check the collapsiblity problem.
# Collapsiblity problem, see Greenland (https://doi.org/10.1016/j.jclinepi.2021.06.007, https://doi.org/10.1016/j.jclinepi.2021.06.004)
checkCollapsibility <- function(tbl2x2=NULL) {
    
    tbl2x2Full <- addmargins(tbl2x2)
    wt0 <- tbl2x2Full[1,3]/tbl2x2Full[3,3]
    wt1 <- tbl2x2Full[2,3]/tbl2x2Full[3,3]
    proptbl2x2 <- addmargins(prop.table(tbl2x2))
    absRiskTotal <- proptbl2x2[3,2]
    oddsTotal <- proptbl2x2[3,2]/proptbl2x2[3,1]
    # Perspective 2, regarding 2x2 table (compute rowwise, across columns)
    perspective2 <- addmargins(prop.table(tbl2x2, margin=1), margin=2)
    absrisk0 <- perspective2[1,2]
    absrisk1 <- perspective2[2,2]
    
    # Odds
    odds0 <- absrisk0/(1-absrisk0)
    odds1 <- absrisk1/(1-absrisk1)
    # Perspective 1, regarding 2x2 table (compute columnwise, across rows); here not needed.
    # (perspective1 <- addmargins(prop.table(tbl2x2, margin=2), margin=1))
    
    riskCollapsed <- wt0*absrisk0 + wt1*absrisk1
    oddsCollapsed <- wt0*odds0 + wt1*odds1
    
    return(list(tbl2x2Full=tbl2x2Full,
                tbl2x2Perspective2=perspective2,
                proptbl2x2=proptbl2x2,
                mainRes=c(absRiskTotal=absRiskTotal,
                          riskCollapsed=riskCollapsed,
                          oddsTotal=oddsTotal,
                          oddsCollapsed=oddsCollapsed)))
    
}

# Custom function to check collapsiblity with a perfectly independent 2x2 table (see function makeNullTbl).
checkNullCollapse <- function(nullClps=NULL) {
    riskCols <- all(dplyr::near(x=nullClps[,"absRiskTotal"], y=nullClps[,"riskCollapsed"]))
    oddsCols <- all(dplyr::near(x=nullClps[,"oddsTotal"], y=nullClps[,"oddsCollapsed"]))
    if(all(c(riskCols, oddsCols))) {
        cat("Null tbl2x2 collapsed as expected.\n")
    } else {
        cat("Null tbl2x2 did NOT all collapse.\n")
    }
}

# Custom function to check that absolute measures of risk collapsed, as expected.
prepareCollapse <- function(tbl2x2Clps=NULL, tbl2x2AdjClps=NULL) {
    
    check1 <- all(round(tbl2x2Clps[,"riskCollapsed"]/tbl2x2Clps[,"absRiskTotal"], digits = 10)==1)
    check2 <- all(round(tbl2x2AdjClps[,"riskCollapsed"]/tbl2x2AdjClps[,"absRiskTotal"], digits = 10)==1)
    
    if(any(c(check1, check2)==FALSE)) {
        cat("Risks have NOT collapsed, check.\n")
    } else {
        return(data.frame(name=rep(c("tbl2x2", "tbl2x2Adj"), each=nrow(tbl2x2Clps)),
                          clpsExcess=c(
                              tbl2x2Clps[,"oddsCollapsed"]/tbl2x2Clps[,"oddsTotal"],
                              tbl2x2AdjClps[,"oddsCollapsed"]/tbl2x2AdjClps[,"oddsTotal"]
                          ))
        )
    }
    
}

# Convert a statistical test's p-value to an S value.
# See Greenland (2019) https://doi.org/10.1080/00031305.2018.1529625
Sfun <- function(x) {-log(x=x, base = 2)}


# Misclassification bias correction

# a = TP, b = FN, c = FP, d = TN
# ------------------------------
outcomeMiscAdj2x2 <- function(a, b, c, d, se1, se0, sp1, sp0) {
    
    Ep <- sum(a+c)
    Em <- sum(b+d)
    A <- (a - Ep*(1-sp1))/(se1-(1-sp1))
    B <- (b - Em*(1-sp0))/(se0-(1-sp0))
    C <- Ep - A
    D <- Em - B
    tbl <- as.table(matrix(data=c(A, B, C, D), nrow=2, byrow = TRUE))
    dimnames(tbl) <- list(c("outcome+", "outcome-"),
                          c("exposure+", "exposure-"))
    return(tbl)
}

# Yorlets et al., 2023 (https://doi.org/10.1016/j.annepidem.2023.08.001)
# rr_calculations_direct_indirect.pdf, page 5 and page 7:
# seLnRR: function to compute standard error for log of risk ratio, given a 2x2 table.
seLnRR <- function(tbl2x2=NULL) {
    sqrt((1/tbl2x2[1,1])+(1/tbl2x2[1,2])-(1/sum(tbl2x2[,1]))-(1/sum(tbl2x2[,2])))
}

# seRD: function to compute standard error for risk difference, given a 2x2 table.
seRD <- function(tbl2x2=NULL) {
    
    numeratorLeft1 <- tbl2x2[1,1]/sum(tbl2x2[,1])
    numeratorLeft2 <- 1 - numeratorLeft1
    denominatorLeft <- sum(tbl2x2[,1])
    
    numeratorRight1 <- tbl2x2[1,2]/sum(tbl2x2[,2])
    numeratorRight2 <- 1 - numeratorRight1
    denominatorRight <- sum(tbl2x2[,2])
    
    return(sqrt((numeratorLeft1*numeratorLeft2/denominatorLeft)+(numeratorRight1*numeratorRight2/denominatorRight)))
}

# For outcome misclassification bias adjusted 2x2 table, compute:
# risk ratio and 95% compatibility interval, same with risk difference.
adjusted_rrrd <- function(adj2x2=NULL, runRiskDf=NULL, r=NULL) {
    
    # Make table with relative numbers (rows of table = outcome, columns of table = exposure)
    adj2x2freq <- prop.table(adj2x2, margin=2)
    
    # Absolute risk for exposed group, in adjusted 2x2 table
    adjAbsRskExposed <- adj2x2freq[1,1]
    # Absolute risk for non-exposed group, in adjusted 2x2 table
    adjAbsRskNonExposed <- adj2x2freq[1,2]
    
    # two-sided critical z value: Divide one-sided value by 2.
    czVal <- qnorm(runRiskDf$a[r]/2, lower.tail = FALSE)
    
    # Adjusted risk ratio
    # -------------------
    adjRR <- adjAbsRskExposed/adjAbsRskNonExposed
    
    adjRRSE <- seLnRR(tbl2x2=adj2x2)
    adjRRlci <- exp(log(adjRR) - czVal * adjRRSE)
    adjRRuci <- exp(log(adjRR) + czVal * adjRRSE)
    
    # Adjusted risk difference
    # ------------------------
    adjRD <- adjAbsRskExposed-adjAbsRskNonExposed
    adjRDSE <- seRD(tbl2x2=adj2x2)
    adjRDlci <- adjRD - czVal * adjRDSE
    adjRDuci <- adjRD + czVal * adjRDSE
    
    # Return
    return(
        list(adjAbsRskExposed=adjAbsRskExposed, adjAbsRskNonExposed=adjAbsRskNonExposed,
             adjRR=adjRR, adjRRSE=adjRRSE, adjRRlci=adjRRlci, adjRRuci=adjRRuci,
             adjRD=adjRD, adjRDSE=adjRDSE, adjRDlci=adjRDlci, adjRDuci=adjRDuci, z=czVal)
    )
}

