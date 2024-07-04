# This script shows how we generated Tables 2 and 3, and all figures of the main document.

# -----------------------------------------------------------
#                           Table 2
# -----------------------------------------------------------

library(pwrss)
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

# ?pwrss::pwrss.z.logreg

achievedPower <- c()
for(i in 1:nrow(runRiskDf)) {
    achievedPower <- c(achievedPower,
                       pwrss::pwrss.z.logreg(p0 = runRiskDf$p0[i],
                                             p1 = runRiskDf$p1[i],
                                             r2.other.x = 0,
                                             n = runRiskDf$requiredN[i],
                                             alpha = runRiskDf$a[i], 
                                             dist="binomial",
                                             alternative = "not equal",
                                             verbose=FALSE)$power)
}
runRiskDf$achievedPower <- achievedPower

runRiskDfPrint <- runRiskDf[,c("p0", "p1", "a", "requiredN", "power", "achievedPower")]

runRiskDfPrint$nTestsExact <- 399751/runRiskDf$requiredN
runRiskDfPrint$nTest <- floor(runRiskDfPrint$nTestsExact)
runRiskDfPrint
# -----------------------------------------------------------
# -----------------------------------------------------------



# -----------------------------------------------------------
#                           Table 3
# -----------------------------------------------------------
printSeSp <- data.frame(matrix(data=c(
    rep(c(.85, .9, .95), each=8),
    rep(seq(.45, .8, by=.05), times=3),
    rep(.95, 48)),
    ncol=4
))
printSeSp <- printSeSp[,c(2,1,3,4)]
colnames(printSeSp) <- c("Se0", "Se1", "Sp0", "Sp1")
printSeSp
# -----------------------------------------------------------
# -----------------------------------------------------------


# -----------------------------------------------------------
#                           Figures 1, 2, 3, and 4.
# -----------------------------------------------------------
#
# Setups 6 and 10 out of 15.
sel1To15Vec <- c(6, 10)
#
# Use same code for 2-sided tests, to produce the multiplots
collectRiskRes2SidedLs <- readRDS(file=paste0(nsduhPath, "collectRiskRes2SidedLs.rds"))
resDfVec <- names(collectRiskRes2SidedLs)
collectRiskResLs <- collectRiskRes2SidedLs

smallestdiff <- function(singleNum=NULL, vec=NULL) {
    return(which(abs(vec - singleNum) == min(abs(vec - singleNum)))[1])
}

minmaxCI <- function(data=NULL, lci=NULL, uci=NULL, noEffect=1) {
    
    tmp <- data[,c(lci, uci)]
    
    idxProtect <- tmp[,uci] < noEffect
    if(all(!idxProtect)) {
        print(paste("Protect =", length(which(idxProtect))))
    } else {
        print(paste("Protect =", length(which(idxProtect))/nrow(tmp)*100))
    }
    idxNonSign <- !(tmp[,uci] < noEffect) & !(tmp[,lci] > noEffect)
    print(paste("Not sign. =", length(which(idxNonSign))/nrow(tmp)*100))
    idxRisk <- tmp[,lci] > noEffect
    print(paste("Risk =", length(which(idxRisk))/nrow(tmp)*100))
    
    ciSize <- abs(tmp[,uci] - tmp[,lci])
    
    # 1 min, 2 1stQu., 3 Median, 4 Mean, 5 3rdQu., 6 max.
    fivenum <- as.numeric(summary(ciSize))
    
    idx0 <- smallestdiff(singleNum = fivenum[1], vec = ciSize)
    # print(data[idx0,])
    idx25 <- smallestdiff(singleNum = fivenum[2], vec = ciSize)
    # print(data[idx25,])
    idx50 <- smallestdiff(singleNum = fivenum[3], vec = ciSize)
    # print(data[idx50,])
    idx75 <- smallestdiff(singleNum = fivenum[5], vec = ciSize)
    # print(data[idx75,])
    idx100 <- smallestdiff(singleNum = fivenum[6], vec = ciSize)
    # print(data[idx100,])
    
    out <-
        rbind(data[idx0,],
              data[idx25,],
              data[idx50,],
              data[idx75,],
              data[idx100,])
    
    return(out)
}

preparePlotTable <- function(tableDf=NULL, colNames=NULL) {
    colnames(tableDf) <- colNames
    tableDfNew <- round(tableDf, digits = 2)
    tableDfNew$Summary <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    return(tableDfNew[,c(4,1:3)])
}

ggplotLs <- list()
# ------------------------------------------
# Forest plot
# S E L E C T  one of these two measures: "rr", "rd"
selRiskMeas <- "rr"
# ------------------------------------------
# sel1To15 <- 1 # Set sel1To15 to a value between 1 and 15, in order to run a single regression setup (setup = rows of runRiskDf). If you want to run a single setup, you will have to mark the code inside the for-loop, then execute it.
for(sel1To15 in sel1To15Vec) {
    
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
    # 
    source(paste0(nsduhPath, "nsduhRRRDORForestPlot.R"), echo = FALSE)
    # Display forest plot in R console
    ggplotLs[[paste0("plot", sel1To15)]] <- plotRisk
}

# Set the single plots free to the global environment:
list2env(ggplotLs , envir = .GlobalEnv)

names(ggplotLs) # "plot6"  "plot10"

# Panel A of Figure 1: 6 (of 15) and scenario 6 (of 24)
rr6Df <-
    minmaxCI(data=collectRiskRes2SidedLs[[resDfVec[6]]][,c("rr", "rrl99", "rru99")],
             lci="rrl99", uci="rru99", noEffect = 1)
rr6DfNew <- preparePlotTable(tableDf = rr6Df, colNames = c("RR", "l99%CI", "u99%CI"))

plot6Test <- plot6 +
    scale_x_continuous(trans = "log2",
                       breaks = c(.65, 1, 1.5, 2, 5, 8)) +
    coord_cartesian(xlim=c(.65, 8)) +
    theme(legend.position = "none")

plot6Test <- plot6Test +
    annotate(geom = "table",
             x=8,
             y=400,
             label = list(rr6DfNew),
             size=4)

# Panel A of Figures 1 and 3 only differs in terms of the x-axis range.

# 10 (of 15) and scenario 6 (of 24)
rr10Df <-
    minmaxCI(data=collectRiskRes2SidedLs[[resDfVec[10]]][,c("rr", "rrl99", "rru99")],
             lci="rrl99", uci="rru99", noEffect = 1)
rr10DfNew <- preparePlotTable(tableDf = rr10Df, colNames = c("RR", "l99%CI", "u99%CI"))

plot10Test <- plot10 +
    scale_x_continuous(trans = "log2",
                       breaks = c(.8, 1, 1.5, 2, 4)) +
    coord_cartesian(xlim=c(.8, 4)) +
    theme(legend.position = "none")

plot10Test <- plot10Test +
    annotate(geom = "table",
             x=4.25,
             y=190,
             label = list(rr10DfNew),
             size=4)

# Panel A of Figures 2 and 4 only differs in terms of the x-axis range.

# Panels B of Figures 1-4.
collectLs <- readRDS(file=paste0(nsduhPath, "qbaDetailedRes.rds"))
length(collectLs)
# Which scenario out of 24 QBA scenarios?
whichLs <- 6 # -> Figures 1 and 2
# whichLs <- 1 # -> Figures 3 and 4
adjLs <- collectLs[[whichLs]]
resDfVec <- names(adjLs)

ggplotRRAdjLs <- list()
# sel1To15 <- 1 # Set sel1To15 to a value between 1 and 15, in order to run a single regression setup (setup = rows of runRiskDf). If you want to run a single setup, you will have to mark the code inside the for-loop, then execute it.
for(sel1To15 in sel1To15Vec) {
    
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
    
    # Adjustment = Empirical adjustment of outcome misclassification bias.
    # ------------------
    source(paste0(nsduhPath, "nsduhRRAdjForestPlot.R"), echo = FALSE)
    # # Display in R console
    # plotRisk
    
    # Display forest plot in R console
    ggplotRRAdjLs[[paste0("rrAdjPlot", sel1To15)]] <- plotRisk
    
}

# Release the list with the plots to the global environment:
list2env(ggplotRRAdjLs , envir = .GlobalEnv)

names(ggplotRRAdjLs) # "rrAdjPlot6"  "rrAdjPlot10"

#                                                                          ----------------------
# 6 (of 15) and scenario 6 (of 24) -> Run script line 227-279, especially: run line 231, NOT 232!
#                                                                          ----------------------
adjrr6Df <-
    minmaxCI(data=adjLs[[resDfVec[6]]][,c("adjrr", "adjrrLci", "adjrrUci")],
             lci="adjrrLci", uci="adjrrUci", noEffect = 1)
adjrr6DfNew <- preparePlotTable(tableDf = adjrr6Df, colNames = c("RR", "l99%CI", "u99%CI"))

rrAdjPlot6Test <- rrAdjPlot6 +
    scale_x_continuous(trans = "log2",
                       breaks = c(.65, 1, 1.5, 2, 5, 8)) +
    coord_cartesian(xlim=c(.65, 8)) +
    theme(legend.position = "none")

rrAdjPlot6Test <- rrAdjPlot6Test +
    annotate(geom = "table",
             x=8,
             y=400,
             label = list(adjrr6DfNew),
             size=4)

#                                                                          ----------------------
# 6 (of 15) and scenario 1 (of 24) -> Run script line 227-279, especially: run line 232, NOT 231!
#                                                                          ----------------------
adjrr6Df <-
    minmaxCI(data=adjLs[[resDfVec[6]]][,c("adjrr", "adjrrLci", "adjrrUci")],
             lci="adjrrLci", uci="adjrrUci", noEffect = 1)
adjrr6DfNew <- preparePlotTable(tableDf = adjrr6Df, colNames = c("RR", "l99%CI", "u99%CI"))

rrAdjPlot6Test <- rrAdjPlot6 +
    scale_x_continuous(trans = "log2",
                       breaks = c(.4, 1, 1.5, 2, 5, 8)) +
    coord_cartesian(xlim=c(.4, 8)) +
    theme(legend.position = "none")

rrAdjPlot6Test <- rrAdjPlot6Test +
    annotate(geom = "table",
             x=8,
             y=400,
             label = list(adjrr6DfNew),
             size=4)

#                                                                           ----------------------
# 10 (of 15) and scenario 6 (of 24) -> Run script line 227-279, especially: run line 231, NOT 232!
#                                                                           ----------------------
adjrr10Df <-
    minmaxCI(data=adjLs[[resDfVec[10]]][,c("adjrr", "adjrrLci", "adjrrUci")],
             lci="adjrrLci", uci="adjrrUci", noEffect = 1)
adjrr10DfNew <- preparePlotTable(tableDf = adjrr10Df, colNames = c("RR", "l99%CI", "u99%CI"))

rrAdjPlot10Test <- rrAdjPlot10 +
    scale_x_continuous(trans = "log2",
                       breaks = c(.8, 1, 1.5, 2, 4)) +
    coord_cartesian(xlim=c(.8, 4)) +
    theme(legend.position = "none")

rrAdjPlot10Test <- rrAdjPlot10Test +
    annotate(geom = "table",
             x=4.25,
             y=190,
             label = list(adjrr10DfNew),
             size=4)

#                                                                           ----------------------
# 10 (of 15) and scenario 1 (of 24) -> Run script line 227-279, especially: run line 232, NOT 231!
#                                                                           ----------------------
adjrr10Df <-
    minmaxCI(data=adjLs[[resDfVec[10]]][,c("adjrr", "adjrrLci", "adjrrUci")],
             lci="adjrrLci", uci="adjrrUci", noEffect = 1)
adjrr10DfNew <- preparePlotTable(tableDf = adjrr10Df, colNames = c("RR", "l99%CI", "u99%CI"))

rrAdjPlot10Test <- rrAdjPlot10 +
    scale_x_continuous(trans = "log2",
                       breaks = c(.5, 1, 1.5, 2, 4)) +
    coord_cartesian(xlim=c(.5, 4)) +
    theme(legend.position = "none")

rrAdjPlot10Test <- rrAdjPlot10Test +
    annotate(geom = "table",
             x=4.25,
             y=190,
             label = list(adjrr10DfNew),
             size=4)

# Figures 1 and 3:
plotRawAdj6 <- cowplot::plot_grid(plot6Test, rrAdjPlot6Test, ncol=2, labels = c("A", "B"))

# Figures 2 and 4:
plotRawAdj10 <- cowplot::plot_grid(plot10Test, rrAdjPlot10Test, ncol=2, labels = c("A", "B"))
# -----------------------------------------------------------
# -----------------------------------------------------------


# -----------------------------------------------------------
#                           Figure 5
# -----------------------------------------------------------

collectRiskResLs <- readRDS(file=paste0(nsduhPath, "collectRiskResLs.rds"))
resDfVec <- names(collectRiskResLs)[seq(1,length(collectRiskResLs),by=4)]

# rrMeta15 = Table that is visualized in Plot A of Figure 8 in MainDocument.
# -------------------------------------------------------------------------
rrDfVec <- names(collectRiskResLs)[seq(2,length(collectRiskResLs),by=4)]
rrMetaLs <- list()
for(i in 1:15) {
    
    rrMeta <- collectRiskResLs[[rrDfVec[i]]]
    
    if(any((1:5) %in% i)) {
        meta.rr <- meta::metagen(TE=log(rr), lower = log(rrl95), upper = log(rru95), sm="RR", prediction = TRUE, data = rrMeta)
    } else if(any((6:10) %in% i)) {
        meta.rr <- meta::metagen(TE=log(rr), lower = log(rrl99), upper = log(rru99), sm="RR", prediction = TRUE, data = rrMeta)
    } else if(any((11:15) %in% i)) {
        meta.rr <- meta::metagen(TE=log(rr), lower = log(rrl995), upper = log(rru995), sm="RR", prediction = TRUE, data = rrMeta)
    }
    
    rrMeta_i <- data.frame(
        matrix(exp(c(meta.rr$TE.random,
                     meta.rr$lower.predict,
                     meta.rr$upper.predict)), ncol=3))
    colnames(rrMeta_i) <- c("estimate", "lPI", "uPI")
    rrMetaLs[[rrDfVec[i]]] <- rrMeta_i
    
}
rrMeta15 <- dplyr::bind_rows(rrMetaLs)

# rdMeta15 = Table that is visualized in Plot B of Figure 8 in MainDocument.
# -------------------------------------------------------------------------
rdDfVec <- names(collectRiskResLs)[seq(3,length(collectRiskResLs),by=4)]
rdMetaLs <- list()
for(i in 1:15) {
    
    rdMeta <- collectRiskResLs[[rdDfVec[i]]]
    
    if(any((1:5) %in% i)) {
        meta.rd <- meta::metagen(TE=rd, lower = rdl95, upper = rdu95, sm="RD", prediction = TRUE, data = rdMeta)
    } else if(any((6:10) %in% i)) {
        meta.rd <- meta::metagen(TE=rd, lower = rdl99, upper = rdu99, sm="RD", prediction = TRUE, data = rdMeta)
    } else if(any((11:15) %in% i)) {
        meta.rd <- meta::metagen(TE=rd, lower = rdl995, upper = rdu995, sm="RD", prediction = TRUE, data = rdMeta)
    }
    
    rdMeta_i <- data.frame(
        matrix(c(meta.rd$TE.random,
                meta.rd$lower.predict,
                meta.rd$upper.predict), ncol=3))
    colnames(rdMeta_i) <- c("estimate", "lPI", "uPI")
    rdMetaLs[[rdDfVec[i]]] <- rdMeta_i
    
}
rdMeta15 <- dplyr::bind_rows(rdMetaLs)

# saveRDS(object=rrMeta15, file=paste0(nsduhPath, "rrMeta.rds"))
# saveRDS(object=rdMeta15, file=paste0(nsduhPath, "rdMeta.rds"))

# Make Figure 8:

rrVec <- rdVec <- run <- c()
# i <- 1
for(i in 1:15) {
    rrVec <- c(rrVec, collectRiskResLs[[resDfVec[i]]]$rr)
    rdVec <- c(rdVec, collectRiskResLs[[resDfVec[i]]]$rd)
    run <- c(run, rep(i, times=nrow(collectRiskResLs[[resDfVec[i]]])))
}
rrrdDf <- data.frame(run=forcats::as_factor(run), rr=rrVec, rd=rdVec)
dim(rrrdDf)

plotRR <- 
    ggplot(data=rrrdDf, aes(x=run, y=rr)) +
    geom_boxplot() +
    # ylim(0,2) +
    ylab(label="Risk ratio (RR)") +
    xlab(label="Effect size, power, alpha level, sample size: 15 setups") +
    scale_x_discrete(breaks = c(1, 5, 8, 11, 15), labels=c("1", "5", "8", "11", "15")) +
    # geom_hline(yintercept=2, colour="red", linetype=2) +
    geom_hline(yintercept=1, colour="red", linetype="dashed", linewidth=1) +
    geom_hline(yintercept=1.2, colour="magenta", linetype="dashed", linewidth=1) +
    geom_hline(yintercept=2, colour="black", linetype="dashed", linewidth=1) +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size = 14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        # strip.text.x = element_text(size = 14),
        # panel.grid.major.y = element_line(color="black", linewidth = .15, linetype = 2),
        panel.border = element_rect(color="black", fill=NA))

plotRD <- 
    ggplot(data=rrrdDf, aes(x=run, y=rd)) +
    geom_boxplot() +
    # ylim(-.1,.1) +
    ylab(label="Risk difference (RD)") +
    xlab(label="Effect size, power, alpha level, sample size: 15 setups") +
    scale_x_discrete(breaks = c(1, 5, 8, 11, 15), labels=c("1", "5", "8", "11", "15")) +
    # geom_hline(yintercept=2, colour="red", linetype=2) +
    geom_hline(yintercept=0, colour="red", linetype="dashed", linewidth=1) +
    geom_hline(yintercept=.02, colour="magenta", linetype="dashed", linewidth=1) +
    geom_hline(yintercept=.1, colour="black", linetype="dashed", linewidth=1) +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size = 14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        # strip.text.x = element_text(size = 14),
        # panel.grid.major.y = element_line(color="black", linewidth = .15, linetype = 2),
        panel.border = element_rect(color="black", fill=NA))

plotBoxrrrd <- cowplot::plot_grid(plotRR, plotRD, ncol=2, labels = c("A", "B"))
# -----------------------------------------------------------
# -----------------------------------------------------------

# -----------------------------------------------------------
#                           Figures 6 and 7
# -----------------------------------------------------------

rrDfVec <- names(collectRiskResLs)[seq(2,length(collectRiskResLs),by=4)]

# Outcome misclassification bias adjustment, selected setting.
biasSet <- list(run1.1=c(.85, .45, .95, .95),
                run1.2=c(.85, .5, .95, .95),
                run1.3=c(.85, .55, .95, .95),
                run1.4=c(.85, .6, .95, .95),
                run1.5=c(.85, .65, .95, .95),
                run1.6=c(.85, .7, .95, .95),
                run1.7=c(.85, .75, .95, .95),
                run1.8=c(.85, .8, .95, .95),
                run2.1=c(.9, .45, .95, .95),
                run2.2=c(.9, .5, .95, .95),
                run2.3=c(.9, .55, .95, .95),
                run2.4=c(.9, .6, .95, .95),
                run2.5=c(.9, .65, .95, .95),
                run2.6=c(.9, .7, .95, .95),
                run2.7=c(.9, .75, .95, .95),
                run2.8=c(.9, .8, .95, .95),
                run3.1=c(.95, .45, .95, .95),
                run3.2=c(.95, .5, .95, .95),
                run3.3=c(.95, .55, .95, .95),
                run3.4=c(.95, .6, .95, .95),
                run3.5=c(.95, .65, .95, .95),
                run3.6=c(.95, .7, .95, .95),
                run3.7=c(.95, .75, .95, .95),
                run3.8=c(.95, .8, .95, .95))

collectLs <- list()
# l <- 6
for(l in 1:length(biasSet)) {
    
    # Assign sensitivities and specificities from the bias setup list.
    se1Set <- biasSet[[l]][1]
    se0Set <- biasSet[[l]][2]
    sp1Set <- biasSet[[l]][3]
    sp0Set <- biasSet[[l]][4]
    
    # Collect results:
    adjLs <- list()
    # r <- 1
    for(r in 1:nrow(runRiskDf)) {
        
        #
        df2x2 <- collectRiskResLs[[rrDfVec[r]]]
        # head(df2x2)
        
        # Model based versus sandwich standard errors in logistic regression
        # https://grodri.github.io/glms/r/robust
        tbl2x2Tmp <- as.table(matrix(as.numeric(df2x2[1,4:1]), nrow=2))
        # epiR::epi.2by2(tbl2x2Tmp)
        
        # 
        getResults <- c()
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
                rrrdRes$z, rrrdRes$adjRRp, rrrdRes$adjRDp)))
        }
        # 
        getResultsDf <- data.frame(matrix(data=getResults, nrow = nrow(df2x2), byrow = TRUE))
        colnames(getResultsDf) <- c(
            "adjrr", "adjrrSE", "adjrrLci", "adjrrUci",
            "adjrd", "adjrdSE", "adjrdLci", "adjrdUci",
            "zval", "adjrrp", "adjrdp")
        adjLs[[resDfVec[r]]] <- data.frame(getResultsDf)
        
    }
    # Collect the collected results
    collectLs[[l]] <- adjLs
    
}

# saveRDS(object = collectLs, file = paste0(nsduhPath, "qbaDetailedRes.rds"))

collectLs <- readRDS(file=paste0(nsduhPath, "qbaDetailedRes.rds"))

length(collectLs)

smryAdjrr <- lapply(collectLs, function(x) {
    lapply(x, function(y) summary(y[,"adjrr"]))
})

lapply(smryAdjrr, function(x) {
    dplyr::bind_rows(x)
})

smryAdjrd <- lapply(collectLs, function(x) {
    lapply(x, function(y) summary(y[,"adjrd"]))
})

lapply(smryAdjrd, function(x) {
    dplyr::bind_rows(x)
})

# Boxplots

length(collectLs[[1]])

# i <- j <- 1; colName <- "adjrr"
extractAdj <- function(collectLs=NULL, colName="adjrr") {
    adjRes <- biasModel <- c()
    for(i in 1:length(collectLs)) {
        run <- c()
        for(j in 1:length(collectLs[[i]])) {
            adjRes <- c(adjRes, collectLs[[i]][[j]][,colName])
            run <- c(run, rep(j, times=nrow(collectLs[[i]][[j]])))
        }
        biasModel <- c(biasModel, rep(i, times=length(run)))
    }
    return(data.frame(biasModel, run=forcats::as_factor(run), adjRes))
}

adjrrBox0 <- extractAdj(collectLs = collectLs)
dim(adjrrBox0)
head(adjrrBox0)

idxOn <- adjrrBox0$biasModel %in% 1:8 # 9:16
adjrrBox <- adjrrBox0[idxOn,]
dim(adjrrBox)

plotAdjrr <- 
    ggplot(data=adjrrBox, aes(x=run, y=adjRes)) +
    geom_boxplot() +
    ylim(0,2) +
    ylab(label="Misclassification bias adjusted RR") +
    xlab(label="Effect size, power, alpha level, sample size: 15 setups") +
    # scale_x_discrete(breaks = c(1,8,15), labels=c("1", "8", "15")) +
    scale_x_discrete(breaks = c(1,5,8,11,15), labels=c("1", "5", "8", "11", "15")) +
    # geom_hline(yintercept=2, colour="red", linetype=2) +
    geom_hline(yintercept=1, colour="red", linetype="dashed", linewidth=1) +
    geom_hline(yintercept=1.2, colour="magenta", linetype="dashed", linewidth=1) +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size = 14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        strip.text.x = element_text(size = 14),
        # panel.grid.major.y = element_line(color="black", linewidth = .15, linetype = 2),
        panel.border = element_rect(color="black", fill=NA)) +
    facet_wrap(~biasModel, ncol = 4)


adjrdBox0 <- extractAdj(collectLs = collectLs, colName = "adjrd")

idxOn <- adjrdBox0$biasModel %in% 1:8 # 9:16
adjrdBox <- adjrdBox0[idxOn,]

plotAdjrd <- 
    ggplot(data=adjrdBox, aes(x=run, y=adjRes)) +
    geom_boxplot() +
    ylim(-.1,.1) +
    ylab(label="Misclassification bias adjusted RD") +
    xlab(label="Effect size, power, alpha level, sample size: 15 setups") +
    # scale_x_discrete(breaks = c(1,8,15), labels=c("1", "8", "15")) +
    scale_x_discrete(breaks = c(1,5,8,11,15), labels=c("1", "5", "8", "11", "15")) +
    # geom_hline(yintercept=2, colour="red", linetype=2) +
    geom_hline(yintercept=0, colour="red", linetype="dashed", linewidth=1) +
    geom_hline(yintercept=.02, colour="magenta", linetype="dashed", linewidth=1) +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size = 14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        strip.text.x = element_text(size = 14),
        # panel.grid.major.y = element_line(color="black", linewidth = .15, linetype = 2),
        panel.border = element_rect(color="black", fill=NA)) +
    facet_wrap(~biasModel, ncol = 4)
# -----------------------------------------------------------
# -----------------------------------------------------------

# -----------------------------------------------------------
#                           Figure 8
# -----------------------------------------------------------

runRiskDf <- data.frame(
    p0 = rep(c(.12, .11, .1, .11, .12), times=3),
    p1 = rep(c(.24, .22, .2, .2, .2), times=3),
    a=c(rep(.05, times=5), rep(.01, times=5), rep(.005, times=5))
)

# ----------------------------------
# Complete copy paste from nsduhResults.R (lines 416-468)
# ----------------------------------------
# 8. Meta-Analysis of conventional two-sided logistic regressions.
# ----------------------------------------

nonadjRRLs <- nonadjRDLs <- list()
for(sel1To15 in 1:15) {
    
    # Risk Ratio (rr)
    # ---------------
    rrMeta <- readRDS(paste0(nsduhPath, "collectRiskResLs.rds"))[[paste0("Result", sel1To15,"rrDf")]]
    # # Display input data in R console
    # head(rrMeta)
    
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
    # # Display results as risk ratios in the R console:
    # exp(metaResRR)
    nonadjRRLs[[sel1To15]] <- data.frame(rr=metaResRR[1],
                                         lwrr=metaResRR[4],
                                         uprr=metaResRR[5])
    
    # -----------------------------------------
    
    # Risk Difference (rd)
    # --------------------
    rdMeta <- readRDS(paste0(nsduhPath, "collectRiskResLs.rds"))[[paste0("Result", sel1To15,"rdDf")]]
    # # Display input data in R console
    # head(rdMeta)
    
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
    # # Display results (risk differences) in the R console:
    # metaResRD
    nonadjRDLs[[sel1To15]] <- data.frame(rd=metaResRD[1],
                                         lwrd=metaResRD[4],
                                         uprd=metaResRD[5])
}
# ----------------------------------

nonadjRRDf <- exp(dplyr::bind_rows(nonadjRRLs))
nonadjRRDf$run <- forcats::as_factor(1:15)

nonadjRDDf <- dplyr::bind_rows(nonadjRDLs)
nonadjRDDf$run <- forcats::as_factor(1:15)


plotRRMetaForest <- 
    ggplot(nonadjRRDf, aes(x=rr, y=run)) +
    geom_point(shape=124, size=5) +
    geom_errorbar(width=.55, aes(xmin=lwrr, xmax=uprr), linewidth=1) +
    geom_vline(xintercept = 1, linetype = "dashed", color="red", linewidth=1) +
    geom_vline(xintercept = 1.2, linetype = "dashed", color="magenta", linewidth=1) +
    # scale_x_continuous(trans = "log2") +
    scale_x_continuous(trans='log2',
                       breaks = c(1, 1.25, 1.5, 1.75)) +
    xlab(label="Risk ratio") +
    ylab(label="Effect size, power, alpha level, sample size: 15 setups") +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        panel.border = element_rect(color="black", fill=NA))

plotRDMetaForest <- 
    ggplot(nonadjRDDf, aes(x=rd, y=run)) +
    geom_point(shape=124, size=5) +
    geom_errorbar(width=.55, aes(xmin=lwrd, xmax=uprd), linewidth=1) +
    geom_vline(xintercept = 0, linetype = "dashed", color="red", linewidth=1) +
    geom_vline(xintercept = 0.02, linetype = "dashed", color="magenta", linewidth=1) +
    xlab(label="Risk difference") +
    ylab(label="Effect size, power, alpha level, sample size: 15 setups") +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        panel.border = element_rect(color="black", fill=NA))

plotMetarrrd <- cowplot::plot_grid(plotRRMetaForest, plotRDMetaForest, ncol=2, labels = c("A", "B"))
# -----------------------------------------------------------
# -----------------------------------------------------------

# -----------------------------------------------------------
#                           Figures 9 and 10
# -----------------------------------------------------------
# Run through the collected results, by applying a meta-analysis of the bias adjusted results.

sel1To15Tmp <- 1:15

rradj1Vec <- rdadj1Vec <- c()

# showRRMetaLs: list which will contain the meta-analytic adjusted risk ratio results.
showRRAdjMetaLs <- list()
# showRDMetaLs: list which will contain the meta-analytic adjusted risk difference results.
showRDAdjMetaLs <- list()
for(l in 1:length(collectLs)) {
    
    adjLs <- collectLs[[l]]
    rradjLs <- list()
    rdadjLs <- list()
    
    for(sel1To15 in sel1To15Tmp) {
        
        rradj1 <- length(which(adjLs[[resDfVec[sel1To15]]]$adjrr == 1))
        rradj1Vec <- c(rradj1Vec, rradj1)
        rdadj1 <- length(which(adjLs[[resDfVec[sel1To15]]]$adjrd == 1))
        rdadj1Vec <- c(rdadj1Vec, rdadj1)
        
        if(rradj1 > 0) {
            adjLs[[resDfVec[sel1To15]]] <- adjLs[[resDfVec[sel1To15]]][adjLs[[resDfVec[sel1To15]]]$adjrr != 1,]
        }
        
        # Risk Ratio (rr)
        # ---------------
        (meta.rrAdj <- meta::metagen(TE=log(adjrr), lower = log(adjrrLci), upper = log(adjrrUci), sm="RR", prediction = TRUE, data = adjLs[[resDfVec[sel1To15]]], level.ci = 1-runRiskDf$a[sel1To15]))
        
        # Extract five results: The meta effect estimate, the lower bound of the compatibility interval (from the random effects model and the prediction interval), the upper bound of the compatibility interval (same specification as with the lower bound).
        metaResRRadj <- c(meta.rrAdj$TE.random,
                          meta.rrAdj$lower.random, meta.rrAdj$upper.random,
                          meta.rrAdj$lower.predict, meta.rrAdj$upper.predict)
        
        # Make data.frame for plotting:
        adjrrDf <- data.frame(model="adjRRMeta",
                              estimate=exp(metaResRRadj)[1],
                              lPI=exp(metaResRRadj)[4],
                              uPI=exp(metaResRRadj)[5])
        
        rradjLs[[sel1To15]] <- adjrrDf
        
        # Risk Difference (rd)
        # --------------------
        (meta.rdAdj <- meta::metagen(TE=adjrd, lower = adjrdLci, upper = adjrdUci, sm="RD", prediction = TRUE, data = adjLs[[resDfVec[sel1To15]]], level.ci = 1-runRiskDf$a[sel1To15]))
        
        # Extract five results: The meta effect estimate, the lower bound of the compatibility interval (from the random effects model and the prediction interval), the upper bound of the compatibility interval (same specification as with the lower bound).
        metaResRDadj <- c(meta.rdAdj$TE.random,
                          meta.rdAdj$lower.random, meta.rdAdj$upper.random,
                          meta.rdAdj$lower.predict, meta.rdAdj$upper.predict)
        
        # Make data.frame for plotting:
        adjrdDf <- data.frame(model="adjRDMeta",
                              estimate=metaResRDadj[1],
                              lPI=metaResRDadj[4],
                              uPI=metaResRDadj[5])
        
        rdadjLs[[sel1To15]] <- adjrdDf
        
    }
    
    showRRAdjMetaLs[[l]] <- dplyr::bind_rows(rradjLs)
    showRDAdjMetaLs[[l]] <- dplyr::bind_rows(rdadjLs)
    
}

# saveRDS(object=showRRAdjMetaLs, file=paste0(nsduhPath, "rrAdjMeta.rds"))
# saveRDS(object=showRDAdjMetaLs, file=paste0(nsduhPath, "rdAdjMeta.rds"))

showRRAdjMetaLs <- readRDS(file=paste0(nsduhPath, "rrAdjMeta.rds"))
showRDAdjMetaLs <- readRDS(file=paste0(nsduhPath, "rdAdjMeta.rds"))

adjrrMetaDf0 <- dplyr::bind_rows(showRRAdjMetaLs)
adjrrMetaDf0$model <- forcats::as_factor(rep(1:15, times=24))
adjrrMetaDf0$biasModel <- rep(1:24, each=15)

idxOn <- adjrrMetaDf0$biasModel %in% 1:8 # 9:16
adjrrMetaDf <- adjrrMetaDf0[idxOn,]

plotRRAdjForest <- 
    ggplot(adjrrMetaDf, aes(x=estimate, y=model)) +
    geom_point(shape=124, size=5) +
    geom_errorbar(width=.55, aes(xmin=lPI, xmax=uPI), linewidth=1) +
    geom_vline(xintercept = 1, linetype = "dashed", color="red", linewidth=1) +
    geom_vline(xintercept = 1.2, linetype = "dashed", color="magenta", linewidth=1) +
    scale_x_continuous(trans = "log2",
                       breaks = c(.7, 1, 1.5, 2, 2.5)) +
    xlab(label="Misclassification bias adjusted RR") +
    ylab(label="Effect size, power, alpha level, sample size: 15 setups") +
    scale_y_discrete(breaks = c(1, 5, 8, 11, 15), labels=c("1", "5", "8", "11", "15")) +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        strip.text.x = element_text(size = 14),
        panel.border = element_rect(color="black", fill=NA)) +
    facet_wrap(~biasModel, ncol=4)



adjrdMetaDf0 <- dplyr::bind_rows(showRDAdjMetaLs)
adjrdMetaDf0$model <- forcats::as_factor(rep(1:15, times=24))
adjrdMetaDf0$biasModel <- rep(1:24, each=15)

idxOn <- adjrdMetaDf0$biasModel %in% 1:8 # 9:16
adjrdMetaDf <- adjrdMetaDf0[idxOn,]

plotRDAdjForest <- 
    ggplot(adjrdMetaDf, aes(x=estimate, y=model)) +
    geom_point(shape=124, size=5) +
    geom_errorbar(width=.55, aes(xmin=lPI, xmax=uPI), linewidth=1) +
    geom_vline(xintercept = 0, linetype = "dashed", color="red", linewidth=1) +
    geom_vline(xintercept = 0.02, linetype = "dashed", color="magenta", linewidth=1) +
    xlab(label="Misclassification bias adjusted RD") +
    ylab(label="Effect size, power, alpha level, sample size: 15 setups") +
    scale_y_discrete(breaks = c(1, 5, 8, 11, 15), labels=c("1", "5", "8", "11", "15")) +
    scale_x_continuous(breaks = c(-.1, -.05, 0, .05, .1)) +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        strip.text.x = element_text(size = 14),
        panel.border = element_rect(color="black", fill=NA)) +
    facet_wrap(~biasModel, ncol=4)
# -----------------------------------------------------------
# -----------------------------------------------------------