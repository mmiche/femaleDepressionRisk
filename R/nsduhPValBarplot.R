if(grepl("rr", specificP)) {
    xlabel <- paste0("Risk ratio - Empirical p-values, separated by ", runRiskDf[sel1To15,"a"])
} else if(grepl("rd", specificP)) {
    xlabel <- paste0("Risk difference - Empirical p-values, separated by ", runRiskDf[sel1To15,"a"])
} else if(grepl("or", specificP)) {
    xlabel <- paste0("Odds Ratio - Empirical p-values, separated by ", runRiskDf[sel1To15,"a"])
} else {
    stop("Select one of these 3: 'rrp', 'rdp', 'orp'.")
}

pvalsVec <- ct(vals = resDf[,specificP], setBreaks = seq(0,1, by=runRiskDf[sel1To15,"a"]))
prop.table(table(pvalsVec))

pvalsDf0 <- data.frame(pCats=forcats::as_factor(1:nlevels(pvalsVec)),
                       pFreq=as.numeric(prop.table(table(pvalsVec))),
                       pVals=seq(.01,1, by=runRiskDf[sel1To15,"a"]))

pvalsDf <- pvalsDf0[pvalsDf0$pFreq>0,]
pvalsDf$pVals <- forcats::as_factor(x=pvalsDf$pVals)

pvalsDf$prozent <- round(pvalsDf$pFreq*100, digits = 1)
# Formatiert
pvalsDf$prozentFormatiert <- paste0(pvalsDf$prozent, "%")
pvalsDf$prozentFormatiert[pvalsDf$prozent<1] <- NA

pvalBar <- 
ggplot(data=pvalsDf, aes(x=pVals, y=pFreq)) + geom_bar(stat="identity") +
    xlab(label=xlabel) +
    ylab(label="Relative frequency") +
    ylim(c(0,1)) +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=16),
        axis.title.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title.y = element_text(size=16),
        panel.border = element_rect(color="black", fill=NA)) +
    # geom_text(aes(label=n), vjust=-0.3, size=6) +
    geom_text(aes(label=prozentFormatiert), vjust=-.3, size=4) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
