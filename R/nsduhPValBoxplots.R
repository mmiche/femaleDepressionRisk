resDfVec <- names(collectRiskResLs)[seq(1,length(collectRiskResLs),by=4)]
resDf <- collectRiskResLs[[resDfVec[sel1To15]]]

# Current alpha significance level is:
runRiskDf[sel1To15,"a"]
length(which(resDf$rrllkp >= runRiskDf[sel1To15,"a"]))/nrow(resDf)
length(which(resDf$rdllkp >= runRiskDf[sel1To15,"a"]))/nrow(resDf)
length(which(resDf$orllkp >= runRiskDf[sel1To15,"a"]))/nrow(resDf)

pValBoxDf <-
    data.frame(model=rep(c("rrp", "rrpsH0", "rdp", "rdpsH0", "orp", "orpsH0",
                           "rrllkp", "rdllkp", "orllkp"), each=nrow(resDf)),
               pVals=unlist(resDf[,c("rrp", "rrpsH0", "rdp", "rdpsH0", "orp",
                                     "orpsH0", "rrllkp", "rdllkp", "orllkp")])
    )
pValBoxDf$model <- factor(pValBoxDf$model,
                          levels = c("rrp", "rrpsH0", "rdp", "rdpsH0",
                                     "orp", "orpsH0",
                                     "rrllkp", "rdllkp", "orllkp"))

pvalBox <-
    ggplot(data=pValBoxDf, aes(x=model, y=pVals)) +
    geom_boxplot() +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        # axis.ticks.y=element_blank(),
        panel.border = element_rect(color="black", fill=NA))

# ggsave(filename="pValBox.png", plot = pvalBox, path = ggsavePath, device = "png", width=9, height=6, units="in", dpi=300)