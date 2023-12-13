resDf <- collectRiskResLs[[resDfVec[sel1To15]]]

resDfVec <- names(collectRiskResLs)[seq(1,length(collectRiskResLs),by=4)]
SLs <- list()
# s <- 1
# names(collectRiskResLs[[resDfVec[s]]])
for(s in 1:nrow(runRiskDf)) {
    SVec <- c()
    SVec <- c(SVec,
              # p-values from exact zero and non-exact zero null hypotheses.
              Sfun(collectRiskResLs[[resDfVec[s]]]$rrp),
              Sfun(collectRiskResLs[[resDfVec[s]]]$rrpsH0),
              Sfun(collectRiskResLs[[resDfVec[s]]]$rdp),
              Sfun(collectRiskResLs[[resDfVec[s]]]$rdpsH0),
              Sfun(collectRiskResLs[[resDfVec[s]]]$orp),
              Sfun(collectRiskResLs[[resDfVec[s]]]$orpsH0),
              # p-values from log likelihood model comparisons
              Sfun(collectRiskResLs[[resDfVec[s]]]$rrllkp),
              Sfun(collectRiskResLs[[resDfVec[s]]]$rdllkp),
              Sfun(collectRiskResLs[[resDfVec[s]]]$orllkp)
    )
    SDf <- data.frame(matrix(data=SVec, ncol=9))
    colnames(SDf) <- c("rrp", "rrpsH0", "rdp", "rdpsH0", "orp", "orpsH0", "rrllkp", "rdllkp", "orllkp")
    SLs[[resDfVec[s]]] <- SDf
}

resDfS <- SLs[[resDfVec[sel1To15]]]

sValBoxDf <-
    data.frame(model=rep(c("rrp", "rrpsH0", "rdp", "rdpsH0", "orp", "orpsH0",
                           "rrllkp", "rdllkp", "orllkp"), each=nrow(resDfS)),
               sVals=unlist(resDfS[,c("rrp", "rrpsH0", "rdp", "rdpsH0", "orp",
                                      "orpsH0", "rrllkp", "rdllkp", "orllkp")])
    )

sValBoxDf$model <- factor(sValBoxDf$model,
                          levels = c("rrp", "rrpsH0", "rdp", "rdpsH0",
                                     "orp", "orpsH0",
                                     "rrllkp", "rdllkp", "orllkp"))

(svalBox <- 
    ggplot(data=sValBoxDf, aes(x=model, y=sVals)) +
    geom_boxplot() +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        # axis.ticks.y=element_blank(),
        panel.border = element_rect(color="black", fill=NA)))

# ggsave(filename="sValBox.png", plot = svalBox, path = ggsavePath, device = "png", width=9, height=6, units="in", dpi=300)
