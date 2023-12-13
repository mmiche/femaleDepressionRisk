resDf <- collectRiskResLs[[resDfVec[sel1To15]]]

Svals <- Sfun(resDf[,specificP])
Smax <- ceiling(max(Svals))

svalsVec <- ct(vals = Svals, setBreaks = seq(Smax, 0, by=-1))
svalsVec <- factor(x=svalsVec, levels=rev(levels(svalsVec)))
prop.table(table(svalsVec))

svalsDf0 <- data.frame(sCats=forcats::as_factor(nlevels(svalsVec):1),
                       sFreq=as.numeric(prop.table(table(svalsVec))))
str(svalsDf0)

svalsDf0$prozent <- round(svalsDf0$sFreq*100, digits = 1)
# Formatiert
svalsDf0$prozentFormatiert <- paste0(svalsDf0$prozent, "%")
svalsDf0$prozentFormatiert[svalsDf0$prozent==0] <- NA
svalsDf <- svalsDf0[!is.na(svalsDf0$prozentFormatiert),]
svalsDf$sCats <- droplevels(svalsDf$sCats)

svalBar <-
ggplot(data=svalsDf, aes(x=reorder(sCats, desc(sCats)), y=sFreq)) +
    geom_bar(stat="identity") +
    xlab(label="Risk ratio - Empirical S-values, separated by 1") +
    ylab(label="Relative frequency") +
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