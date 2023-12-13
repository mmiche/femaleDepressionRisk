resDf <- adjLs[[resDfVec[sel1To15]]][,c("adjrd", "adjrdLci", "adjrdUci")]

numberOfTests <- paste(nrow(resDf), "Tests")
statSignLevel <- c(paste0("p >= ", runRiskDf$a[sel1To15]),
                   paste0("p < ", runRiskDf$a[sel1To15]))

# Compatibility interval level.
(cixx <- gsub("\\.", "", 100-(runRiskDf$a[sel1To15]*100)))

plotDf <- resDf
plotDf <- plotDf[order(plotDf[,"adjrdLci"], decreasing = TRUE),]

signif <- plotDf[,"adjrdUci"] < 0 | plotDf[,"adjrdLci"] > 0
notSignif <- !signif

plotDf$StatSign <- NA
plotDf$StatSign[signif] <- "steelblue"
plotDf$StatSign[notSignif] <- "grey"

plotTbl <- tibble::rownames_to_column(plotDf, var="pred") %>% as_tibble()
plotTbl$pred <- forcats::as_factor(plotTbl$pred)

plotRisk <- 
    ggplot(data=plotTbl, aes(x=adjrd, y=pred, color = StatSign)) +
    geom_point(size=4, shape=124, color="black") +
    geom_errorbar(width=.45, aes(xmin=adjrdLci, xmax=adjrdUci), linewidth=.2) +
    ylab(label=numberOfTests) +
    geom_vline(xintercept = 0, linetype = "dashed", color="red", linewidth=.5) +
    geom_vline(xintercept = .02, linetype = "dashed", color="magenta", linewidth=.5) +
    xlab("Ajdusted Risk Difference (adjRD)") +
    scale_x_continuous(
        # RD 0, ..., 0.15
        breaks =
            c(-.1, -.05, 0, .025, .05, .1, .15),
        labels =
            c("-.1", "-.05", "0", ".025", ".05", ".1", ".15")) +
    scale_color_manual(values=c("gray", "steelblue"), labels = statSignLevel) +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        # axis.text.y=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_rect(color="black", fill=NA),
        legend.position = "top")

# ggsave(filename="plotRD.png", plot = plotRisk, path = ggsavePath, device = "png", width=8, height=7, units="in", dpi=300)