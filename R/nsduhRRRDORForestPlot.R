resDf <- collectRiskResLs[[resDfVec[sel1To15]]]

numberOfTests <- paste(nrow(resDf), "Tests")
statSignLevel <- c(paste0("p >= ", runRiskDf$a[sel1To15]),
                   paste0("p < ", runRiskDf$a[sel1To15]))

# Compatibility interval level.
(cixx <- gsub("\\.", "", 100-(runRiskDf$a[sel1To15]*100)))
colsToPlot <- list(
    rr=c("rr",
         paste0("rrl", cixx), paste0("rru", cixx), "rrp"),
    rd=c("rd",
         paste0("rdl", cixx), paste0("rdu", cixx), "rdp"),
    or=c("or",
         paste0("orl", cixx), paste0("oru", cixx), "orp"))


pickCols <- colsToPlot[[selRiskMeas]]

plotDf <- resDf[,pickCols]
plotDf <- plotDf[order(plotDf[,4]),]

# signif <- plotDf[,4] < runRiskDf$a[sel1To15]; notSignif <- !signif

if(selRiskMeas == "rr" | selRiskMeas == "or") {
    # If lower ci < 1 or upper ci > 1
    signif <- plotDf[,3] < 1 | plotDf[,2] > 1
} else if(selRiskMeas == "rd") {
    # If lower ci < 0 or upper ci > 0
    signif <- plotDf[,3] < 0 | plotDf[,2] > 0
}
notSignif <- !signif


plotDf$StatSign <- NA
plotDf$StatSign[signif] <- "steelblue"
plotDf$StatSign[notSignif] <- "grey"

plotTbl <- tibble::rownames_to_column(plotDf, var="pred") %>% as_tibble()
plotTbl$pred <- forcats::as_factor(plotTbl$pred)

plotRisk <- 
    ggplot(data=plotTbl, aes(x=.data[[pickCols[1]]], y=.data[["pred"]], color = .data[["StatSign"]])) +
    geom_point(size=4, shape=124, color="black") +
    geom_errorbar(width=.45, aes(xmin=.data[[pickCols[2]]], xmax=.data[[pickCols[3]]]), linewidth=.2) +
    ylab(label=numberOfTests)

if(selRiskMeas == "rr" | selRiskMeas == "or") {
    plotRisk <- plotRisk +
        # Set 1 and 1.2 for RR, OR.
        geom_vline(xintercept = 1, linetype = "dashed", color="red", linewidth=.5) +
        geom_vline(xintercept = 1.2, linetype = "dashed", color="magenta", linewidth=.5)
    
    if(selRiskMeas == "rr") {
        plotRisk <- plotRisk +
            xlab("Risk Ratio (RR)") +
            scale_x_continuous(
                trans='log2',
                # RR 1, ..., 2.5
                breaks =
                    c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5),
                labels =
                    c("1", "1.25", "1.5", "1.75", "2", "2.25", "2.5"))
    } else if(selRiskMeas == "or") {
        plotRisk <- plotRisk +
            xlab("Odds Ratio (OR)") +
            scale_x_continuous(
                trans='log2',
                # OR 1, ..., 3
                breaks =
                    c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3),
                labels =
                    c("1", "1.25", "1.5", "1.75", "2", "2.25", "2.5", "2.75", "3"))
    }
} else if(selRiskMeas == "rd") {
    plotRisk <- plotRisk +
        # Set 0 and 0.02 for RD.
        geom_vline(xintercept = 0, linetype = "dashed", color="red", linewidth=.5) +
        geom_vline(xintercept = .02, linetype = "dashed", color="magenta", linewidth=.5) +
        xlab("Risk Difference (RD)") +
        scale_x_continuous(
            # RD 0, ..., 0.15
            breaks =
                c(0, .025, .05, .075, .1, .125, .15),
            labels =
                c("0", ".025", ".05", ".075", ".1", ".125", ".15"))
}

(plotRisk <- plotRisk +
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
        legend.position = "top"))

# ggsave(filename="plotOR.png", plot = plotRisk, path = ggsavePath, device = "png", width=8, height=7, units="in", dpi=300)
