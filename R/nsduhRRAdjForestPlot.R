resDf <- adjLs[[resDfVec[sel1To15]]][,c("adjrr", "adjrrLci", "adjrrUci")]

numberOfTests <- paste(nrow(resDf), "Tests")
statSignLevel <- c(paste0("p >= ", runRiskDf$a[sel1To15]),
                   paste0("p < ", runRiskDf$a[sel1To15]))

# Compatibility interval level. gsub(...) necessary, otherwise power = .995 will be messed up.
(cixx <- gsub("\\.", "", 100-(runRiskDf$a[sel1To15]*100)))

plotDf <- resDf; rm(resDf)
plotDf <- plotDf[order(plotDf[,"adjrrLci"], decreasing = TRUE),]

signif <- plotDf[,"adjrrUci"] < 1 | plotDf[,"adjrrLci"] > 1
notSignif <- !signif

plotDf$StatSign <- NA
plotDf$StatSign[signif] <- "steelblue"
plotDf$StatSign[notSignif] <- "grey"

plotTbl <- tibble::rownames_to_column(plotDf, var="pred") %>% as_tibble()
plotTbl$pred <- forcats::as_factor(plotTbl$pred)

plotRisk <- 
    ggplot(data=plotTbl, aes(x=adjrr, y=pred, color = StatSign)) +
    geom_point(size=4, shape=124, color="black") +
    geom_errorbar(width=.45, aes(xmin=adjrrLci, xmax=adjrrUci), linewidth=.2) +
    ylab(label=numberOfTests) +
    geom_vline(xintercept = 1, linetype = "dashed", color="red", linewidth=.5) +
    geom_vline(xintercept = 1.2, linetype = "dashed", color="magenta", linewidth=.5) +
    # # Use below code as template, if you want to adapt the x-axis.
    # scale_x_continuous(
    #     trans='log2',
    #     # RR 1, ..., 2.5
    #     breaks =
    #         c(.75, 1, 1.25, 1.5, 2, 2.5),
    #     labels =
    #         c(".75", "1", "1.25", "1.5", "2", "2.5")) +
    xlab("Ajdusted Risk Ratio (adjRR)") +
    scale_color_manual(values=c("gray", "steelblue"), labels = statSignLevel) +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_rect(color="black", fill=NA),
        legend.position = "top")

# ggsave(filename="plotRR.png", plot = plotRisk, path = ggsavePath, device = "png", width=8, height=7, units="in", dpi=300)