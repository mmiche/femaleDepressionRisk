# cols = column names.
pickCols <- c("propDiff", "conf.low", "conf.high", "p")

# Execute the selection. 
propDf0 <- collectPrpResLs[[sel1To15]]

numberOfTests <- paste(nrow(propDf0), "Tests")
statSignLevel <- c(paste0("p >= ", runPropTstDf$a[sel1To15]),
                   paste0("p < ", runPropTstDf$a[sel1To15]))

compatibilityLevel <- as.character(100-(runPropTstDf$a[sel1To15]*100))

# Subtract outcome (major depressive episode) proportion in females (estimate 1) from outcome proportion in males (estimate 2).
propDf0$propDiff <- propDf0$estimate1 - propDf0$estimate2
propDf <- propDf0[,pickCols]
propDf <- propDf[order(propDf[,4]),]
signif <- propDf[,4] < runPropTstDf$a[sel1To15]; notSignif <- !signif
propDf$statSign <- NA
propDf$statSign[signif] <- "steelblue"
propDf$statSign[notSignif] <- "grey"

plotTbl <- tibble::rownames_to_column(propDf, var="pred") %>% as_tibble()
plotTbl$pred <- forcats::as_factor(plotTbl$pred)

plotProp <- 
    ggplot(data=plotTbl, aes(x=.data[[pickCols[1]]], y=.data[["pred"]], color = .data[["statSign"]])) +
    geom_point(size=4, shape=124, color="black") +
    geom_errorbar(width=.45, aes(xmin=.data[[pickCols[2]]], xmax=.data[[pickCols[3]]]), linewidth=.2) +
    
    # Set 0 and 0.02 for difference of proportions.
    geom_vline(xintercept = 0, linetype = "dashed", color="red", linewidth=.5) +
    geom_vline(xintercept = .02, linetype = "dashed", color="magenta", linewidth=.5) +
    ylab(label = numberOfTests) +
    xlab(paste0("z statistic and ", compatibilityLevel, "% compatibility interval")) +
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

# ggsave(filename="plotPropTest.png", plot = plotProp, path = ggsavePath, device = "png", width=8, height=7, units="in", dpi=300)
