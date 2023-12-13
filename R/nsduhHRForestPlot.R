resDf <- collectCoxResLs[[resDfVec[sel1To9]]]

numberOfTests <- paste(nrow(resDf), "Tests")
statSignLevel <- c(paste0("p >= ", runCoxDf$a[sel1To9]),
                   paste0("p < ", runCoxDf$a[sel1To9]))

# Compatibility interval level.
(cixx <- gsub("\\.", "", 100-(runCoxDf$a[sel1To9]*100)))
pickCols <- c("hr",
              paste0("hrl", cixx), paste0("hru", cixx), "hrp")

hrDf <- resDf[,pickCols]
hrDf <- hrDf[order(hrDf$hrp),]
signif <- hrDf$hrp < runCoxDf$a[sel1To9]; notSignif <- !signif
hrDf$StatSign <- NA
hrDf$StatSign[signif] <- "steelblue"
hrDf$StatSign[notSignif] <- "grey"
summary(hrDf)

hrTbl <- tibble::rownames_to_column(hrDf, var="pred") %>% as_tibble()
hrTbl$pred <- forcats::as_factor(hrTbl$pred)

# 
plotHazard <-
    ggplot(hrTbl, aes(x=.data[[pickCols[1]]], y=.data[["pred"]], color = .data[["StatSign"]])) +
    geom_point(size=4, shape=124, color="black") +
    geom_errorbar(width=.45, aes(xmin=.data[[pickCols[2]]], xmax=.data[[pickCols[3]]]), linewidth=.2) +
    xlab("Hazard Ratio (HR)") +
    ylab(label = numberOfTests) +
    
    geom_vline(xintercept = 1, linetype = "dashed", color="red", linewidth=.5) +
    geom_vline(xintercept = 1.2, linetype = "dashed", color="magenta", linewidth=.5) +
    
    scale_x_continuous(trans='log2',
                       breaks = c(1, 1.25, 1.5, 1.75, 2, 2.25),
                       labels = c("1", "1.25", "1.5", "1.75", "2", "2.25")) +
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

# ggsave(filename="plotHR5.png", plot = plotHazard, path = ggsavePath, device = "png", width=8, height=7, units="in", dpi=300)
