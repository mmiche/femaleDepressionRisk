#
rrorhrForest <-
    ggplot(fpDfRRORHR, aes(x=est, y=model)) + geom_point(shape=124, size=5) +
    geom_errorbar(width=.15, aes(xmin=lci, xmax=uci), linewidth=1) +
    geom_vline(xintercept = 1, linetype = "dashed", color="red", linewidth=1) +
    geom_vline(xintercept = 1.2, linetype = "dashed", color="magenta", linewidth=1) +
    scale_x_continuous(trans = "log2") +
    # # Use below code as template, if you want to further adapt the x-axis.
    # scale_x_continuous(trans='log2',
    #                    breaks = c(1, 1.25, 1.5, 1.75, 2),
    #                    labels = c("1", "1.25", "1.5", "1.75", "2")) +
    
    xlab(label="Risk ratio (RR), odds ratio (OR), and hazard ratio (HR)") +
    
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        panel.border = element_rect(color="black", fill=NA))

# ggsave(filename="plotRRORHRForest.png", plot = rrorhrForest, path = ggsavePath, device = "png", width=8, height=7, units="in", dpi=300)
