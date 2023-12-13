rdForest <- 
    ggplot(fpDfRD, aes(x=est, y=model)) + geom_point(shape=124, size=5) +
    geom_errorbar(width=.15, aes(xmin=lci, xmax=uci), linewidth=1) +
    geom_vline(xintercept = 0, linetype = "dashed", color="red", linewidth=1) +
    geom_vline(xintercept = 0.02, linetype = "dashed", color="magenta", linewidth=1) +
    xlab(label="Risk difference (RD)") +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        # axis.text.y=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        panel.border = element_rect(color="black", fill=NA))
# legend.position = "top")

# ggsave(filename="plotRD8Forest.png", plot = rdForest, path = ggsavePath, device = "png", width=8, height=7, units="in", dpi=300)
