nlrPlot <-
    ggplot(data=df2x2Nlr, aes(x=name, y=nlr)) +
    geom_boxplot() +
    ylab(label="Negative Likelihood Ratio") +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        # axis.text.y=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        panel.border = element_rect(color="black", fill=NA))

# ggsave(filename="plotNegLR.png", plot = nlrPlot, path = ggsavePath, device = "png", width=8, height=7, units="in", dpi=300)
