plrPlot <- 
    ggplot(data=df2x2Plr, aes(x=name, y=plr)) +
    geom_boxplot() +
    ylab(label="Positive Likelihood Ratio") +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        # axis.text.y=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        panel.border = element_rect(color="black", fill=NA))

# ggsave(filename="plotPosLR.png", plot = plrPlot, path = ggsavePath, device = "png", width=8, height=7, units="in", dpi=300)
