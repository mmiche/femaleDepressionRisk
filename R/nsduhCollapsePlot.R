clpsPlot <-
    ggplot(data=clps, aes(x=name, y=clpsExcess)) +
    geom_boxplot() +
    ylab(label="Odds Ratio collapsibility problem") +
    xlab(label="Before and after outcome misclassification bias adjustment") +
    theme(
        panel.background = element_blank(),
        axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=14),
        # axis.text.y=element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=14),
        panel.border = element_rect(color="black", fill=NA))

# ggsave(filename="clpsPlot8.png", plot = clpsPlot, path = ggsavePath, device = "png", width=8, height=7, units="in", dpi=300)
