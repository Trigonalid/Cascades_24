Fig_ST <- ggplot(graf_ST, aes(x = dataset_type, y = value)) +
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  geom_signif(
    comparisons = list(c("PAR-CAT","CAT-PLANT"), 
                       c("CAT-PLANT","Subsampled")),
    map_signif_level = TRUE) +
  labs(title = "ST", x = "", y = "Dissimilarity  by species turnover") +
  theme_classic()

Fig_OS <- ggplot(graf_OS, aes(x = dataset_type, y = value)) +
  geom_boxplot() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  geom_signif(
    comparisons = list(c("PAR-CAT","CAT-PLANT"), 
                       c("CAT-PLANT","Subsampled")),
    map_signif_level = TRUE) +
  labs(title = "OS", x = "", y = "Dissimilairty explained by rewiring") +
  theme_classic()

Fig_WN <- ggplot(graf_WN, aes(x = dataset_type, y = value)) +
  geom_violin() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  geom_signif(
    comparisons = list(c("PAR-CAT","CAT-PLANT"), 
                       c("CAT-PLANT","Subsampled")),
    map_signif_level = TRUE) +
  labs(title = "WN", x = "", y = "General dissimilairty between the two networks") +
  theme_classic()

fig_subsampling <- ggarrange(Fig_WN, Fig_OS, Fig_ST, common.legend = TRUE, legend = "bottom", nrow = 1)


fig_subsampling
ggsave("output/fig/fig_subsampling.png",
       fig_subsampling,
       height = 9,
       width = 22,
       units = "cm"
)
