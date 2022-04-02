library("scales") # Load scales R package
library("ggplot2")
#############
# SI Figure 1

dens3 <- tidyr::gather(HomoPanDivergences[,5:7])
dens3$key <- factor(dens3$key, levels = c('Min', 'ESTIMATION', 'Max'))

show_col(hue_pal()(3)) # Plot hex codes/colors

ggplot(dens3, aes(x = value, color = key)) + 
  geom_line(stat = "density") + 
  geom_vline(xintercept = median(HomoPanDivergences$Min, na.rm = TRUE),
             col = hue_pal()(3)[1], linetype = "dashed") +
  geom_vline(xintercept = median(HomoPanDivergences$ESTIMATION, na.rm = TRUE),
             col = hue_pal()(3)[2], linetype = "dashed") +
  geom_vline(xintercept = median(HomoPanDivergences$Max, na.rm = TRUE),
             col = hue_pal()(3)[3], linetype = "dashed") +
  scale_color_discrete(
    name = "Density curves",
    labels = c("Min range", "Divergence estimate", "Max range")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(name = "Mega-annum (Ma)", breaks = seq(0, 16, by = 1)) +
  theme_classic() + theme(legend.position = c(0.85, 0.7))

ggsave("SF_1.tiff", last_plot(), width = 5, height = 2, dpi = "retina",
       device = "tiff", bg = 'white')


