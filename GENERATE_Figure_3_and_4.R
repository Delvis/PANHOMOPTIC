source("prepare_data.R")

# Code for Figure 3:

ggplot(aes(x = date, y = ESTIMATION), data = HomoPanDivergences) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 5.33, ymax = 11.63, alpha = .05) + # Late Miocene
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 2.8, fill = "#e74c3c", alpha = .7) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2.8, ymax = FAD_A, fill = "#e74c3c", alpha = .6) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = FAD_A, ymax = FAD_B, fill = "#e67e22", alpha = .6) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = FAD_B, ymax = FAD_C, fill = "#f1c40f", alpha = .5) +
  geom_smooth(data = sahelReg, aes(x = date, y = ESTIMATION), fill = "#9b59b6", col = "#9b59b6", method = "lm", size = 1.2) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1, col = "black", alpha = 0.2) +
  geom_hline(yintercept = 2.8, col = "#e74c3c", size = 1.2) +
  geom_hline(yintercept = FAD_A, col = "#e74c3c", size = 1.2, linetype = "dashed") +
  geom_hline(yintercept = 5.33, col = "black", size = 1, linetype = "dotted") +
  geom_hline(yintercept = FAD_B, col = "#e67e22", size = 1.2, linetype = "dashed") +
  geom_hline(yintercept = FAD_C, col = "#f1c40f", size = 1.2, linetype = "dashed") +
  geom_hline(yintercept = 8, col = "#3498db", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 9.5, col = "#3498db", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 9.9, col = "#3498db", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 11.63, col = "black", size = 1, linetype = "dotted") +
  geom_hline(yintercept = 12.5, col = "#7ed6df", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 13, col = "#7ed6df", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 13.5, col = "#7ed6df", size = 1, linetype = "dashed") + 
  #geom_vline(xintercept = 2005, col = "#2ecc71", size = 1, linetype = "dashed") +
  geom_vline(xintercept = 1994, col = "#e74c3c", size = 1.2, linetype = "dashed") +
  geom_vline(xintercept = 2001, col = "#e67e22", size = 1.2, linetype = "dashed") +
  geom_vline(xintercept = 2002.583, col = "#f1c40f", size = 1.2, linetype = "dashed") +
  xlab("Publication date") + ylab(ggyaxis) +
  scale_x_continuous(breaks = round(seq(1965, 2025, by = 5),1), limits = c(1967, 2027)) +
  scale_y_continuous(breaks = round(seq(0, 15, by = 1))) +
  geom_errorbar(aes(ymin = Min, ymax = Max), width = 0.75, linetype = "dotted", alpha = 0.8) + 
  geom_point(size = 2) +
  theme_minimal() +
  # ggtitle("Molecular clock estimations for the split of the Pan/Homo lineage") +
  geom_label(label = "Paranthropus & \n Homo FADs", x = 2026.1, y = 2.8,
             label.padding = unit(0.4, "lines"), color = "black", fill = "#e74c3C") +
  geom_label(label = "Ardipithecus ramidus & \n Australopithecus FADs", x = 2024.5, y = FAD_A,
             label.padding = unit(0.4, "lines"), color = "black", fill = "#e74c3C") +
  geom_label(label = "Ardipithecus & \n Orrorin FADs", x = 2026.1, y = FAD_B,
             label.padding = unit(0.4, "lines"), color = "black", fill = "#e67e22") +
  geom_label(label = "Sahelanthropus", x = 2026.1, y = FAD_C,
             label.padding = unit(0.4, "lines"), color = "black", fill = "#f1c40f") +
  geom_label(label = "Chororapithecus", x = 2026.3, y = 8,
             label.padding = unit(0.4, "lines"), color = "black", fill = "#3498db") +
  geom_label(label = "Nakalipithecus & \n Samburupithecus", x = 2025.9, y = 9.7,
             label.padding = unit(0.4, "lines"), color = "black", fill = "#3498db") +
  geom_label(label = "Otavipithecus", x = 2026, y = 12.4,
             label.padding = unit(0.4, "lines"), color = "black", fill = "#7ed6df") +
  geom_label(label = "Ngorora ape", x = 2026, y = 13,
             label.padding = unit(0.4, "lines"), color = "black", fill = "#7ed6df") +
  geom_label(label = "Kenyapithecus LAD", x = 2025.5, y = 13.6,
             label.padding = unit(0.4, "lines"), color = "black", fill = "#7ed6df") +
  annotate(geom = "text", size = 7, x = 1994.6, y = 15, label = "A", color = "#e74c3C") +
  annotate(geom = "text", size = 7, x = 2001.6, y = 15, label = "B", color = "#e67e22") +
  annotate(geom = "text", size = 7, x = 2003.3, y = 15, label = "C", color = "#f1c40f",) #+

# SAVE Figure 3

ggsave("Figure_3.tiff", last_plot(), width = 12, height = 7,
       scale = 1.2, dpi = "retina", device = "tiff", bg = 'white')

###############

# Code for Figure 4:

ggplot(aes(x = date, y = ESTIMATION), data = HomoPanDivergences) +
  annotate("rect", xmin = 1994, xmax = Inf, ymin = -Inf, ymax = FAD_A, fill = "#e74c3c", alpha = .6) +
  annotate("rect", xmin = 2001, xmax = Inf, ymin = FAD_A, ymax = FAD_B, fill = "#e67e22", alpha = .6) +
  annotate("rect", xmin = 2002.583, xmax = Inf, ymin = FAD_B, ymax = FAD_C, fill = "#f1c40f", alpha = .5) +
  geom_hline(yintercept = FAD_A, col = "#e74c3c", size = 1.2, linetype = "dashed") +
  geom_hline(yintercept = 5.33, col = "black", size = 1, linetype = "dotted") +
  geom_hline(yintercept = FAD_B, col = "#e67e22", size = 1.2, linetype = "dashed") +
  geom_hline(yintercept = FAD_C, col = "#f1c40f", size = 1.2, linetype = "dashed") +
  geom_hline(yintercept = 11.63, col = "black", size = 1, linetype = "dotted") +
  geom_vline(xintercept = 2005.75, col = "#2ecc71", size = 1, linetype = "dashed") +
  geom_vline(xintercept = 1994, col = "#e74c3c", size = 1.2, linetype = "dashed") +
  geom_vline(xintercept = 2001, col = "#e67e22", size = 1.2, linetype = "dashed") +
  geom_vline(xintercept = 2002.583, col = "#f1c40f", size = 1.2, linetype = "dashed") +
  xlab("Publication date") + ylab(ggyaxis) +
  scale_x_continuous(breaks = round(seq(1980, 2025, by = 5),1), limits = c(1981, 2024.5)) +
  scale_y_continuous(breaks = round(seq(0, 15, by = 1))) +
  geom_smooth(data = filteredReg, aes(x = date, y = ESTIMATION), method = "lm", formula = y ~ poly(x, 2), size = 1, col = "black", alpha = 0.2) +
  geom_smooth(data = genomeReg, aes(x = date, y = ESTIMATION), fill = "#2ecc71", col = "#2ecc71", method = "lm", size = 1.2) +
  geom_errorbar(aes(ymin = Min, ymax = Max), width = 0.5, linetype = "dotted", alpha = 0.5) + 
  geom_point(size = 3, aes(shape = Type, color = Type)) +
  scale_color_manual(values = c("#2c3e50", "#27ae60", "#3498db", "black")) +
  scale_shape_manual(values = c(16,17,15,25)) +
  theme_minimal() +
  geom_label(label = "Ardipithecus ramidus & \n Australopithecus FADs", x = 2023, y = FAD_A,
             label.padding = unit(0.4, "lines"), color = "black", fill = "#e74c3C") +
  geom_label(label = "Ardipithecus & \n Orrorin FADs", x = 2024, y = FAD_B,
             label.padding = unit(0.4, "lines"), color = "black", fill = "#e67e22") +
  geom_label(label = "Sahelanthropus", x = 2024, y = FAD_C,
             label.padding = unit(0.4, "lines"), color = "black", fill = "#f1c40f") +
  annotate(geom = "text", size = 4.8, x = 1994, y = 14, label = "White el al. 1994 \n Leakey et al. 1995", color = "#e74c3C", angle = 90) +
  annotate(geom = "text", size = 4.8, x = 2001, y = 14, label = "Haile-Selassie, 2001 \n Senut el al. 2001", color = "#e67e22", angle = 90) +
  annotate(geom = "text", size = 4.8, x = 2003, y = 14, label = "Brunet el al. 2002", color = "#f1c40f", angle = 90) +
  annotate(geom = "text", size = 4.8, x = 2005.75, y = 14, label = "Chimpanzee \n genome (2005)", color = "#2ecc71", angle = 90) +
  theme(legend.position = c(0.95, 0.9), legend.background = element_rect(fill = "white"), legend.text = element_text(size=10, face = "bold"))


# SAVE Figure 4

ggsave("Figure_4.tiff", last_plot(), width = 12, height = 7,
       scale = 1.2, dpi = "retina", device = "tiff", bg = 'white')
