# Header ----------------------------------------------------------------
# Project: Refugia
# File name: 02_data_visualisation.R
# Last updated: 2025-05-22
# Author: Lewis A. Jones; Danijela Dimitrijevic
# Email: LewisA.Jones@outlook.com; danijela.dimitrijevic@fau.de
# Repository: https://github.com/MURKYSIG/Refugia

# Load libraries --------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(deeptime)

# Load data -------------------------------------------------------------
# Get geological periods
periods <- data.frame(deeptime::periods)
# Filter to Ordovician and younger
periods <- subset(periods, max_age <= 485.4000)
# Load reef counts
reef <- read.csv("data/reef_counts.csv")
# Load collection counts
coll <- read.csv("data/collection_counts.csv")
# Load genus counts
genus <- read.csv("data/genus_counts.csv")

# Setup plot data -------------------------------------------------------
# Define xlim for plotting
xlim <- c(485.4000, 2.5800)
# Define events for plotting
events <- data.frame(time = c(443.07, 359.3, 251.9, 201.36, 184.2, 121, 94, 66, 56),
                     text = c("LOME", "D/C", "P/T", "T/J", "TOAE", "OAE1a", 
                              "OAE2", "End-Cretaceous", "PETM"),
                     text_pos = c(443.07, 359.3, 251.9, 206.36, 179.2, 121, 94, 66, 56))
# Define colours
cols <- c("#e59c2d", "#aad3cb", "#1a95b4", "#bcbddc")
# Set levels
reef$photic <- factor(x = reef$photic, levels = c("Brown mesophotic", 
                                                  "Euphotic", 
                                                  "Blue mesophotic",
                                                  "Unknown"))
coll$photic <- factor(x = coll$photic, levels = c("Brown mesophotic", 
                                                  "Euphotic", 
                                                  "Blue mesophotic",
                                                  "Unknown"))
genus$photic <- factor(x = genus$photic, levels = c("Brown mesophotic", 
                                                    "Euphotic", 
                                                    "Blue mesophotic",
                                                    "Unknown"))

# Reef plots -------------------------------------------------------------
# Create counts plot
reef_counts <- ggplot() +
  geom_rect(data = periods, 
            aes(xmin = max_age, xmax = min_age, ymin = -Inf, ymax = Inf),
            fill = rep(c("grey95", "white"), length.out = nrow(periods))) +
  geom_bar(data = reef, aes(x = mid_ma, y = n, fill = photic),
           position = "stack", stat = "identity", colour = "black", 
           width = reef$duration_myr, linewidth = 0.25) +
  geom_vline(data = events, aes(xintercept = time), 
             linetype = 1, colour = "red") +
  geom_label(data = events, aes(x = time, y = Inf, label = text), 
             vjust = 0.5, hjust = -0.5,
             fill = "white", size = 2, angle = 270) +
  scale_fill_manual(values = cols) +
  scale_x_reverse(limits = xlim) +
  ylab("Number of reef sites") + 
  xlab("Time (Ma)") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  coord_geo(height = unit(1.25, "line"), fill = "grey80", lab_color = "black")

# Create proportion plot
reef_prop <- ggplot() +
  geom_rect(data = periods, 
            aes(xmin = max_age, xmax = min_age, ymin = -Inf, ymax = Inf),
            fill = rep(c("grey95", "white"), length.out = nrow(periods))) +
  geom_bar(data = reef, aes(x = mid_ma, y = n, fill = photic),
           position = "fill", stat = "identity", colour = "black", 
           width = reef$duration_myr, linewidth = 0.25) +
  geom_vline(data = events, aes(xintercept = time), 
             linetype = 1, colour = "red") +
  scale_fill_manual(values = cols) +
  scale_x_reverse(limits = xlim) +
  ylab("Proportion of reef sites") + 
  xlab("Time (Ma)") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  coord_geo(height = unit(1.25, "line"), fill = "grey80", lab_color = "black")


# Collection plots ------------------------------------------------------

# Create counts plot
coll_counts <- ggplot() +
  geom_rect(data = periods, 
            aes(xmin = max_age, xmax = min_age, ymin = -Inf, ymax = Inf),
            fill = rep(c("grey95", "white"), length.out = nrow(periods))) +
  geom_bar(data = coll, aes(x = mid_ma, y = n, fill = photic),
           position = "stack", stat = "identity", colour = "black", 
           width = coll$duration_myr, linewidth = 0.25) +
  geom_vline(data = events, aes(xintercept = time), 
             linetype = 1, colour = "red") +
  geom_label(data = events, aes(x = time, y = Inf, label = text), 
             vjust = 0.5, hjust = -0.5,
             fill = "white", size = 2, angle = 270) +
  scale_fill_manual(values = cols) +
  scale_x_reverse(limits = xlim) +
  ylab("Number of collections") + 
  xlab("Time (Ma)") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  coord_geo(height = unit(1.25, "line"), fill = "grey80", lab_color = "black")

# Create proportion plot
coll_prop <- ggplot() +
  geom_rect(data = periods, 
            aes(xmin = max_age, xmax = min_age, ymin = -Inf, ymax = Inf),
            fill = rep(c("grey95", "white"), length.out = nrow(periods))) +
  geom_bar(data = coll, aes(x = mid_ma, y = n, fill = photic),
           position = "fill", stat = "identity", colour = "black", 
           width = coll$duration_myr, linewidth = 0.25) +
  geom_vline(data = events, aes(xintercept = time), 
             linetype = 1, colour = "red") +
  scale_fill_manual(values = cols) +
  scale_x_reverse(limits = xlim) +
  ylab("Proportion of collections") + 
  xlab("Time (Ma)") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  coord_geo(height = unit(1.25, "line"), fill = "grey80", lab_color = "black")


# Diversity plot --------------------------------------------------------

div <- ggplot() +
  geom_rect(data = periods, 
            aes(xmin = max_age, xmax = min_age, ymin = -Inf, ymax = Inf),
            fill = rep(c("grey95", "white"), length.out = nrow(periods))) +
  geom_bar(data = genus, aes(x = mid_ma, y = genus_counts, fill = photic),
           position = "stack", stat = "identity", colour = "black", 
           width = coll$duration_myr, linewidth = 0.25) +
  geom_vline(data = events, aes(xintercept = time), 
             linetype = 1, colour = "red") +
  geom_label(data = events, aes(x = time, y = Inf, label = text), 
             vjust = 0.5, hjust = -0.5,
             fill = "white", size = 2, angle = 270) +
  scale_fill_manual(values = cols) +
  scale_x_reverse(limits = xlim) +
  ylab("Number of genera") + 
  xlab("Time (Ma)") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  coord_geo(height = unit(1.25, "line"), fill = "grey80", lab_color = "black")

# Arrange plots ---------------------------------------------------------
p1 <- ggarrange(reef_counts, reef_prop, labels = c("A", "C"),
                nrow = 2, ncol = 1, align = "hv", legend = "none", 
                common.legend = TRUE)
p2 <- ggarrange(coll_counts, coll_prop, labels = c("B", "D"), 
                nrow = 2, ncol = 1, align = "hv", legend = "none")
# Plot
p <- ggarrange(p1, p2)
div <- ggarrange(div, labels = "E")
p <- ggarrange(p, div, nrow = 2, align = "hv", heights = c(1.75, 1.25))
p
# Save
ggsave("figures/counts.png", plot = p, 
       dpi = 300, width = 300, height = 300, units = "mm", bg = "white")
