## Load libraries

library(here)
library(divDyn)
library(ggplot2)


## Load files

load(file = here("data", "Reefs.RData"))
load(file = here("data", "Corals.RData"))


## Plot raw number of reefs by environment

tsplot(stages, boxes="sys", shading="system", 
       boxes.args = list(border="white", col="#335C670D"),
       shading.col = c("white", "#335C670D"), xlim=16:95, 
       ylim=c(0,110), ylab="Number of reef sites", xlab="Age (Ma)",
       plot.args = list(font.lab=2))

cols <- "darkgrey"
cols <- shades::saturation(cols, 0.5)

lines(n.reefs$mid, n.reefs$n, lwd=2.5, col = "darkgrey", type="l") # all reef sites
lines(n.tur$mid, n.tur$n, lwd = 2.5, col = "#e59c2d", type = "l") # brown mesophotic
lines(n.eup$mid, n.eup$n, lwd = 2.5, col = "#aad3cb", type = "l") # euphotic
lines(n.blue$mid, n.blue$n, lwd = 2.5, col ="#1a95b4", type = "l") # blue mesophotic 

abline(v=c(443.07, 359.3, 251.9, 201.36, 184.2, 56), lty = 2, lwd = 1, col = "grey")


text(423, 105, "LOME", pos = 1, col = "grey", srt = 0)
text(346, 105, "D/C", pos = 1, col = "grey", srt = 0)
text(243, 105, "P/T", pos = 1, col = "grey", srt = 0)
text(209, 105, "T/J", pos = 1, col = "grey", srt = 0)
text(171, 105, "TOAE", pos = 1, col = "grey", srt = 0)
text(42, 105, "PETM", pos = 1, col = "grey", srt = 0)

plotnames <- c("brown", "euphotic", "blue", "all reef sites")
legend("topleft", inset=c(0.01, 0.1),
       legend= plotnames, fill=c("#e59c2d", "#aad3cb","#1a95b4", "darkgrey"), bg="white", cex = 0.8)


## Ugly proportion plot

tsplot(stages, shading="system", boxes="sys", 
       boxes.args = list(border="white", col="#335C670D"),
       shading.col = c("white", "#335C670D"),
       xlim=16:94, xlab = "Age (Ma)",
       ylab="Proportion of reef sites", ylim=c(0,1))


parts(all.prop$mid, all.prop$meso, prop=T, col=c("#1a95b4","#e59c2d","#aad3cb", "white", "darkgrey"), labs=F, border=NA)


### ggplot stacked proportions


ggplot() +
  geom_bar(data = app, aes(x = mid, y = prop, fill = meso),
           stat = "identity", position = "stack", width = 4, alpha = 0.8) +
  scale_fill_manual(values = c("#1a95b4", "#e59c2d", "#aad3cb", "white", "darkgrey")) +
  labs(x = "Age (Ma)", y = "Proportion of Reef Sites",
       fill = "Environment") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_reverse() +  # Geological time runs from past to present
  coord_geo(dat = list("periods"), pos = "bottom") +  # Adds geological timescale
  theme_minimal()



### Plotting number of colonial coral collections by environment 

tsplot(stages, boxes="sys", shading="system", 
       boxes.args = list(border="white", col="#335C670D"),
       shading.col = c("white", "#335C670D"), xlim=16:95, 
       ylim=c(0,300), ylab="Number of collections", xlab="Age (Ma)",
       plot.args = list(font.lab=2))

cols <- "darkgrey"
cols <- shades::saturation(cols, 0.5)

lines(col.tur$mid, col.tur$n_col, lwd = 2.5, col = "#e59c2d", type = "l") # brown mesophotic
lines(col.eup$mid, col.eup$n_col, lwd = 2.5, col = "#aad3cb", type = "l") # euphotic
lines(col.blue$mid, col.blue$n_col, lwd = 2.5, col ="#1a95b4", type = "l") # blue mesophotic
lines(unk$mid, unk$n_col, lwd=2.5, col = "darkgrey", type="l") # unknown

 

abline(v=c(443.07, 359.3, 251.9, 201.36, 184.2, 56), lty = 2, lwd = 1, col = "grey")

text(423, 298, "LOME", pos = 1, col = "grey", srt = 0)
text(346, 298, "D/C", pos = 1, col = "grey", srt = 0)
text(243, 298, "P/T", pos = 1, col = "grey", srt = 0)
text(209, 298, "T/J", pos = 1, col = "grey", srt = 0)
text(166, 298, "TOAE", pos = 1, col = "grey", srt = 0)
text(36, 298, "PETM", pos = 1, col = "grey", srt = 0)

plotnames <- c("brown", "euphotic", "blue", "unknown")
legend("topleft", inset=c(0.01, 0.1),
       legend= plotnames, fill=c("#e59c2d","#aad3cb","#1a95b4","darkgrey"), bg="white", cex = 0.6)


## Plot colonial coral diversity


tsplot(stages, boxes="sys", shading="system", 
       boxes.args = list(border="white", col="#335C670D"),
       shading.col = c("white", "#335C670D"), xlim=16:95, 
       ylim=c(0, 150), ylab="Sampled in bin colonial coral genera", xlab="Age (Ma)",
       plot.args = list(font.lab=2))

cols <- "darkgrey"
cols <- shades::saturation(cols, 0.5)

lines(dd$mid, dd$divSIB, lwd=2.5, col = "#1a95b4", type="l") # all reef sites


abline(v=c(443.07, 359.3, 251.9, 201.36, 184.2, 56), lty = 2, lwd = 1, col = "grey")

text(421, 140, "LOME", pos = 1, col = "grey", srt = 0)
text(346, 140, "D/C", pos = 1, col = "grey", srt = 0)
text(241, 140, "P/T", pos = 1, col = "grey", srt = 0)
text(209, 140, "T/J", pos = 1, col = "grey", srt = 0)
text(164, 140, "TOAE", pos = 1, col = "grey", srt = 0)
text(35, 140, "PETM", pos = 1, col = "grey", srt = 0)


## Plot Sampled in bin diversity of colonial corals by environment

tsplot(stages, boxes="sys", shading="system", 
       boxes.args = list(border="white", col="#335C670D"),
       shading.col = c("white", "#335C670D"), xlim=16:95, 
       ylim=c(0, 100), ylab="Sampled in bin colonial coral genera", xlab="Age (Ma)",
       plot.args = list(font.lab=2))

cols <- "darkgrey"
cols <- shades::saturation(cols, 0.5)

lines(brown.dd$mid, brown.dd$divSIB, lwd=2.5, col = "#e59c2d", type="l") # brown
lines(euph.dd$mid, euph.dd$divSIB, lwd=2.5, col = "#aad3cb", type="l") # euphotic
lines(blue.dd$mid, blue.dd$divSIB, lwd=2.5, col = "#1a95b4", type="l") # blue
lines(unk.dd$mid, unk.dd$divSIB, lwd=2.5, col = "darkgrey", type="l") # unknown



abline(v=c(443.07, 359.3, 251.9, 201.36, 184.2, 56), lty = 2, lwd = 1, col = "grey")

text(421, 98, "LOME", pos = 1, col = "grey", srt = 0)
text(346, 98, "D/C", pos = 1, col = "grey", srt = 0)
text(241, 98, "P/T", pos = 1, col = "grey", srt = 0)
text(209, 98, "T/J", pos = 1, col = "grey", srt = 0)
text(164, 98, "TOAE", pos = 1, col = "grey", srt = 0)
text(35, 98, "PETM", pos = 1, col = "grey", srt = 0)

plotnames <- c("brown", "euphotic", "blue", "unknown")
legend("topleft", inset=c(0.01, 0.1),
       legend= plotnames, fill=c("#e59c2d","#aad3cb","#1a95b4","darkgrey"), bg="white", cex = 0.6)
