### FIGURE Supp. 4 TERNARY PLOT OF BETA, GAINS & LOSSES ####

### PACKAGES ####

library(dplyr)
library(RColorBrewer)
library(scales)
library(graphicsutils)

### FUNCTIONS ####

source("functions/BCD_simplex.R")

### DATA ####

source("scripts/prep_data.R")

reg_title <- c("Sugar maple-hickory\nSugar maple-basswood",
               "Sugar maple-yellow birch",
               "Balsam fir-yellow birch",
               "Balsam fir-white birch", "Spruce-moss")



# Graphical parameters

mylayout <- matrix(c(1:20), 4, 5, byrow = F)
mylayout <- cbind(matrix(c(0,21,22,23), 4), mylayout)
mylayout <- cbind(mylayout, matrix(c(0,24,25,26), 4))

n0 <- sum(BCDdf$disturb==0)
n1 <- sum(BCDdf$disturb==1)
n2 <- sum(BCDdf$disturb==2)

# Plot

pdf("ms/figures/figS5_ternary.pdf", 
    width = 10, height = 5)
# quartz(width = 10, height = 5)
layout(mylayout, widths = c(0.2,1,1,1,1,1,0.1), heights = c(.19,1,1,1))
for(reg in levels(BCDdf$ecoreg5)) {
  par(mar=c(0,0,0,0))
  plot0(text = reg_title[which(levels(BCDdf$ecoreg5)==reg)], font=2, cex = 1.2, xpd=T)

  # all forests
  par(mar=c(.8,0.8,.3,0.8))
  tmp <- subset(BCDdf, ecoreg5 == reg & disturb == 0, select = c("losses","gains"))
  BCD_simplex(tmp$gains, tmp$losses, cex = 1, lwd = 1.2, label_beta = F)

  # undisturbed
  tmp2 <- subset(BCDdf, ecoreg5 == reg & disturb == 1, select = c("losses","gains"))
  BCD_simplex(tmp2$gains, tmp2$losses, cex = 1, lwd = 1.2, label_beta = F)

  # disturbed
  tmp3 <- subset(BCDdf, ecoreg5 == reg & disturb == 2, select = c("losses","gains"))
  BCD_simplex(tmp3$gains, tmp3$losses, cex = 1, lwd = 1.2, label_beta = F)

}
# group of forests
par(mar=c(0,0.1,0,0))
plot0()
text(c(1.1,1.1), c(0.5,0.15),c("No or minor\ndisturbances",paste("n =", n0)),
     srt = 0, font = c(2,1), cex = 1.1, xpd=NA)

plot0()
text(c(1.1,1.1), c(0.5,0.15), c("Moderate\ndisturbances", paste("n =", n1)),
     srt = 0, font = c(2,1), cex = 1.1, xpd=NA)
plot0()
text(c(1.1,1.1), c(0.5,0.15), c("Major\ndisturbances", paste("n =", n2)),
     srt = 0, font = c(2,1), cex = 1.1, xpd=NA)


# arrow for beta
par(mar=c(0,0,0,0))
plot0()
myArrow(x0 = -0.9, y0 = -.8, x1 = -0.9, y1 = .8,
        col = "grey35", lwd = 2, angle = 30, L=.05, code=1, xpd=NA)

plot0()
myArrow(x0 = -0.9, y0 = -.8, x1 = -0.9, y1 = .8,
        col = "grey35", lwd = 2, angle = 30, L=.05, code=1, xpd=NA)
text(0.2, 0, "ß diversity", srt = 270, font = 2, cex = 1.2, xpd=T)

plot0()
myArrow(x0 = -0.9, y0 = -.8, x1 = -0.9, y1 = .8,
        col = "grey35", lwd = 2, angle = 30, L=.05, code=1, xpd=NA)

dev.off()
