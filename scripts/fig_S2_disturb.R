### FIGURE supp. 2 DISTURBANCE FREQUENCY #####

### PACKAGES ####
require(dplyr)

require(scales)

### DATA ####

source("scripts/prep_TBI.R")

### PREP ####

col_d <- c("grey70", "grey55", "grey35")

x=cbind.data.frame("No or minor" = plyr::count(BCDdf, "disturb")[,2],
                   Harvest = plyr::count(BCDdf, "logging")[,2],
                   "Old harvest" = plyr::count(BCDdf, "old_logging")[,2],
                   "Recent harvest" = plyr::count(BCDdf, "rec_logging")[,2],
                   Natural = plyr::count(BCDdf, "nat_disturb")[,2],
                   "Old natural" = plyr::count(BCDdf, "old_nat_disturb")[,2],
                   "Recent natural" = plyr::count(BCDdf, "rec_nat_disturb")[,2])


plyr::count(BCDdf, c("old_logging", "rec_logging"))

harvest2 <- cbind.data.frame(Old = 130, Both = 302, Recent = 942)
harvest1 <- cbind.data.frame(Old = 277, Both = 300, Recent = 793)
harvest0 <- 3537

plyr::count(BCDdf, c("old_nat_disturb","rec_nat_disturb"))

natural2 <- cbind.data.frame(Old = 80, Both = 200, Recent = 305)
natural1 <- cbind.data.frame(Old = 195, Both = 276, Recent = 1020)
natural0 <- 4205

natural <- t(rbind.data.frame(natural0=c(natural0,0,0,0), 
                              natural1=c(1491,natural1), 
                              natural2=c(585,natural2)))

harvest <- t(rbind.data.frame(harvest0=c(harvest0,0,0,0), 
                              harvest1=c(1370,harvest1), 
                              harvest2=c(1374,harvest2)))

tab_disturb <- cbind.data.frame(natural, harvest)


pdf("ms/figures/figS2_disturb.pdf", 
    width = 6, height = 4)
# quartz(width = 6, height = 4)
par(mar=c(3,4,.5,3))

# Barplots
barplot(as.matrix(tab_disturb[1,]), las = 1, border = "white", 
        beside = T, space = c(0,.5),
        col =  rep(col_d, 2), 
        ylab= "Number of disturbed forest plots", cex.axis = .8, axisnames=F)

barplot(as.matrix(tab_disturb[1,]), las = 1, border = "white", 
        beside = T, space = c(0,.5),
        col =  rep(c(NA, "grey70","grey50"), 2), density = c(0,15,15), 
        ylab= "", xlab="", axes=F, axisnames=F,  add=T)

barplot(matrix(colSums(tab_disturb[2:3,]),1), las = 1, border = "white", 
        beside = T, space = c(0,.5),
        col = rep(c(NA, "grey70","grey50"), 2),
        density = c(0,15,15),
        angle = 135, 
        ylab= "", xlab="", axes=F, axisnames=F, add=T)

barplot(as.matrix(tab_disturb[2,]), las = 1, border = "white", 
        beside = T, space = c(0,.5),
        col =  rep(col_d, 2),  
        ylab= "", xlab="", axes=F,axisnames=F,  add=T)

barplot(as.matrix(tab_disturb[2,]), las = 1, border = "white", 
        beside = T, space = c(0,.5),
        col =  rep(c(NA, "grey70","grey50"), 2), 
        density = c(0,15,15), angle = 135,
        ylab= "", xlab="", axes=F, axisnames=F,  add=T)

# Axis labels

lines(x=c(0.75,4.25), y = c(-150,-150), lwd = 2, xpd = NA)
mtext(c("Minor", "Moderate", "Major"), 1, at = c(1, 2.5, 4), line = .5,
      cex = .8)
mtext("Natural", 1, at = 2.5, line = 2, font = 2)

lines(x=c(5.25,8.75), y = c(-150,-150), lwd = 2, xpd = NA)
mtext(c("Minor", "Moderate", "Major"), 1, at = c(5.5, 7, 8.5), line = .5,
      cex = .8)
mtext("Harvest", 1, at = 7, line = 2, font = 2)

# Legend
text(9.4, 70, "Old", xpd = NA, adj = 0)
arrows(x0=9.5, x1=9, y0=70, y1=70, code= 0, lwd = 1.5)
text(9.4, 310, "Both", xpd = NA, adj = 0)
arrows(x0=9.5, x1=9, y0=310, y1=310, code= 0, lwd = 1.5)
text(9.4, 1000, "Recent", xpd = NA, adj = 0)
arrows(x0=9.5, x1=9, y0=1000, y1=1000, code= 0, lwd = 1.5)


dev.off()
