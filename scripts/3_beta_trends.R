#### Temporal beta diversity trends ####

### PACKAGES ####
require(dplyr)
require(reshape2)

require(adespatial)
require(vegan)
require(zoo)

require(sf)

require(RColorBrewer)
require(scales)
require(graphicsutils)


source('functions/plot_tbi.R')

source('functions/misc_fun.R')

### DATA ####

source("scripts/prep_data.R")

##################################
### Figure 1. REGION MAP ####
##################################

reg_title <- c("Sugar maple-hickory\nSugar maple-basswood",
               "Sugar maple-yellow birch",
               "Balsam fir-yellow birch",
               "Balsam fir-white birch", "Spruce-moss")

col_reg <- brewer.pal(6,"Spectral")[c(1,1:3,5,6)]

n_reg <- table(BCDdf$ecoreg5)

lim <- st_bbox(ecoregion)

pdf("ms/figures/fig1_region.pdf",
    width = 5, height = 3)
# quartz(width = 5, height = 3)
par(mar=c(1.8,2.3,.3,0.3))

plot0(xlim = lim[c(1,3)]+c(.3,.1), ylim = lim[c(2,4)]+c(-.1,.1),
      grid.col = alpha("grey60", .3), fill = "white")

box2(1:2)
plot(st_geometry(ecoregion), border = "grey50",
     col = alpha(col_reg,.4)[ecoregion$SOUS_DOM6],
     axes=F, add=T)

axis(1, labels = F, tcl = -.4)
axis(1, at = seq(-80,-60,by=5), labels = paste0(abs(seq(-80,-60,by=5)), "°W"),
     line = -.3, cex.axis = .8, tick = F)
axis(2, labels = F, tcl = -.4)
axis(2, at = seq(46,52,by=2), labels = paste0(seq(46,52,by=2), "°N"),
     line = -.3, las = 1, cex.axis=.8, tick = F)

plot(st_geometry(xy), cex = .1, pch = 19, col = alpha("grey15",.3), add = T)

points(-61.8, 47.5, cex= 3, pch = 19, col = "white")
text(-59.7, 53.1, paste0(reg_title[5], " (", n_reg[5], ")"), cex = .75, xpd = NA)
text(-59.2, 49.6, paste0(reg_title[4], "\n(", n_reg[4], ")"), cex = .75, xpd = NA)
text(-63.8, 47.5, paste0(reg_title[3], " (", n_reg[3], ")"), cex = .75)
text(-64.9, 46.5, paste0(reg_title[2], " (", n_reg[2], ")"), cex = .75)
text(-66.8, 45.2, paste0(reg_title[1], " (", n_reg[1], ")"), cex = .75)

dev.off()

#########################################
### Figure 2. MAPS + STACKPLOT ####
#########################################

tbi.pal <- colorRampPalette(c("#E8E6F0", "#A8A1C6", "#7469A4", "#352965"))
losses.pal <- colorRampPalette(brewer.pal(9, "Reds"))
gains.pal <- colorRampPalette(brewer.pal(9, "Blues"))

cols <- c("grey15", "#A50F15", "#08519C")

# Rollmean

BCD_lat <- cbind.data.frame(BCD, BCD_boreal, BCD_temperate, BCD_pioneer,
                            disturb = BCDdf$disturb, st_coordinates(xy))

xy.tbi <- BCD_lat %>%
  mutate(gains.rel = gains/tbi, losses.rel = losses/tbi)

tbi_lat <- arrange(xy.tbi, Y)

xy.tbi <- st_as_sf(xy.tbi, coords = c("X", "Y"))

k <- 500

BCDdf1 <- BCD_lat %>% subset(disturb==0)
BCDdf2 <- BCD_lat %>% subset(disturb==1)
BCDdf3 <- BCD_lat %>% subset(disturb==2)

mean(BCDdf1$losses)/mean(BCDdf1$tbi)*100
mean(BCDdf2$losses)/mean(BCDdf2$tbi)*100
mean(BCDdf3$losses)/mean(BCDdf3$tbi)*100

mean(BCDdf1$t.gains)
mean(BCDdf2$t.gains)
mean(BCDdf3$t.gains)

aovCRp <- aov(t.gains ~ as.factor(disturb), data=BCD_lat)
summary(aovCRp)
(tHSD <- TukeyHSD(aovCRp))

tbi_lat1 <- arrange(BCDdf1,Y)
tbi_lat2 <- arrange(BCDdf2,Y)
tbi_lat3 <- arrange(BCDdf3,Y)


to_stack <- c("similarity", "b.gains", "b.losses",
              "p.gains", "p.losses",
              "t.gains",  "t.losses")

latlim = c(46,50)

# Means
mean_tbi <- round(mean(xy.tbi$tbi),2)
mean_gains <- myround(mean(xy.tbi$gains),2)
mean_gains100 <- round(mean(xy.tbi$gains)/mean(xy.tbi$tbi)*100, 2)
mean_losses <- round(mean(xy.tbi$losses),2)
mean_losses100 <- round(mean(xy.tbi$losses)/mean(xy.tbi$tbi)*100, 2)


#### Maps of points ####
xy.gains <- which(xy.tbi$gains.rel > 0.5)
xy.losses <- which(xy.tbi$losses.rel > 0.5)
col_gr <- c("#08519C", "#C1D4E6", "#D97E21", "#F6DFC8", "#B22306", "#ECC8C1")

m <- matrix(c(0, 2,2,2,2,2,2, 3,3,3,3,3,3, 1,1,1,
           4, 5,5,5,5,5, 6,6,6,6,6, 7,7,7,7,7), 2, byrow=T)


pdf("ms/figures/fig2_map_roll.pdf",
    width = 6.7, height = 4.55)
# quartz(width = 6.7, height = 4.55)
layout(m, heights = c(.75, 1), widths = c(.38, rep(1,15)))

## Rollmean
lim = lim <- st_bbox(ecoregion)
par(oma = c(0,1,0,0), mar = c(1.5,0,1.6,2), xaxs="i", yaxs="i")
plot_roll(val=tbi_lat[,1:3], coord = tbi_lat$Y, k = 500,
          xlim = c(0.15,0.67), ylim = lim[c(2,4)],
          cols = cols, alphas = c(1, 1, 1), at = c(.2,.4,.6))

mtext("ß diversity", 3, line=.5, col=cols[1], font=2, cex = .8, at = .4)
mtext(expression("Gains" * phantom(" + Losses")), 3, line=-.7,
      col=cols[3], font=2, cex = .8, at = .4)
mtext(expression(phantom("Gains") * " + " * phantom("Losses")), 3, line=-.7,
      col="black", font=2, cex = .8, at = .4)
mtext(expression(phantom("Gains + ") * "Losses"), 3, line=-.7,
      col=cols[2], font=2, cex = .8, at = .4)
mtext(mean_tbi, 3, line=-1.9, cex = .8, at = .4)

par(mar = c(1.5,0,1.6,.4))

## Map of species gains
map_tbi(bg = ecoregion, xy_pts = xy.tbi$geom[xy.gains],
        val_xy = xy.tbi$gains[xy.gains], pal_xy = gains.pal)

legend("bottomright", legend = c("Gains", paste0(mean_gains, " (", mean_gains100, "%)")),
       bty = 'n', adj = 0.5, text.font = c(2,1), cex = 1.2,
       y.intersp = 1.1, inset = c(0,-.1), xpd = NA)

axis(2, at = c(46,48,50,52), labels = paste0(c(46,48,50,52), "°N"), line = -.9, tick = F,
     col = "grey35", col.axis = "grey35", las =1, xpd = NA)

mtext(letters[1], 3, adj = 0, line = .4)

## Map of species losses
map_tbi(bg = ecoregion, xy_pts = xy.tbi$geom[xy.losses],
        val_xy = xy.tbi$losses[xy.losses], pal_xy = losses.pal)

legend("bottomright", legend = c("Losses", paste0(mean_losses, " (", mean_losses100, "%)")),
       bty = 'n', adj = 0.5, text.font = c(2,1), cex = 1.2,
       y.intersp = 1.1, inset = c(0,-.1), xpd = NA)


### Stack plot

par(mar=c(3.5,0,2.5,0))
plot.new()
mtext("Temporal ß diversity", side = 2, font=2, line = -.1, cex= .8, las=0, xpd = NA)

par(las=1, mar=c(3.5,1,2.5,1))
stack_plot(dat = tbi_lat1, stk = to_stack, index = "Y", lines = "tbi", col = col_gr, xlim = latlim, title = "No or minor disturbances")
mtext(letters[2], 3, adj = -.1, line = .5)

stack_plot(dat = tbi_lat2, stk = to_stack, index = "Y", lines = "tbi", col = col_gr, xlim = latlim, laby=F, lgd = F, title = "Moderate disturbances")
mtext(letters[3], 3, adj = -.1, line = .5)

stack_plot(dat = tbi_lat3, stk = to_stack, index = "Y", lines = "tbi", col = col_gr, xlim = latlim, laby=F, lgd = F, title = "Major disturbances")
mtext(letters[4], 3, adj = -.1, line = .5)

mtext("Latitude", 1, outer = T, font=2, line = -1, cex = .8)

dev.off()


#########################################
### FIGURE 3. HISTOGRAMS by region ####
#########################################

aovCRp <- aov(tbi ~ as.factor(disturb)+ecoreg5, data=BCDdf)
summary(aovCRp)
(tHSD <- TukeyHSD(aovCRp))

col_d <- c("grey70", "grey55", "grey35")

mylayout <- matrix(c(6,1:5,0,7,7,7,7,7), 2, 6, byrow = T)

pdf("ms/figures/fig3_hist.pdf",
    width = 9.1, height = 2.5)

# quartz(width = 9.1, height = 2.5)
layout(mylayout, widths = c(.2,1,1,1,1,1), heights = c(1,.21))

par(mar = c(1.5,1,2.5,0.15), oma = c(0,0,0,.3), yaxs="i")
for(reg in levels(BCDdf$ecoreg5)) {

  BCDdf_reg <- subset(BCDdf, ecoreg5 == reg)

  hist(BCDdf_reg$tbi, breaks = 30,
       main = "",
       xlim = c(0,1), ylim = c(-3,200), col = col_d[3], border = "white",
       ylab ="", xlab="",
       cex.axis=.7, las = 1, axes = F, cex.main = 1.2)

  mtext(reg_title[which(levels(BCDdf$ecoreg5)==reg)], 3, line = 1.1,
        cex = .8, font = 2, padj = .5)

  BCDdf_reg1 = subset(BCDdf_reg, disturb != 2)

  hist(BCDdf_reg1$tbi, breaks = 30, col = col_d[2], border = "white", add=T)

  BCDdf_reg0 = subset(BCDdf_reg, disturb == 0)

  hist(BCDdf_reg0$tbi, breaks = 30, col = col_d[1], border = "white", add=T)

  mean_d <- aggregate(BCDdf_reg[,c("tbi", "losses")],
                      by = list(BCDdf_reg$disturb), mean)

  text(.725, c(185,165,145),
       rev(myround(mean_d[,"tbi"],2)),
       col = rev(col_d), cex = .98, font = 2)

  text(.94, c(185,165,145),
       paste0("(", rev(myround(mean_d[,"losses"]/mean_d[,"tbi"]*100,1)), "%)"),
       col = rev(col_d), cex = .98, xpd = NA, font = 2)

  axis(1, tcl = -.4, labels = F)
  axis(1, tick = F, line = -.1, cex.axis = .95)
  if(reg == "Sugar maple-basswood") axis(2, las =1, tick = F,line = -.1, cex.axis = .95)
  axis(2, labels=FALSE, tcl = -.4)
  box2(1:2, lwd = 1.2)
}

par(mar=c(0,0.1,0,0))
plot0()
mtext("Frequency", side = 2, cex = .9, line = -1.1)

par(mar=c(0,1,0.2,0.1))
plot0()
mtext("Temporal ß diversity", side = 3, line = -1.7, cex = .9)

legend("bottom",
       legend = c("No or minor disturbances", "Moderate disturbances", "Major disturbances"),
       fill = col_d, border = "white", cex = 1.15,
       bty = "n", horiz = T, xpd =NA, inset = c(0,-.17))

dev.off()
