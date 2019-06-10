### FIGURE S1. Climate trend ####

# Data
source("scripts/prep_data.R")

bioclim_all <- readRDS("data/bioclim_corrected.RDS")
bioclim_all$year <- as.numeric(bioclim_all$year)
bioclim_all <- bioclim_all %>% subset(ID_PE %in% sp_mat$ID_PE & year >= 1970)


mean_TP <- aggregate(bioclim_all$sg_15, by = list(bioclim_all$year), mean)
mean_PP <- aggregate(bioclim_all$sg_06, by = list(bioclim_all$year), mean)
mean_CMI <- aggregate(bioclim_all$cmi, by = list(bioclim_all$year), mean)



pdf("ms/figures/figS1_clim_trend.pdf",
    width = 3, height = 7)
# quartz(width = 3.2, height = 7)
par(mfrow = c(3,1), mar = c(2,4,.5,.5), oma = c(2,0,0,0))

plot(x~Group.1, data = mean_TP, type = "l", las = 1,
     xlab = "", ylab = "", cex.axis=.9,
     col = "grey45")
abline(lm(sg_15~year, data = bioclim_all), lwd = 1.2)
mtext(paste("Slope =", round(lm(sg_15~year, data = bioclim_all)$coef[2],3), "°C/year"),
      3, line = -2, at = 1973, adj = 0,
      cex = .7)

mtext("Growing season temperature", 2, line = 2.7, cex = 0.8, font = 2)

plot(x~Group.1, data = mean_PP, type = "l", las = 1,
     xlab = "", ylab = "", cex.axis=.9,
     col = "grey45")
abline(lm(sg_06~year, data = bioclim_all), lwd = 1.2)
mtext(paste("Slope =", round(lm(sg_06~year, data = bioclim_all)$coef[2],3), "mm/year"),
      3, line = -2, at = 1973, adj = 0,
      cex = .7)

mtext("Growing season precipitation", 2, line = 2.7, cex = 0.8, font = 2)


plot(x~Group.1, data = mean_CMI, type = "l", las = 1,
     xlab = "", ylab = "", cex.axis=.9,
     col = "grey45")
abline(lm(cmi~year, data = bioclim_all), lwd = 1.2)
mtext(paste("Slope =", round(lm(cmi~year, data = bioclim_all)$coef[2],3), "cm/year"),
      3, line = -2, at = 1973, adj = 0,
      cex = .7)
mtext("Year", 1, line = 2.5, cex = 0.8, font = 2)
mtext("Annual Climate Moisture Index", 2, line = 2.7, cex = 0.8, font = 2)

dev.off()
