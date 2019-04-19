#### RICHNESS ####

### PACKAGES ####
library(vegan)
library(zoo)

### DATA ####
source("scripts/prep_TBI.R")

# Compute richess for each time period
rich1 <- rowSums(decostand(sp_mat1[,MySpecies], "pa"))
rich2 <- rowSums(decostand(sp_mat2[,MySpecies], "pa"))


Rich_diff <- cbind.data.frame(plot_id = sp_mat1$plot_id, 
                              rich1 = rich1, rich2 = rich2,
                              rich_diff = rich2-rich1, 
                              disturb = BCDdf$disturb, 
                              ecoreg5 = BCDdf$ecoreg5,
                              st_coordinates(xy))
Rich_diff1 <- Rich_diff %>% subset(disturb==0)
Rich_diff2 <- Rich_diff %>% subset(disturb==1)
Rich_diff3 <- Rich_diff %>% subset(disturb==2)


tbi_lat1 <- arrange(Rich_diff1,Y) %>% select(-ecoreg5, -disturb)
tbi_lat2 <- arrange(Rich_diff2,Y) %>% select(-ecoreg5, -disturb)
tbi_lat3 <- arrange(Rich_diff3,Y) %>% select(-ecoreg5, -disturb)

# Plot parameters

reg_title <- c("Sugar maple-hickory\nSugar maple-basswood", 
               "Sugar maple-yellow birch",
               "Balsam fir-yellow birch", 
               "Balsam fir-white birch", "Spruce-moss")

nplots <- table(Rich_diff$ecoreg5, Rich_diff$disturb)


richlim = c(1,6)
latlim = c(46,50)

pdf("ms/figures/figS6_richness.pdf", 
    width = 7.8, height = 3)
# quartz(width = 7.8, height = 3)
par(mfrow=c(1,3), las=1, mar=c(2,1,2.2,1), oma = c(1.8,2.3,0,0), xaxs="i", yaxs="i")
rollmean_plot(dat = tbi_lat1, var = c("rich1", "rich2"), 
              ylim = richlim, xlim = latlim, labx = T, 
              title = "No or minor disturbances", lgd = F)

mtext("Species richness", 2, las =0, font = 2, line = 1.9, cex = .85)
my.mtext(my.adj = 0.05,
         paste("Change in richness: ", myround(mean(tbi_lat1$rich_diff),3)), 1, 
      line = -2, cex = .7, adj = 0)
legend("topright", c("Historical surveys", "Contemporary surveys"), col = c("black", "grey45"), lwd = 1.5, bty = "n")

rollmean_plot(dat = tbi_lat2, var = c("rich1", "rich2"), 
              ylim = richlim, xlim = latlim, labx = T, laby = F,
              title = "Moderate disturbances", lgd = F)
mtext("Latitude", 1, font = 2, line = 2.3, cex = .85)
my.mtext(my.adj = 0.05,
         paste("Change in richness: ", myround(mean(tbi_lat2$rich_diff),3)), 1, 
            line = -2, cex = .7, adj = 0)

rollmean_plot(dat = tbi_lat3, var = c("rich1", "rich2"), 
              ylim = richlim, xlim = latlim, labx = T, laby = F,
              title = "Major disturbances", lgd = F)
my.mtext(my.adj = 0.05,
         paste("Change in richness: ", myround(mean(tbi_lat3$rich_diff),3)), 1, 
            line = -2, cex = .7, adj = 0)

dev.off()  