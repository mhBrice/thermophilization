### FIGURE supp. 3 SPECIES TEMPORAL CHANGE ####

### PACKAGES ####

library(dplyr)
library(RColorBrewer)
library(scales)
library(graphicsutils)
library(vegan)

### FUNCTIONS ####

source('functions/plot_beta.R')

### DATA ####

source("scripts/prep_data.R")

sps_code <- read.csv2("../Quebec_data/raw_data/ref_spCode.csv")




### Wilcoxon paired test to select species ####

wil <- apply(sp_mat2[,MySpecies]-sp_mat1[,MySpecies], 2, function(x) wilcox.test(x)$p.value)

occ1 <- decostand(sp_mat1[,MySpecies],"pa")
occ2 <- decostand(sp_mat2[,MySpecies],"pa")
wil_pa <- apply(occ2[,MySpecies]-occ1[,MySpecies], 2, function(x) wilcox.test(x)$p.value)

sp_sel <- names(which(colSums(occ1)>20))
spnames <- as.character(sps_code$complete.name[match(sp_sel,sps_code$spCode)])

wil <- wil[sp_sel]
wil_pa <- wil_pa[sp_sel]
occ1 <- occ1[,sp_sel]
occ2 <- occ2[,sp_sel]
ord_pa <- order(apply(occ2-occ1, 2, sum))

### Graphical parameters ####

reg_title <- c("Sugar maple-hickory\nSugar maple-basswood",
               "Sugar maple-yellow birch",
               "Balsam fir-yellow birch",
               "Balsam fir-white birch", "Spruce-moss")

pch_gr <- sp_sel
pch_gr[pch_gr %in% boreal] = "B"
pch_gr[pch_gr %in% pioneer] = "P"
pch_gr[pch_gr %in% temperate] = "T"


pal.reg <- c("#D53E4F", "#FC8D59" ,"#FCCB3C", "#99D594", "#3288BD")


### PLOT ####

pdf("ms/figures/figS4_spchange.pdf", 
    height = 6, width = 6.6)
# quartz(height = 6, width = 6.6)
par(mfrow=c(1,2), mar = c(.5,.5,.1,.5), oma = c(3,6.6,.1,.8))

### Change in presence absence
cust_bp(y1 = sp_mat1[,sp_sel], y2 = sp_mat2[,sp_sel], type = "pa", col_txt = "black",
        spnames = spnames, pch_gr=pch_gr, text_x = "Change in tree occurrence",
        reg = BCDdf$ecoreg5, pal_reg = pal.reg, pstar = wil_pa)

### Change in abundance
cust_bp(y1 = sp_mat1[,sp_sel], y2 = sp_mat2[,sp_sel], type = "ab",
        col_txt = "black", sporder = ord_pa, bg = F, axis = F,
        reg = BCDdf$ecoreg5, pal_reg = pal.reg, pstar = wil)


legend(750, 22.5, legend = rev(reg_title),
       col = rev(pal.reg), pt.bg = alpha(rev(pal.reg), .5),
       pch = rep(21,6), xpd = NA, pt.lwd = 1.2,
       pt.cex = 1, cex = .7, bg = "white", box.col = "white")

dev.off()
