#### CTI ####

### PACKAGES ####
library(dplyr)
library(zoo)
library(scales)
library(graphicsutils)
library(FD)
library(effects)

### FUNCTIONS ####
source('functions/plot_beta.R')

source('functions/misc_fun.R')

source('functions/trait_fun.R')

### DATA ####

source("scripts/prep_data.R")


tree_trait <- readRDS("data/tree_trait_sti.RDS")


#### COMMUNITY TEMPERATURE INDEX ####

# Delta CAI
trait <- c("STI", "STq10", "STq90", "TolS")

CAIdiff <- cbind(plot_id = Comm_trait1$plot_id,
                 (Comm_trait2[,trait]-Comm_trait1[,trait])/BCDdf$time_interv*10,
                 BCD, BCD_boreal, BCD_temperate, BCD_pioneer,
                 disturb = BCDdf$disturb,
                 ecoreg5 = BCDdf$ecoreg5,
                 st_coordinates(xy))

CAIdiff1 <- CAIdiff %>% subset(disturb==0)
CAIdiff2 <- CAIdiff %>% subset(disturb==1)
CAIdiff3 <- CAIdiff %>% subset(disturb==2)


anov <- aov(STI_med~ as.factor(disturb), data = CAIdiff)
TukeyHSD(anov)

aggregate(CAIdiff$STI, by = list(CAIdiff$ecoreg5), mean)

####
tp_trait <- c("STI", "STq10", "STq90")
thermo <- aggregate(CAIdiff[,tp_trait],
                    by = list(CAIdiff$disturb, CAIdiff$ecoreg5), mean)

thermo_qc <- aggregate(CAIdiff[,tp_trait],
                    by = list(CAIdiff$disturb), mean)

thermo <- bind_rows(thermo_qc, thermo)

thermo$Group.2 <- as.character(thermo$Group.2)
thermo$Group.2[which(is.na(thermo$Group.2))] <- "All"
thermo$Group.2 <- ordered(thermo$Group.2, unique(thermo$Group.2))


####

beta_lat1 <- arrange(CAIdiff1,Y) %>% select(-ecoreg5, -disturb)
beta_lat2 <- arrange(CAIdiff2,Y) %>% select(-ecoreg5, -disturb)
beta_lat3 <- arrange(CAIdiff3,Y) %>% select(-ecoreg5, -disturb)


to_stack <- c("similarity", "b.gains", "b.losses",
              "p.gains", "p.losses",
              "t.gains",  "t.losses")


ctilim = c(-.1,.1)
latlim = c(46,50)

##############################################
### FIGURE 5. THERMOPHILIZATION ####
##############################################

reg_title <- c("Sugar maple-hickory\nSugar maple-basswood",
               "Sugar maple-yellow birch",
               "Balsam fir-yellow birch",
               "Balsam fir-white birch", "Spruce-moss")

nplots <- table(CAIdiff$ecoreg5, CAIdiff$disturb)

m <- matrix(c(1,2,3,4,5,
              6,7,8,9,0,
              10,10,10,10,10), 3,5,byrow=T)

#nolog -> si disturb ne considère pas logging
pdf("ms/figures/fig5_thermo.pdf",
    width = 7.8, height = 5.8)
# quartz(width = 7.8, height = 5.8)
layout(m, widths = c(0.24,1,1,1,0.06), heights = c(1,1, .17))

par(mar=c(2,0,2.2,0))
plot0()
mtext("Thermophilization (∆CTI)", side = 2, font = 2,
      line = -1.15, cex = .84,las=0, xpd = NA)
mtext("Succession (∆CSI)", side = 2, font = 2,
      line = -2.35, cex= .84, las=0, col="grey45")

par(las=1, mar=c(2,1,2.2,1), xaxs="i", yaxs="i")

### No disturbance
rollmean_plot(beta_lat1, var = c("STI", "TolS"), ylim = ctilim, xlim = latlim, labx = T,
              title = "No or minor disturbances")

my.mtext(my.adj = .92, letters[1], 3, adj = 0, line = -1.5)

### Moderate disturbances
rollmean_plot(beta_lat2, var = c("STI", "TolS"), ylim = ctilim, xlim = latlim,
              labx = T, laby = F,
              title = "Moderate disturbances")

mtext("Latitude", 1, font = 2, line = 2.2, cex = .85)

my.mtext(my.adj = .92, letters[2], 3, adj = 0, line = -1.5)

### Major disturbances
rollmean_plot(beta_lat3, var = c("STI", "TolS"), ylim = ctilim, xlim = latlim,
              labx = T, laby = F,
              title = "Major disturbances")

my.mtext(my.adj = .92, letters[3], 3, adj = 0, line = -1.5)

#
par(mar=c(2,0,2.2,0))
plot0(x = c(-1, 1), y = c(-1, 1))
text(-.2, .52, "+ warm-adapted sp.", srt = 270, cex = 1.1, font = 2, xpd=NA)
arrows(x0 = -2, y0 = .02, y1 = .98, angle = 15, length = .1, lwd = 2.1, xpd=NA)

text(-.2, -.5, "+ pioneer sp.", srt = 270, cex = 1.1, font = 2, col = "grey45", xpd=NA)
arrows(x0 = -2, y0 = -.02, y1 = -.98, angle = 15, length = .1, lwd = 2.1, col = "grey45", xpd = NA)

####

par(mar=c(2.8,0,1.4,0))
plot0()
mtext("Thermophilization (∆CTI)", side = 2, font = 2,
      line = -1.8, cex = .84,las=0, xpd = NA)

par(mar=c(2.8,1,1.4,1))

thermo0 <- subset(thermo, Group.1==0)
thermo_plot(cti = thermo0[,tp_trait], reg = thermo0$Group.2,
            ylim = c(-.08,.2), n = nplots[,1])

my.mtext(my.adj = .92, letters[4], 3, adj = 0, line = -1.5)

thermo1 <- subset(thermo, Group.1==1)
thermo_plot(cti = thermo1[,tp_trait], reg = thermo1$Group.2,
            ylim = c(-.08,.2), laby = F, n = nplots[,2])

my.mtext(my.adj = .92, letters[5], 3, adj = 0, line = -1.5)

thermo2 <- subset(thermo, Group.1==2)
thermo_plot(cti = thermo2[,tp_trait], reg = thermo2$Group.2,
            ylim = c(-.08,.2), laby = F, n = nplots[,3], lgd = T)

my.mtext(my.adj = .92, letters[6], 3, adj = 0, line = -1.5)

par(mar=c(0,2,0,0))
plot0()
w=0
for(i in 1:5) {
  text(-.9+w, -.3, letters[i], font = 2, pos = 4)
  w <- w + strwidth(paste(letters[i]," "))
  text(-.9+w, -.3, reg_title[i], font = 1, pos = 4)
  w <- w + strwidth(paste(reg_title[i], "     "))
}

dev.off()


########################################
#### FIGURE 6. SPECIES CONTRIBUTION TO ∆CTI ####
########################################

sps_code <- read.csv2("data/ref_spCode.csv")

tree_mat <- as.matrix(tree_trait[,c("STI")])
rownames(tree_mat) <- MySpecies


sp_contrib_cti <- sp_contrib(mat1 = sp_mat1[,MySpecies],
             mat2 = sp_mat2[,MySpecies],
             trait = tree_mat)



list_contrib <- list()
delta_sp <- (sp_mat2[,MySpecies] - sp_mat1[,MySpecies])
mat_contrib <- matrix(nrow=2, ncol=ncol(sp_contrib_cti), dimnames = list(c("+","-"), colnames(sp_contrib_cti)))
for(i in colnames(mat_contrib)) {
  mat_contrib[1,i] <- sum(sp_contrib_cti[delta_sp[,i]>0,i], na.rm=T)/nrow(sp_contrib_cti)
  mat_contrib[2,i] <- sum(sp_contrib_cti[delta_sp[,i]<=0,i], na.rm=T)/nrow(sp_contrib_cti)
  mat_contrib[is.na(mat_contrib)] <- 0
  list_contrib[["Quebec"]] <- mat_contrib
}

for(reg in levels(BCDdf$ecoreg5)) {
  mat_contrib <- matrix(nrow=2, ncol=ncol(sp_contrib_cti), dimnames = list(c("+","-"), colnames(sp_contrib_cti)))
  tmp <- sp_contrib_cti[which(reg == BCDdf$ecoreg5),]
  delta_sp <- (sp_mat2[which(reg == BCDdf$ecoreg5),MySpecies] - sp_mat1[which(reg == BCDdf$ecoreg5),MySpecies])

  for(i in colnames(mat_contrib)) {
    mat_contrib[1,i] <- sum(tmp[delta_sp[,i]>0,i], na.rm=T)/nrow(tmp)
    mat_contrib[2,i] <- sum(tmp[delta_sp[,i]<=0,i], na.rm=T)/nrow(tmp)
    mat_contrib[is.na(mat_contrib)] <- 0
    list_contrib[[reg]] <- mat_contrib
  }
}


# Species labels
x <- do.call("rbind",list_contrib)
sp_sel <- names(which(apply(abs(x), 2, function(x) any(x > 0.01))))
spnames <- as.character(sps_code$complete.name[match(sp_sel,sps_code$spCode)])
spnames[startsWith(spnames,"Alnus")] <- "Alnus incana"


pch_gr <- sp_sel
pch_gr[pch_gr %in% boreal] = "B"
pch_gr[pch_gr %in% pioneer] = "P"
pch_gr[pch_gr %in% temperate] = "T"

list_contrib <- lapply(list_contrib, function(x) x[,sp_sel])
ord <- order(list_contrib$Quebec[1,])
range(list_contrib)

pal_reg <- c("#D53E4F", "#FC8D59" ,"#FCCB3C", "#99D594", "#3288BD")

pdf("ms/figures/fig6_spcontrib_cti.pdf",
    height = 5.9, width = 5.9)
# quartz(height = 5.9, width = 5.9)
par(mar = c(2.2, 7.78, 1.9, 1.5))

par(yaxs="i")
bp <- barplot(list_contrib$Quebec[,ord], horiz = T, col = "transparent",
              border = NA, axisnames = F,  xlim = c(-.25,.25), axes = F)

abline(h = bp, xpd = NA, col = c(alpha("grey85", .35), alpha("grey85", .6)), lwd = 19.5)

barplot(list_contrib$Quebec[1,ord], horiz = T, col = alpha("grey15",.9),
        border = NA, axisnames = F, add = T, axes=F, width = .51, space = 1.352)
barplot(list_contrib$Quebec[2,ord], horiz = T, col = alpha("grey55",.9),
        border = NA, axisnames = F, add = T, axes=F, width = .51, space = 1.352)


# Points for Region
for(i in 1:5) {
  list_contrib[[i+1]][which(list_contrib[[i+1]]==0)] <- NA
  points(as.numeric(list_contrib[[i+1]][1,ord]), bp-.4, pch = 24,  lwd = 1.2,
         col = pal_reg[i], bg = alpha(pal_reg[i], 0.6), cex = .75, xpd = NA)
  points(as.numeric(list_contrib[[i+1]][2,ord]), bp-.25, pch = 6, lwd = 1.2,
         col = pal_reg[i], cex = .75, xpd = NA)
}

axis(1, labels = F, tcl = -.3, line = .1)
axis(1, cex.axis = .7, tick = F, line = -.6)

axis(3, labels = F, tcl = -.3, line = .1)
axis(3, cex.axis = .7, tick = F, line = -.6)

mtext("Species contribution to ∆CTI through gains or losses", 1, line = 1.2, cex = .85, font = 2)
mtext("Species change increases ∆CTI", 3, at = .13, line = 1.1, cex = .75)
mtext("Species change decreases ∆CTI", 3, at = -.13, line = 1.1, cex = .75)

abline(v = 0, lty = 2, col = "grey45")

mtext(spnames[ord], 2, at = bp, col = "black", las = 1,
      xpd =NA, line = 1.5, cex = .7, font = 3)

mtext(pch_gr[ord], 2, at = bp, line = .5, xpd = T, las =1, cex = .8, font = 2)

lgd <- legend(0.065, 19.5, legend = rev(reg_title),
       col = rev(pal_reg), pt.bg = alpha(rev(pal_reg), .5),
       pch = rep(24, 5), x.intersp = 1.7, xpd = NA,text.width = 5,
       pt.cex = .8, cex = .7, bg = "white", box.col = "white")
points(lgd$text$x-.01, lgd$text$y+.15, col = alpha(rev(pal_reg), .5), bg = NA,
       pch = rep(6, 5), lwd = 1.1, cex = .8)

lgd2 <- legend(0.065, 21.5,
               legend = c("Contribution through gains", "Contribution through losses"),
       col = c(alpha("grey15",.9), alpha("grey55",.9)), pch = 15, pt.cex = 1.1,
       xpd = NA, text.width = 5,
       cex = 0.7, bg = "white", box.col = "white", x.intersp = 1.7)
points(lgd2$text$x-.01, lgd2$text$y,
       col = c(alpha("grey15",.9), alpha("grey55",.9)),
       pch = c(24, 6), bg = c(alpha("grey15",.6), NA), lwd = 1.1, cex = .8)

dev.off()


############################################
### FIGURE S5. ∆CTI vs gains and losses ####
#############################################

col_d <- c("grey65", "grey40", "grey15")
c("grey55", "grey35", "black")

m <- matrix(c(1:15), 3, 5, byrow = T)


pdf("ms/figures/figS5_CTIvsGains.pdf",
    width = 8, height = 5.6)
# quartz(width = 8, height = 6)
layout(m)
par(mar=c(2.9,1,1.5,1), oma = c(1,2,1.5,0))

plot_reg_cti(delta_CTI= CAIdiff$STI, tbi = CAIdiff$t.gains,
             reg = BCDdf$ecoreg5,
             reg_title = reg_title, title_x = "Gains in temperate species",
             axis_x=F)

plot_reg_cti(delta_CTI= CAIdiff$STI, tbi = CAIdiff$p.gains,
             reg = BCDdf$ecoreg5,
             title_x = "Gains in pioneer species")

plot_reg_cti(delta_CTI= CAIdiff$STI, tbi = CAIdiff$b.losses,
             reg = BCDdf$ecoreg5,
             title_x = "Losses in boreal species")

dev.off()







### SUPP. REGRESSION CTI ####

sti <- CAIdiff$STI

sti_df <- BCDdf

# Scale variables

var_to_scale <- c("time_interv", "TP", "PP",
                  "slope_TP", "slope_PP", "slope_CMI",
                  "CMI_min", "TP_max", "TP_min",
                  "age_mean")
sti_df[, var_to_scale] <- scale(sti_df[, var_to_scale])

# Coefficients

# baseline climate
coefs_base <-c("TP", "PP", "I(TP^2)","I(PP^2)","time_interv")

# climate change
coefs_cc <-c("slope_TP", "slope_PP", "slope_CMI", "TP_max", "TP_min", "CMI_min")

# disturbances
coefs_dist <- c("age_mean", "rec_logging", "old_logging",
                "rec_nat_disturb", "old_nat_disturb",
                "rec_logging:slope_TP", "old_logging:slope_TP",
                "rec_nat_disturb:slope_CMI", "rec_nat_disturb:slope_TP")


# Formulas

form_a <- formula(paste0(" ~ ", paste(c(coefs_base,coefs_dist,coefs_cc), collapse = "+")))
form_b <- formula(paste0(" ~ ", paste(coefs_base, collapse = "+")))
form_c <- formula(paste0(" ~ ", paste(coefs_cc, collapse = "+")))
form_d <- formula(paste0(" ~ ", paste(coefs_dist, collapse = "+")))

# Model matrix

mm_a <- as.data.frame(model.matrix(form_a, sti_df))[,-1]
mm_b <- as.data.frame(model.matrix(form_b, sti_df))[,-1]
mm_c <- as.data.frame(model.matrix(form_c, sti_df))[,-1]
mm_d <- as.data.frame(model.matrix(form_d, sti_df))[,-1]

### STI regressions

lm(sti ~ ., mm_a) %>% summary

lm(sti ~ ., mm_b)  %>% summary

lm(sti ~ ., mm_c)  %>% summary

lm(sti ~ ., mm_d)  %>% summary


##### SUPP. ANOVA ####

anov <- aov(sti ~ rec_disturb+old_disturb, data = BCDdf)
# Interaction is not significant
TukeyHSD(anov)
eff_disturb <- allEffects(mod=anov)

quartz(width = 6, height = 3)
par(mfrow=c(1,2), mar =c(3.5, 1, 1, 1), oma = c(0,3,0,0), yaxs = "i")
plot(eff_disturb$rec_disturb$fit, type = "l",
     ylim = c(-.05, .06), axes = F, ann = F)
points(eff_disturb$rec_disturb$fit, pch = 19)
arrows(x0 = 1:3, y0 = eff_disturb$rec_disturb$lower,
       y1 = eff_disturb$rec_disturb$upper,
       code = 0)
abline(h=0, col = "grey")

box2(1:2, lwd = 1.2)
axis(1, at = 1:3, labels = c(0,1,2), cex.axis = .8)
axis(2, cex.axis = .8, las = 1)
mtext("Recent disturbances", 1, line = 2.3, cex = .9, font = 2)
mtext("Thermophilization (∆CTI)", 2, line = 2.8, cex = .9, font = 2)

plot(eff_disturb$old_disturb$fit, type = "l",
     ylim = c(-.05, .06), axes = F, ann = F)
points(eff_disturb$old_disturb$fit, pch = 19)
arrows(x0 = 1:3, y0 = eff_disturb$old_disturb$lower,
       y1 = eff_disturb$old_disturb$upper,
       code = 0)
abline(h=0, col = "grey")

box2(1:2, lwd = 1.2)
axis(1, at = 1:3, labels = c(0,1,2), cex.axis = .8)
axis(2, labels = F)
mtext("Old disturbances", 1, line = 2.3, cex = .9, font = 2)
