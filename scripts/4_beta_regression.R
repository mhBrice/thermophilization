#### Multiple regression + variation partioning analyses on temporal beta diversity ####

### PACKAGES ####
library(dplyr)
library(reshape2)

library(adespatial)
library(vegan)
library(car)
library(gtools)

library(scales)
library(graphicsutils)

### FUNCTIONS ####

source('functions/varpart_fun.R')
source('functions/misc_fun.R')


### DATA ####

source("scripts/prep_data.R")


### PREP ####

# Scale variables

var_to_scale <- c("time_interv", "TP", "PP",
                  "slope_TP", "slope_PP", "slope_CMI",
                  "CMI_min", "TP_max", "TP_min",
                  "age_mean")
BCDdf[, var_to_scale] <- scale(BCDdf[, var_to_scale])


# FORMULAS

beta_logit <- car::logit(BCDdf$tbi, adjust=0.01)
gains_logit <- car::logit(BCDdf$gains, adjust=0.01)
losses_logit <- car::logit(BCDdf$losses, adjust=0.01)

### Coefficients ####

# baseline climate

coefs_base <-c("TP", "PP", "I(TP^2)","I(PP^2)","time_interv")

# climate change

coefs_cc <-c("slope_TP", "slope_PP", "slope_CMI", "TP_max", "TP_min", "CMI_min")

# disturbances

coefs_dist <- c("age_mean", "rec_logging", "old_logging",
                "rec_nat_disturb", "old_nat_disturb",
                "rec_logging:slope_TP", "old_logging:slope_TP",
                "rec_nat_disturb:slope_CMI", "rec_nat_disturb:slope_TP")

### Formulas ####

form_a <- formula(paste0(" ~ ", paste(c(coefs_base,coefs_dist,coefs_cc), collapse = "+")))
form_b <- formula(paste0(" ~ ", paste(coefs_base, collapse = "+")))
form_c <- formula(paste0(" ~ ", paste(coefs_cc, collapse = "+")))
form_d <- formula(paste0(" ~ ", paste(coefs_dist, collapse = "+")))

### Model matrix ####

mm_a <- as.data.frame(model.matrix(form_a, BCDdf))[,-1]
mm_b <- as.data.frame(model.matrix(form_b, BCDdf))[,-1]
mm_c <- as.data.frame(model.matrix(form_c, BCDdf))[,-1]
mm_d <- as.data.frame(model.matrix(form_d, BCDdf))[,-1]

### REGRESSION MODELS ####

### TBI ####

lm_beta <- lm(beta_logit ~ ., mm_a)
summary(lm_beta)


r2a_b <- RsquareAdj(lm(beta_logit ~ ., mm_b))$adj.r.squared
r2a_c <- RsquareAdj(lm(beta_logit ~ ., mm_c))$adj.r.squared
r2a_d <- RsquareAdj(lm(beta_logit ~ ., mm_d))$adj.r.squared

# selection (takes a few seconds)
beta_sel_b <- forward.sel(beta_logit, mm_b, adjR2thresh = r2a_b)
beta_sel_c <- forward.sel(beta_logit, mm_c, adjR2thresh = r2a_c)
beta_sel_d <- forward.sel(beta_logit, mm_d, adjR2thresh = r2a_d)
beta_sel_d <- beta_sel_d[-9,]

(beta_var <- unique(c(beta_sel_b$variables,
                     beta_sel_c$variables,
                     beta_sel_d$variables)))

beta_sel <- lm(beta_logit ~ ., mm_a[,beta_var])

# coefficients

(beta_summ <- summary(beta_sel))
beta_pval <- beta_summ$coefficients[-1, 4]
beta_est <- beta_sel$coefficients[-1]
beta_se <- beta_summ$coefficients[-1, 2]

### LOSSES ####

lm_losses <- lm(losses_logit ~ ., mm_a)
summary(lm_losses)


r2a_b <- RsquareAdj(lm(losses_logit ~ ., mm_b))$adj.r.squared
r2a_c <- RsquareAdj(lm(losses_logit ~ ., mm_c))$adj.r.squared
r2a_d <- RsquareAdj(lm(losses_logit ~ ., mm_d))$adj.r.squared

# selection (takes a few seconds)
losses_sel_b <- forward.sel(losses_logit, mm_b, adjR2thresh = r2a_b)
losses_sel_c <- forward.sel(losses_logit, mm_c, adjR2thresh = r2a_c)
losses_sel_d <- forward.sel(losses_logit, mm_d, adjR2thresh = r2a_d)
losses_sel_d <- losses_sel_d[-11,]

(losses_var <- unique(c(losses_sel_b$variables,
                        losses_sel_c$variables,
                        losses_sel_d$variables)))

losses_sel <- lm(losses_logit ~ ., mm_a[,losses_var])

# coefficients

(losses_summ <- summary(losses_sel))
losses_pval <- losses_summ$coefficients[-1, 4]
losses_est <- losses_sel$coefficients[-1]
losses_se <- losses_summ$coefficients[-1, 2]

### GAINS ####

lm_gains <- lm(gains_logit ~ ., mm_a)
summary(lm_gains)

r2a_b <- RsquareAdj(lm(gains_logit ~ ., mm_b))$adj.r.squared
r2a_c <- RsquareAdj(lm(gains_logit ~ ., mm_c))$adj.r.squared
r2a_d <- RsquareAdj(lm(gains_logit ~ ., mm_d))$adj.r.squared

# selection (takes a few seconds)
gains_sel_b <- forward.sel(gains_logit, mm_b, adjR2thresh = r2a_b)
gains_sel_c <- forward.sel(gains_logit, mm_c, adjR2thresh = r2a_c)
gains_sel_d <- forward.sel(gains_logit, mm_d, adjR2thresh = r2a_d)

(gains_var <- unique(c(gains_sel_b$variables,
                       gains_sel_c$variables,
                       gains_sel_d$variables)))

gains_sel <- lm(gains_logit ~ ., mm_a[,gains_var])

# coefficients

(gains_summ <- summary(gains_sel))
gains_pval <- gains_summ$coefficients[-1, 4]
gains_est <- gains_sel$coefficients[-1]
gains_se <- gains_summ$coefficients[-1, 2]


### SELECTED VARIABLES ####

(all_var <- (unique(c(beta_var, losses_var, gains_var))))

all_var <- all_var[order(match(all_var, c("TP","I(TP^2)","PP", "I(PP^2)", "time_interv",
                                          "slope_TP","slope_PP",
                                          "TP_min", "TP_max", "CMI_min",
                                          "age_mean",
                                          "rec_logging1","rec_logging2",
                                          "old_logging1", "old_logging2",
                                          "rec_nat_disturb1", "rec_nat_disturb2",
                                          "old_nat_disturb1", "old_nat_disturb2",
                                          "rec_logging1:slope_TP")))]

### LABELS ####

labels_sig <- c("Temp", "Temp^2", "Precip", "Precip^2", "Delta*Time",
                "Delta*Temp", "Delta*Precip", "Temp~min", "Temp~max","CMI~min",
                "Age",
                "Recent~harvest[1]","Recent~harvest[2]",
                "Old~harvest[1]", "Old~harvest[2]",
                "Recent~natural[1]", "Recent~natural[2]",
                "Old~natural[1]", "Old~natural[2]",
                "Delta*Temp~x~Recent~harvest[1]")
.expressions <- labels_sig
labs_expressions <- parse(text = .expressions)


### COMBINE REGRESSION RESULTS ####

losses_reg <- gains_reg <- beta_reg <- data.frame(var = all_var, est = 0, se = 0, pval = 1)

for(i in 1:length(all_var)) {
  tmp <- gsub("`", "", names(beta_est)) == all_var[i]
  if(any(tmp)) {
    beta_reg[i, 2] <- beta_est[[which(tmp)]]
    beta_reg[i, 3] <- beta_se[[which(tmp)]]
    beta_reg[i, 4] <- beta_pval[[which(tmp)]]
  }

  tmp1 <- gsub("`", "", names(gains_est)) == all_var[i]
  if(any(tmp1)) {
    gains_reg[i, 2] <- gains_est[[which(tmp1)]]
    gains_reg[i, 3] <- gains_se[[which(tmp1)]]
    gains_reg[i, 4] <- gains_pval[[which(tmp1)]]
  }

  tmp2 <- gsub("`", "", names(losses_est)) == all_var[i]
  if(any(tmp2)) {
    losses_reg[i, 2] <- losses_est[[which(tmp2)]]
    losses_reg[i, 3] <- losses_se[[which(tmp2)]]
    losses_reg[i, 4] <- losses_pval[[which(tmp2)]]
  }

}


### VARPART ####

# TBI

vp_beta <- varpart_fun(Y = beta_logit,
                      x1 = mm_b[,beta_sel_b$variables],
                      x2 = mm_c[,beta_sel_c$variables],
                      x3 = mm_d[,beta_sel_d$variables])

# GAINS

vp_gains <- varpart_fun(Y = gains_logit,
                        x1 = mm_b[,gains_sel_b$variables],
                        x2 = mm_c[,gains_sel_c$variables],
                        x3 = mm_d[,gains_sel_d$variables])

# LOSSES

vp_losses <- varpart_fun(Y = losses_logit,
                         x1 = mm_b[,losses_sel_b$variables],
                         x2 = mm_c[,losses_sel_c$variables],
                         x3 = mm_d[,losses_sel_d$variables])

save(beta_reg, gains_reg, losses_reg,
     vp_beta, vp_gains, vp_losses, labs_expressions,
     file = "ms/figures/result_reg.rda")

load("ms/figures/result_reg.rda")


################################################
#### FIGURE 4. REGRESSION + VARPART ####
################################################


m <- matrix(c(1,2,3,4, 5,5,6,7), 2, 4, byrow = T)

x_lab <- barplot(beta_reg$est, plot = F)
col_frac <- c(alpha("grey", .07), alpha("grey", .25), alpha("grey", .5))

pdf("ms/figures/fig4_reg.pdf",
    width = 7, height = 5.657)

# quartz(width = 7, height = 5.657)
layout(m, widths = c(.47, .53, 1, 1), heights = c(1, .55))

par(mar = c(2.5, 0, 1.5, 0), yaxs="i", oma = c(0, 1, 0, .3))

plot0(x = rep(1, length(x_lab)), y = x_lab,
      xlim = c(0,1), ylim = c(max(x_lab)+.7, 0))

# Colored rectangles
rect(1.06, x_lab[1,]-.6, 10, x_lab[5,]+.6, col = col_frac[1], border = NA, xpd = NA)
rect(1.06, x_lab[6,]-.6, 10, x_lab[10,]+.6, col = col_frac[2], border = NA, xpd = NA)
rect(1.06, x_lab[11,]-.6, 10, x_lab[20,]+.6, col = col_frac[3], border = NA, xpd = NA)

# Coefs labels
text(1.05, x_lab, labs_expressions, xpd = NA, adj = 1, cex = .96)

par(mar = c(2.5,.8,1.5,.8))

coef_bp(coefs = beta_reg$est, se = beta_reg$se, pstar = beta_reg$pval, axis_y=F,
        at = c(-.5,0,.5,1),
        xlim = c(-.5,1.5), text_x = "", title = "ß diversity")

my.mtext(my.adj = -.1, letters[1], 3, adj = 0, line = 0)

coef_bp(coefs = gains_reg$est, se = gains_reg$se, pstar = gains_reg$pval, axis_y=F,
        xlim = c(-1,3.5), title = "Gains")

my.mtext(my.adj = 0.1, letters[2], 3, adj = 0, line = 0)

coef_bp(coefs = losses_reg$est, se = losses_reg$se, pstar = losses_reg$pval, axis_y=F,
        xlim = c(-3,1.5), text_x = "", title = "Losses")

my.mtext(my.adj = 0.1, letters[3], 3, adj = 0, line = 0)


### Venn diagram ####

par(mar = c(1,1.5,2.2,1.3))

varpart_plot(vp = vp_beta$varpart, pval = vp_beta$pval, col_frac = col_frac)
my.mtext(my.adj = 0.05, letters[4], 3, adj = 0, line = -.8)

varpart_plot(vp = vp_gains$varpart, pval = vp_gains$pval, col_frac = col_frac)
my.mtext(my.adj = 0.05, letters[5], 3, adj = 0, line = -.8)

varpart_plot(vp = vp_losses$varpart, pval = vp_losses$pval, col_frac = col_frac)
my.mtext(my.adj = 0.05, letters[6], 3, adj = 0, line = -.8)


dev.off()
