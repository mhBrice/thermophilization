### VARPART #####

varpart_fun <- function(Y, x1, x2 = NULL, x3 = NULL) {
  
  # Varpart
  vp <- varpart(Y, x1, x2, x3)
  
  # Adj R2
  r2 <- vp$part$indfract$Adj.R.squared
  
  # Fraction tests
  frac.1 <- anova(rda(Y, x1, cbind.data.frame(x2, x3)), step=10000)
  
  frac.2 <- anova(rda(Y, x2, cbind.data.frame(x1, x3)), step=10000)
  
  frac.3 <- anova(rda(Y, x3, cbind.data.frame(x2, x1)), step=10000)
  
  res <- list(varpart = vp, 
              r2a = r2, 
              frac.1 = frac.1, frac.2 = frac.2, frac.3 = frac.3,
              pval = c(frac.1$`Pr(>F)`[1], frac.2$`Pr(>F)`[1], frac.3$`Pr(>F)`[1]))
}

### BARPLOT OF COEFFICIENTS ####


coef_bp <- function(coefs, se, pstar = NULL,
                    coeflabs = NULL,
                    axis_y = T, at = NULL,
                    xlim = NULL, ylim = NULL,
                    text_x = "Regression slope coefficients", 
                    title = NULL,
                    col = "grey35", ci_col = "grey") {
  
  # Order
  coefs <- rev(coefs)
  se <- rev(se)
  pstar <- rev(pstar)
  coeflabs <- rev(coeflabs)
  
  # Barplot
  par(yaxs="i")
  bp <- barplot(coefs, plot = F)
  barplot(coefs, horiz = T, col = col, border = NA, axisnames = F, 
          xlim = xlim, ylim = c(0, max(bp)+.7), axes = F)
  
  axis(1, at = at, tcl = -.4)
  
  mtext(text_x, 1, line = 2.3, cex = .85)
  
  mtext(title, 3, line = .1, cex = .9, font = 2)
  
  abline(v=0, lty = 2, col = "grey45")
  
  # Labels
  if(is.null(coeflabs)) { coeflabs <- 1:length(coefs) } else { coeflabs <- coeflabs}
  
  if(axis_y) {
    mtext(coeflabs, 2, at = bp, las = 1, 
          xpd =NA, line = .2, cex = .7)
  }
  
  # Stars
  
  if(!is.null(pstar)) {
    pstar <- gtools::stars.pval(pstar)
    l <- (max(coefs)-min(coefs))/30
    text(x = coefs+sign(coefs)*l, y = bp, labels = pstar,
         cex = 1, font = 2, xpd = NA, srt = 90)
  }

  # Confidence Interval
  bp[se==0] <- NA
  se[se==0] <- NA
  arrows(y0 = bp, 
         x0 = ifelse(coefs>0, coefs-se, coefs),
         x1 = ifelse(coefs<0, coefs+se, coefs), 
         angle = 90, code=1, 
         length=0, col = "grey75", lwd = 1.5)
  
}


### VARPART PLOT ####

varpart_plot <- function(vp, labels = c('Baseline', "Climate change", "Disturbances"), pval, col_frac) {
  
  require(plotrix)
  
  r2a <- vp$part$indfract$Adj.R.square
  r2a[8] <- 1-r2a[8] # global r2a = 1-residuals
  r2a <- paste0(round(r2a*100,2), "%")
  
  # Empty plot
  plot0(xlim=c(-10,10), ylim = c(-10,10))
  rect(-11.6, -11, 11.6, 11, xpd = NA, border = "grey15")
  # Draw circles
  draw.circle(0, 3, 5.5, col = col_frac[1])
  draw.circle(-3, -3, 5.5, col = col_frac[2])
  draw.circle(3, -3, 5.5, col = col_frac[3])
  
  draw.circle(0, 3, 5.5)
  draw.circle(-3, -3, 5.5)
  
  # Fraction labels
  text(0, 10, labels[1], cex = 1.1, font =2, xpd = NA)
  text(-6, -10, labels[2], cex = 1.1, font =2, xpd = NA)
  text(6, -10, labels[3], cex = 1.1, font =2, xpd = NA)
  
  # Fraction r2a
  text(0, 5, paste0(r2a[1], stars.pval(pval[1])), cex = 1)

  text(-5.5, -4.5, paste0(r2a[2], stars.pval(pval[2])), cex = 1)
  
  text(5.5, -4.5, paste0(r2a[3], stars.pval(pval[3])), cex = 1)
  
  # Joint fractions r2
  text(-2.95, 1.1, r2a[4], cex = .9)
  
  text(0, -4.5, r2a[5], cex = .9)
  
  text(2.95, 1.1, r2a[6], cex = .9)
  
  text(0, -1, r2a[7], cex = .9)
  
  # Global r2a
  text(8, 6.6, expression(bold("Global R"[adj]^{"2"})), cex = 1, xpd = NA, font = 2)
  text(8, 4.7, r2a[8], cex = 1, xpd = NA, font = 2)
  
}




