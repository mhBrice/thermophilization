### ROLLMEAN TRAITS ####

rollmean_plot <- function(dat, var, k=400, xlim = NULL, ylim = NULL,
                          col = c("black", "grey45"), lwd = 1.5,
                          labx = T, laby = T, title = NULL, inset=c(0,0), lgd = T) {

  roll <- rollmean(dat, k)
  plot0(xlim = xlim, ylim = ylim)

  mean1 <- mean(dat[,var[1]])
  mean2 <- mean(dat[,var[2]])

  abline(h=0, col="grey75")

  abline(h=mean1, lty = 3, col=col[1], lwd = 1.5)
  abline(h=mean2, lty = 3, col=col[2], lwd = 1.5)

  lines(roll[,var[1]] ~ roll[,'Y'],
       col = col[1], lwd = 1.5)
  lines(roll[,var[2]] ~ roll[,'Y'], data = roll, col = col[2], lwd=1.5)

  box2(1:4, lwd = 1.2)
  at <- pretty(xlim)
  if(labx) labx = paste0(at, "°N") else labx = labx
  axis(1, at = at, labels = F)
  axis(1, at = at, labels = labx, line = -.1, tick = F)
  axis(2, labels = laby, las = 1)


  lgd1 <- paste(ifelse(mean1>0, "+", "-"), myround(abs(mean1),3))
  lgd2 <- paste(ifelse(mean2>0, "+", "-"), myround(abs(mean2),3))

  if(lgd){
    legend("bottomright", legend = c(lgd1, lgd2),
           text.font = 2, cex = 1.1, text.col = col, bty = "n", pt.cex=0,
           inset = c(0 + inset[1], 0 + inset[2]))
  }

  mtext(title, 3, line = 1, font=2, cex = .8)
  mtext(paste(nrow(dat), "forest plots"), side= 3, line=.1, cex = .7)

}

### ∆CTI North tail South tail ####


thermo_plot <- function(cti, reg, ylim = NULL,
                          col = c("black", "#08519C", "#A50F15"),
                          labx = T, laby = T, title = NULL,
                          title_y = NULL, lgd = F, n = NULL) {

  plot(cti[,1]~reg, xlab = '', ylab = "", las = 1, axes = F, ylim = ylim, col = col[1L])

  abline(h=0, col="grey75")

  abline(v=1, col= alpha("grey75", .2), lwd = 35)

  box2(1:4, lwd = 1.2)
  mtext(c("All", letters[1:5]), 1, at = c(1:6), line = 1, cex = .8, font = 2)
  mtext(paste0("(",n,")"), 1, at = c(2:6), line = 2, cex = .65)
  # text(1:6, par("usr")[3]-.015, labels = paste0(reg, "\n", paste0("(",n,")")),
  #      srt = 45, pos = 2, xpd = NA, offset = c(.1,0), cex = 1.1)
  axis(1, labels = F, las = 1)
  axis(2, labels = laby, las = 1)


  mtext(title_y, 2, font = 2, line = 2.5, cex = .85, las=0, xpd = NA)
  points(cti[,2]~reg, col = col[2L], pch = 19)
  points(cti[,3]~reg, col = col[3L], pch = 19)

  if (lgd) {
  legend("bottom", legend = c("Shift in warm species", "Shift in cold
    species"), pch = 19, col = col[3:2], bty = "n")
  }


}





### ∆CTI vs species gains ####

plot_reg_cti <- function(delta_CTI, tbi, reg,
                         reg_title = NULL, lgd = NULL,title_x = "Gains",
                         xlim = c(0,1), ylim = c(-2,2), axis_x = T,
                         pch = 19, col_pts = alpha("black", .2), cex = .5,
                         col_line = "grey50", lwd = 1.5) {

  nreg <- length(unique(reg))

  dat <- cbind.data.frame(delta_CTI, tbi, reg)

  for(i in 1:nreg) {
    plot0(xlim = xlim, ylim = ylim)

    box2(1:2, lwd = 1.2)
    axis(1, tcl = -.4)

    if(i==1) axis(2, las = 1, tcl = -.4) else axis(2, labels = F, tcl = -.4)

    mtext(reg_title[i], 3, cex = .75, font = 2, line = .5)

    if(i==1) mtext(expression(Delta*"CTI"), 2, line = 1.8, cex = .8, xpd = NA, font = 2)

    if(i==3) mtext(title_x, 1, line = 2.3, cex = .8, font = 2)

    regi <- levels(reg)[i]

    tmp_reg <- dat[which(dat$reg==regi),]

    points(delta_CTI~tbi, data = tmp_reg,
           pch = pch, col = col_pts, cex = cex)

    lm1 <- lm(delta_CTI~tbi, data = tmp_reg)
    if(!any(is.na(lm1$coefficients))) {
      abline(lm1, col = col_line, lty = 2, lwd = lwd)}
    lm_res <- cbind(r2 = myround(summary(lm1)$adj.r.squared,2),
                    slope = myround(lm1$coefficients[[2]],2))

    my.mtext(my.adj = .95, paste0("Slope: ", lm_res[,2]),
          1, line = -2.5, adj = 1,
          cex = .6, font = 2)

    my.mtext(my.adj = .95, bquote(bold("R"[adj]^{"2"}*":" ~ .(lm_res[,1]))),
          1, line = -1, adj = 1,
          cex = .6, font = 2)

  }

}

### SPECIES CONTRIBUTION TO ∆CTI ####

sp_contrib_fun <- function(mat1, mat2, trait) {
  trait1 <- as.matrix(trait[rownames(trait) %in% colnames(mat1),])
  trait2 <- as.matrix(trait[rownames(trait) %in% colnames(mat2),])

  cwm1 <- functcomp(trait1, as.matrix(mat1))

  cwm2 <- functcomp(trait2, as.matrix(mat2))

  delta_cwm <- cwm2-cwm1

  sp_contrib_cwm <- list()

  for(i in 1:ncol(mat1)) {
    mat2new <- mat2
    mat2new[,i] <- mat1[,i] # abundance of species i remains unchange
    cwmi2 <- functcomp(trait, as.matrix(mat2new))

    tmp <- cwmi2-cwm1

    sp_contrib_cwm[[i]] <- delta_cwm - tmp

  }
  sp_contrib_cwm <- do.call("cbind", sp_contrib_cwm)

  colnames(sp_contrib_cwm) <- colnames(mat1)

  sp_contrib_cwm
}

### Round function
myround <- function(x, k = 2) trimws(format(round(x, k), nsmall = k))
