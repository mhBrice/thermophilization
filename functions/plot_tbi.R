### BUBBLE MAP TBI ####

map_tbi <- function(bg, col_bg = "grey95", border_bg = "grey50",
                     xy_pts, pal_xy, val_xy, pch = 19, breaks = 50) {
  
  lim <- st_bbox(bg)
  
  plot0(xlim = lim[c(1,3)], ylim = lim[c(2,4)])

  plot(bg$geom, border=border_bg, col = col_bg, add = T)
  
  plot(xy_pts, add = T, pch = pch,
       col = pal_xy(breaks)[cut(val_xy, breaks=breaks)], 
       cex = val_xy)
}



#### ROLLMEAN TBI ####

plot_roll <- function(val, coord, k = 100, 
                      cols, alphas, lwd = 1.2,
                      xlab = "", ylab = "", xlim = NULL, ylim = NULL, 
                      at = NULL) {
  
  roll_coord <- rollmean(coord, k)
  roll_val <- rollmean(val, k)
  
  lat_grat <- c(46,48,50,52)
  
  if(is.null(xlim)) xlim = range(roll_val) else xlim = xlim
  if(is.null(ylim)) ylim = range(roll_coord) else ylim = ylim
  
  plot0(xlim = xlim, ylim = ylim)
  
  abline(h = lat_grat, col = alpha("grey", .4), xpd = NA)
  
  for(i in 1:ncol(val)) {
    coli <- alpha(cols[i], alphas[i])
    lines(roll_coord~roll_val[,i], col = coli, lwd = lwd, xpd = NA)
  }

  axis(1, at=at,  cex=.7, line = 0, col = "grey35", col.axis = "grey35", tcl = .4, labels = F, lwd = 0, lwd.ticks = 1)
  axis(1, at=at,  cex=.7, line = -.7, col = "grey35", col.axis = "grey35", tick = F)
  
  axis(4, at = lat_grat, labels = paste0(lat_grat, "°N"), line = -.9, tick = F,
       col = "grey35", col.axis = "grey35", las =1, xpd = NA)
}

### STACKED AREA PLOT #####

stack_plot <- function(dat, stk, index, k = 400, 
                       lines, col, xlim = NULL, ylim = c(0,1), 
                       labx = T, laby = T, lgd = T, title = NULL) {
  
  roll <- rollmean(dat, k)
  
  for(i in stk) {
    roll[which(roll[,i]<0),i] <- 0
  }
  
  plot0(ylim = ylim, xlim = xlim, xaxs = "i", yaxs = "i")
  at <- pretty(xlim)
  if(labx) labx = paste0(at, "°N") else labx = labx
  axis(1, labels = labx, at = at)
  axis(2, labels = laby, las = 1)
  
  
  stackedAreas(t(roll[,stk]),index=roll[,index], add=T,
               col = c("transparent", col))
  
  lines(roll[,lines]~roll[,index], lwd = 1.8, col = "grey15")
  
  box2(1:4, lwd = 1.2)
  
  if(lgd) {
    
    legend("topleft",
           legend = c("Boreal gain", "Boreal loss",
                      "Pioneer gain", "Pioneer loss",  
                      "Temperate gain", "Temperate loss"), 
           fill = col, 
           border="transparent", bty="n", cex=.96, inset = c(0,0))
    
    text(49.1, .75, "ß diversity", 
         col = "grey15",  cex = .96)
    text(x = 48.4, y = .75, '}',  cex = 7.6, col = "grey15", 
         family = 'Helvetica Neue UltraLight')
  }
  mtext(title, 3, line = 1, font=2, cex = .8)
  mtext(paste(nrow(dat), "forest plots"), side= 3, line=.1, cex = .7)
}


### SPECIES CHANGE BARPLOTS ####


cust_bp <- function(y1, y2, sporder = NULL, type = "pa", tbi = NULL, bg = T,
                    text_x = "Change in tree abundance", spnames = NULL, pch_gr = NULL,
                    col_txt = 'black', col = "black", axis = T, 
                    xlim = NULL, reg = NULL, pal_reg = NULL,
                    pstar = NULL) {
  
  if(type=="pa") { 
    y1 <- decostand(y1, "pa")
    y2 <- decostand(y2, "pa")
    spchange <- apply(y2 - y1, 2, sum)
    spchange_reg <- aggregate(y2 - y1, by = list(reg), sum)[,-1]
  } else if(type=="ab") { 
    spchange <- apply(y2 - y1, 2, sum)
    spchange_reg <- aggregate(y2 - y1, by = list(reg), sum)[,-1]
  } else if(type=="beta") {
    spchange <- apply(y2-y1, 2, mean) 
    spchange_reg <- aggregate(y2-y1, by = list(reg), mean)[,-1]
  }
  spchange_reg[spchange_reg==0] <- NA
  
  nsp <- ncol(y1)
  
  if(is.null(spnames)) { spnames <- colnames(y1) } else { spnames <- spnames}

  if(is.null(sporder)) {
    sporder <- order(spchange)
    if(length(col_txt)>1) col_txt <- col_txt[sporder]
    spchange <- spchange[sporder]
    spchange_reg <- spchange_reg[sporder]
    spnames <- spnames[sporder]
    pstar <- pstar[sporder]
    pch_gr <- pch_gr[sporder]

  } else {
    if(length(col_txt)>1) col_txt <- col_txt[sporder]
    spchange <- spchange[sporder]
    spchange_reg <- spchange_reg[sporder]
    spnames <- spnames[sporder]
    pstar <- pstar[sporder]
    pch_gr <- pch_gr[sporder]
  }

  
  # Barplot
  par(yaxs="i")
  bp <- barplot(spchange, horiz = T, col = "transparent", 
                border = NA, axisnames = F,  xlim = xlim,
                cex.axis = .8)

  if(bg) abline(h=bp, xpd=NA, col = c(alpha("grey85", .35), alpha("grey85", .6)), lwd = 17)

  bp <- barplot(spchange, horiz = T, col = col, border = NA, axisnames = F, add = T,
                axes=F)
  
  mtext(text_x, 1, line = 2.2, cex = .85, font = 2)
  
  abline(v=0, lty = 2, col = "grey45")
  
  # Species name
  if(axis) {
    mtext(spnames, 2, at = bp, col = col_txt, las = 1, 
          xpd =NA, line = 1.5, cex = .66, font = 3)
    mtext(pch_gr, 2, at = bp, line = .6, xpd = T, las =1, cex = .8, font = 2)
  }
  
  # Points for Region 
  for(i in 1:5) {
    points(as.numeric(spchange_reg[i,]), bp, pch = 21, lwd = 1.2,
           col = pal_reg[i], bg = alpha(pal_reg[i], 0.5), cex = 1)
  }

  if(!is.null(pstar)) {
    pstar <- gtools::stars.pval(pstar)
    l <- (max(spchange)-min(spchange))/30
    text(x = spchange+sign(spchange)*l, y = bp, labels = pstar,
         cex = .7, font = 2, xpd = NA, srt = 90)
  }
}



