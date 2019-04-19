BCD_simplex <- function(gains, losses, 
                        main = NULL,
                        label_beta = T,
                        info = T,
                        centroid = T,
                        col_grid = "grey45",
                        cex = 1, lwd = 1.1, rot = -5*pi/4) 
  {
  
  colors <- rgb(losses,
                 gains,
                 1-(gains+losses), 
                 0.5)
  
  rotate_coord <- function(x, y, rotrad = rot) {
    x_rot <- x*cos(rotrad) + y*sin(rotrad)
    y_rot <- - x*sin(rotrad) + y*cos(rotrad) 
    xy_rot <- cbind.data.frame(x_rot, y_rot)
  }
  myround <- function(x, k=2) trimws(format(round(x, k), nsmall=k))
  
  # Rotation of axes
  BC_rot <- rotate_coord(losses, gains)
  gains_rot <- BC_rot[,1]
  losses_rot <- BC_rot[,2]
  
  # Grid coordinates
  gridx <- c(0, 0.5, 0.5)
  gridy <- c(0.5, 0, 0.5)
  grid_rot <- rotate_coord(gridx, gridy)
  gridx_rot <- grid_rot[,1]
  gridy_rot <- grid_rot[,2] 
  
  corner <- sqrt(2)/2
  
  # Plot
  plot(gains_rot, losses_rot, xlim = c(-corner-.1, corner+.1), ylim=c(-corner-.1, 0.1), 
       pch = 20, col = colors, axes = F, ann = F, cex = cex)
  
  # Pseudo-axis
  arrows(x0 = 0, y0 = 0.03, x1 = corner+0.03, y1 = -corner, 
         length = .05, col = col_grid, lwd = lwd)
  arrows(x0 = 0, y0 = 0.03, x1 = -corner-0.03, y1 = -corner, 
         length = .05, col = col_grid, lwd = lwd)
  
  arrows(x0 = 0, y0 = 0.03, x1 = 0, y1 = -corner, 
         length = 0, col = col_grid, lwd = lwd, lty = 2)
  
  polygon(gridx_rot, gridy_rot, border = col_grid, lty = 2, lwd = lwd)
  
  # Centroid
  centroid_x <- mean(losses)
  centroid_y <- mean(gains)
  centro_rot <- rotate_coord(centroid_x, centroid_y)
  
  points(centro_rot, cex = cex+.7, col = "grey75", bg= "black", pch = 21, lwd=1.1)
  
  # Labels
  text(corner+0.03, -corner-0.1, xpd = T,
       paste("Gains\n", myround(mean(gains)/mean(gains+losses)*100,0), " %"))
  text(-corner-0.03, -corner-0.1, xpd = T, 
       paste("Losses\n", myround(mean(losses)/mean(gains+losses)*100,0), " %"))
  
  if(label_beta) {
    text(0, 0.1, "Low ß diversity", xpd = T, font = 2)
    text(0, -.8, "High ß diversity", xpd = T, font = 2)
    #Title
    text(0, 0.15, main, xpd = T, font = 2, cex = 1.1)
  } else {
    #Title
    text(0, 0.1, main, xpd = T, font = 2, cex = 1.1)
  }

  # Axis value
  text(0, 0.07, "0", xpd = T, col = col_grid)
  text(corner+0.08, -corner, "1", xpd = T, col = col_grid)
  text(-corner-0.08, -corner, "1", xpd = T, col = col_grid)
  text(gridx_rot[1:2]*1.4, gridy_rot[1:2]*0.96, "0.5", xpd = T, col = col_grid)
  
  # Info
  
  
  if(info) {
    text(corner, -0.07, paste("ß =", myround(mean(gains+losses))), adj = 1, font =2)
    text(corner, -0.15, paste("n =", length(gains)), adj = 1)
  }
}


# TEST 
# BCD_simplex(BCDdf$gains, BCDdf$losses, cex = 1, lwd = 1.3)

# 15.3c Arrows =========================================================== 15.3c 
## From Kurt Taylor Gaubartz
# A solid arrowhead function
# 
# This function draws an arrow from (x0,y0) to (x1,y1) with a solid
# arrowhead. 
#
# The default arrowhead length is .25 inches. 
# The default angle of the arrowhead edges is 30 degrees.
# The other parameters are standard for line and polygon formatting.
#
# The basic logic here is to use matrix rotation to translate the arrowhead
# coordinates.
#
# Note that R's trigonometric functions work with radians rather than degrees
#
myArrow = function(x0, y0, x1, y1,     # Set up arrow function ----------------+
                   L = .25,                             # Default arrowhead length              |
                   angle = 30,                          # Default angle of arrowhead            |
                   code = 2,                            # Default arrowhead at x1,y1            |
                   col = par("fg"),                     # Default color                         |
                   ljoin = par("ljoin"),                # Default line joint style (0)          |
                   lty = par("lty"),                    # Default line type                     |
                   lwd = par("lwd"),                    # Default line width                    |
                   xpd = FALSE){                        # Default stay within plot area         |
  # Start function code                   |
  if(code == 1){                     # Reverse arrow direction               |
    tmp = x1; x1 = x0; x0 = tmp      # Switch x values                       |
  }                                  #                                       |
  if(code == 1){                     #                                       |
    tmp = y1; y1 = y0; y0 = tmp      # Switch y values                       |
  }                                  #                                       |
  #                                                                              |
  # We need to control for the aspect ratio or for different x,y scales          |
  #   in setting up the arrow heads. We'll do that by translating the            |
  #   usr parameter setting from the original x,y units to units based           |
  #   on the plot dimensions measured in inches.  This will allow us to          |
  #   adjust the angles to account for different x and y scales. Note,           |
  #   however, that rescaling the plot after it is drawn will distort            |
  #   the arrowheads.                                                            |
  #                                                                              |
  X0 = (x0 - par()$usr[1])/(par()$usr[2] - par()$usr[1]) * par()$fin[1]  #   |
  Y0 = (y0 - par()$usr[3])/(par()$usr[4] - par()$usr[3]) * par()$fin[2]  #   |
  X1 = (x1 - par()$usr[1])/(par()$usr[2] - par()$usr[1]) * par()$fin[1]  #   |
  Y1 = (y1 - par()$usr[3])/(par()$usr[4] - par()$usr[3]) * par()$fin[2]  #   |
  #                                                                              |    
  oldusr = par("usr")                # Save original usr settings            |
  par(usr = c(0, par("fin")[1],      # Set up new usr settings based         |
              0, par("fin")[2]))       #  on plot dimensions in inches         |
  #                                                                              |
  t = angle * pi/180                 # Convert angle degrees to radians      |
  slope = (Y1 - Y0)/(X1 - X0)        # Calculate slope of line               |
  S = atan(slope)                    # Slope angle in radians                |
  #                                       |
  M = ifelse(X1 < X0, -1, 1)         # Set a marker for X1 < X0              |
  # Set length of vector XA               |
  XA = sqrt((X1 - X0)^2 + (Y1 - Y0)^2)                                   #   |
  #                                                                              |
  # Get arrowhead vertices from rotated vectors                                  |
  XC = X0 + M * ((XA - L) * cos(S) + L * tan(t) * sin(S))                #   |
  YC = Y0 + M * ((XA - L) * sin(S) - L * tan(t) * cos(S))                #   |
  XB = X0 + M * ((XA - L) * cos(S) - L * tan(t) * sin(S))                #   |
  YB = Y0 + M * ((XA - L) * sin(S) + L * tan(t) * cos(S))                #   |
  #                     |                                                        |
  # Draw arrow line stopping at beginning of the arrowhead                       |
  lines(x = c(X0, X1 - M * L * cos(S)),                                  #   |
        y = c(Y0, Y1 - M * L * sin(S)),                                      #   |
        lty = lty, lwd = lwd,            # Apply line format options             |
        col = col, xpd = xpd,            #                                       |
        ljoin = ljoin)                   #                                       |
  polygon(x = c(X1, XB, XC),         # Draw arrow head                       |
          y = c(Y1, YB, YC),               #  at vertices                          |
          col = col,                       # Apply format options                  |
          border = col,                    #                                       |
          xpd = xpd)                       #                                       |
  par(usr = oldusr)                  # Reset to original usr settings        |
}                                      # End of myArrow function --------------+
