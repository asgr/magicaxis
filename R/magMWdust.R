#=========================================
#
# File Name : magMWdust.R
# Created By : awright
# Creation Date : 03-06-2026
# Last Modified : Wed Jun  3 12:50:43 2026
#
#=========================================


# Define a plotting helper that overlays Milky Way dust on a projected sky plot.
magMWdust <- function(type = "p", pch = 16, pt.cex = 0.5, dlon = 1, dlat = 1, opacity.range = c(0, 0.5), whiteblack.percentile = c(0.5, 0.95), stretch = "lin", min.opacity.plot = 0.01, ...) {

  # Restrict the drawing style to points or polygons.
  if (!type %in% c("pl", "p")) stop("magMWdust function expects type of 'p' (for points) or 'pl' (for polygons) only")

  # Build an equatorial sky grid with the requested spacing.
  sky <- expand.grid(seq(0 + dlon, 360 - dlon, by = dlon), seq(-90 + dlat, 90 - dlat, by = dlat))
  # Convert the equatorial grid to galactic coordinates for dust lookups.
  gal <- .eq2gal(sky[, 1], sky[, 2], eq = "B1950")
  # Query the dust map and keep the original equatorial coordinates.
  dust_all <- cbind(ra = sky[, 1], dec = sky[, 2], dust_getval(gal[, 1], gal[, 2], map = "Ebv", interp = TRUE))
  # Map dust values onto an opacity scale for plotting.
  dust_all$map <- magicaxis::magmap(dust_all$ebv, range = opacity.range, hicut = whiteblack.percentile[2], locut = whiteblack.percentile[1], stretch = stretch)$map
  # Drop grid cells that would be too faint to plot usefully.
  dust <- dust_all[which(dust_all$map > min.opacity.plot), ]

  # Draw filled polygons when polygon mode has been requested.
  if (type == "pl") {
    # Open a progress bar for the per-cell polygon loop.
    pb <- txtProgressBar(style = 3, min = 1, max = nrow(dust))
    # Iterate over each retained sky cell.
    for (i in 1:nrow(dust)) {
      # Project and draw the four corners of the current sky cell.
      magicaxis::magproj(c(dust$ra[i] - dlon/2, dust$ra[i] - dlon/2, dust$ra[i] + dlon/2, dust$ra[i] + dlon/2), c(dust$dec[i] - dlat/2, dust$dec[i] + dlat/2, dust$dec[i] + dlat/2, dust$dec[i] - dlat/2), type = type, add = TRUE, col = hsv(v = 0, alpha = dust$map[i]), ...)
      # Advance the progress bar after drawing the current polygon.
      setTxtProgressBar(pb, i)
    }
    # Close the progress bar when the polygon layer is complete.
    close(pb)
  } else {
    # Draw the retained dust grid cells as projected points.
    magicaxis::magproj(dust$ra, dust$dec, type = "p", add = TRUE, border = NA, pch = pch, cex = pt.cex, col = hsv(v = 0, alpha = dust$map), ...)
  }
  # Return invisibly because this function is used for its plotting side effects.
  return(invisible(NULL))
}

#Convert equatorial coordinates to galactic coordinates 
.eq2gal<-function(alpha,delta,equinox='J2000') { 
  if (equinox=='J2000') { 
    alpha0<-192.8595
    delta0<-27.1284
    delta0<-45
    ell0<-122.9320
  } else if (equinox=='B1950') { 
    alpha0<-192.25
    delta0<-27.40
    ell0<-123.000
  } else { 
    stop("unknown equinox provided: must be either J2000 or B1950") 
  }

  dcos<-function(theta) cos(theta*pi/180)
  dsin<-function(theta) sin(theta*pi/180)
  datan2<-function(f,x) atan2(f,x)*180/pi
  dasin<-function(f) asin(f)*180/pi

  alpha_prime=alpha-alpha0
  delta_prime=delta-delta0

  b = (dasin(dsin(delta)*dsin(delta0)+dcos(delta)*dcos(delta0)*dcos(alpha_prime)))

  calcy = (dcos(delta)*dsin(alpha_prime))
  calcx = (dsin(delta)*dcos(delta0)-dcos(delta)*dsin(delta0)*dcos(alpha_prime))

  ell = -datan2(calcy,calcx)+ell0

  ell[which(ell<0)]<-ell[which(ell<0)]+360
  ell[which(ell>360)]<-ell[which(ell>360)]-360

  return=cbind(ell,b)

}

