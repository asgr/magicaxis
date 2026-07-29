#=========================================
#
# File Name : magMWdust.R
# Created By : awright
# Creation Date : 03-06-2026
# Last Modified : Wed Jun  3 13:40:13 2026
#
#=========================================

utils::globalVariables("SFD_dust")

# Define a plotting helper that overlays Milky Way dust on a projected sky plot.
magMWdust = function(dust.data = NULL, dlon = NULL, dlat = NULL, type = "p", pch = 16, pt.cex = 0.5, opacity.range = c(0, 0.5), whiteblack.percentile = c(0.5, 0.95), stretch = "lin", min.opacity.plot = 0.01, show.status = TRUE, ...) {

  # Restrict the drawing style to points or polygons.
  if (!type %in% c("pl", "p")) stop("magMWdust function expects type of 'p' (for points) or 'pl' (for polygons) only")
  # If none provided, read the dust map data, which is a dlon=dlat=1 sampling  
  if (is.null(dust.data)) { 
    # Define the dlon and dlat values
    dlon = dlat = 1
    # Lazy load the SFD_dust data
    dust_all = SFD_dust
  } else { 
    if (!is.data.frame(dust.data)) {
      stop("dust.data is not a data frame; load an example with data(SFD_dust)") 
    }
    if (!all(c("ebv","ra","dec")%in%colnames(dust.data))) {
      stop("dust.data is missing required components; load an example with data(SFD_dust)") 
    }
    dust_all=dust.data 
    if (type=='pl') { 
      if (is.null(dlon)) stop("dlon must be provided when providing input dust.data and using type == 'pl'") 
      if (is.null(dlat)) stop("dlat must be provided when providing input dust.data and using type == 'pl'") 
      if (!is.numeric(dlon)) stop("dlon must be numeric") 
      if (!is.numeric(dlat)) stop("dlat must be numeric") 
    }
  }
  # Map dust values onto an opacity scale for plotting.
  dust_all$map = magicaxis::magmap(dust_all$ebv, range = opacity.range, hicut = whiteblack.percentile[2], locut = whiteblack.percentile[1], stretch = stretch)$map
  # Drop grid cells that would be too faint to plot usefully.
  dust = dust_all[which(dust_all$map > min.opacity.plot), ]
  # Stop on empty result 
  if (nrow(dust)==0) stop("threshold for min.opacity.plot causes no data to be plotted. Reduce value to produce a result")

  # Draw filled polygons when polygon mode has been requested.
  if (type == "pl") {
    # Open a progress bar for the per-cell polygon loop.
    if (interactive() & isTRUE(show.status)) { 
      pb = txtProgressBar(style = 3, min = 1, max = nrow(dust))
    }
    # Iterate over each retained sky cell.
    for (i in 1:nrow(dust)) {
      # Project and draw the four corners of the current sky cell.
      magicaxis::magproj(c(dust$ra[i]  - dlon/2, dust$ra[i]  - dlon/2, dust$ra[i]  + dlon/2, dust$ra[i]  + dlon/2), 
                         c(dust$dec[i] - dlat/2, dust$dec[i] + dlat/2, dust$dec[i] + dlat/2, dust$dec[i] - dlat/2), 
                         type = type, add = TRUE, col = hsv(v = 0, alpha = dust$map[i]), border=NA, ...)
      # Advance the progress bar after drawing the current polygon.
      if (interactive() & isTRUE(show.status)) {
        setTxtProgressBar(pb, i)
      } 
    }
    # Close the progress bar when the polygon layer is complete.
    if (interactive() & isTRUE(show.status)) {
      close(pb)
    }
  } else {
    # Draw the retained dust grid cells as projected points.
    magicaxis::magproj(dust$ra, dust$dec, type = "p", add = TRUE, pch = pch, cex = pt.cex, col = hsv(v = 0, alpha = dust$map), ...)
  }
  # Return invisibly because this function is used for its plotting side effects.
  return(invisible(NULL))
}
