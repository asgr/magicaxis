magcutout=function(image, loc = dim(image)/2, box = c(100, 100), shiftloc=FALSE, paddim=TRUE,
                   padval=NA, plot = FALSE, ...){
  
  if(length(image)==1){
    if(is.character(image) & 'fst' %in% .packages()){
      image = fst::fst(image)
    }else{
      stop('Target image must be fst file!')
    }
  }
  
  loc = as.numeric(loc)
  xcen = loc[1]
  ycen = loc[2]
  loc = ceiling(loc)
  box = ceiling(box)
  if(length(box)==1){box=rep(box,2)}
  xlo = ceiling(loc[1] - (box[1]/2 - 0.5))
  xhi = ceiling(loc[1] + (box[1]/2 - 0.5))
  ylo = ceiling(loc[2] - (box[2]/2 - 0.5))
  yhi = ceiling(loc[2] + (box[2]/2 - 0.5))
  
  loc.diff = c(x=xlo-1, y=ylo-1)
  
  expand = paddim && shiftloc
  diffxlo = xlo - 1
  if (diffxlo < 0) {
    xlo = 1
    if(expand) xhi = xlo + (box[1] - 1)
  }
  diffxhi = xhi - dim(image)[1]
  if (diffxhi > 0) {
    xhi = dim(image)[1]
    if(expand) {
	    xlo = xlo - diffxhi
	    if(xlo < 1) xlo = 1
    }
  }
  diffylo = ylo - 1
  if (diffylo < 0) {
    ylo = 1
    if(expand) yhi = ylo + (box[2] - 1)
  }
  diffyhi = yhi - dim(image)[2]
  if (diffyhi > 0) {
    yhi = dim(image)[2]
    if(expand) {
	    ylo = ylo - diffyhi
	    if(ylo < 1) ylo = 1
    }
  }
  if(!paddim && !shiftloc) {
  	if(diffxlo < 0 && (-diffxlo > diffxhi)) xhi = xhi - max(diffxhi,0) + diffxlo
  	if(diffxhi > 0 && (-diffxlo < diffxhi)) xlo = xlo + diffxhi - min(diffxlo,0)
  	if(diffylo < 0 && (-diffylo > diffyhi)) yhi = yhi - max(diffyhi,0) + diffylo
  	if(diffyhi > 0 && (-diffylo < diffyhi)) ylo = ylo + diffyhi - min(diffylo,0)
  }
  xsel = as.integer(xlo:xhi)
  ysel = as.integer(ylo:yhi)
  xsel=xsel[xsel>0]
  ysel=ysel[ysel>0]
  
  if(length(xsel)==0 | length(ysel)==0){
    image=matrix(padval,box[1],box[2])
  }else{
    image = as.matrix(image[xsel, ysel])
    if(paddim && !shiftloc && any(c(diffxlo,-diffxhi,diffylo,-diffyhi) < 0)) {
    	padded = matrix(padval,box[1],box[2])
    	padded[xsel-diffxlo,ysel-diffylo] = image
    	image = padded
    }
  }
  
  if(paddim & shiftloc==FALSE){
    loc=c(x=xcen-diffxlo,y=ycen-diffylo)
  }else{
    loc=c(x=xcen-xlo+1, y=ycen-ylo+1)
  }
  loc.orig=c(x=xcen, y=ycen)
  loc.diff=c(x=loc.orig[1]-loc[1], y=loc.orig[2]-loc[2])
  
  output = list(image = image, loc=loc, loc.orig=loc.orig, loc.diff=loc.diff, xsel=xsel, ysel=ysel)
  if (plot) {
    if(all(is.na(image))){
      image[]=0
      magimage(image, ...)
    }else{
      magimage(image, ...)
    }
  }
  invisible(output)
}
