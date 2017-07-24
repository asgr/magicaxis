magimage<-function(x, y, z, zlim, xlim, ylim, col = grey((0:1e3)/1e3), add = FALSE, useRaster=TRUE, asp=1, magmap=TRUE, lo=0.4, hi=0.995, flip=FALSE, range=c(0,1), type = "quan", stretch="asinh", stretchscale='auto', bad=NA, clip="", axes=TRUE, frame.plot=TRUE, sparse='auto', ...) {
  if(!missing(x)){
    if(is.list(x)){
      if('y' %in% names(x)){y=x$y}
      if('z' %in% names(x)){z=x$z}
      if('x' %in% names(x)){x=x$x}
    }
    if(is.matrix(x)){
      z=x
      x=seq(0.5,dim(z)[1]-0.5)
      if(missing(y)){y=seq(0.5,dim(z)[2]-0.5)}
    }
  }
  if(!missing(z)){
    if(is.matrix(z)){
      if(missing(x)){x=seq(0.5,dim(z)[1]-0.5)}
      if(missing(y)){y=seq(0.5,dim(z)[2]-0.5)}
    }
  }
  if(missing(xlim) & length(x)==dim(z)[1]){
    xlim=range(x, na.rm=TRUE)+c(-0.5,0.5)*diff(range(x, na.rm=TRUE))/(dim(z)[1]-1)
  }
  if(missing(ylim) & length(y)==dim(z)[2]){
    ylim=range(y, na.rm=TRUE)+c(-0.5,0.5)*diff(range(y, na.rm=TRUE))/(dim(z)[2]-1)
  }
  if(missing(xlim) & length(x)==(dim(z)[1]+1)){
    xlim=range(x, na.rm=TRUE)
  }
  if(missing(ylim) & length(y)==(dim(z)[2]+1)){
    ylim=range(y, na.rm=TRUE)
  }
  if(x[1]>x[length(x)]){x=rev(x); xlim=rev(xlim)}
  if(y[1]>y[length(y)]){y=rev(y); ylim=rev(ylim)}
  if(sparse=='auto'){
    sparse=ceiling(max(dim(z)/1e3))
  }
  if(sparse>1){
    samplex=seq(sparse/2,length(x),by=sparse)
    sampley=seq(sparse/2,length(y),by=sparse)
    x=x[samplex]
    y=y[sampley]
    z=z[samplex,sampley]
  }
  if(magmap){
    if(type=='quan'){
      if(quantile(z,lo,na.rm=T) != quantile(z,hi,na.rm=T)){
        z=magmap(data=z, lo=lo, hi=hi, flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale, bad=bad, clip=clip)$map
      }else{
        print('Too many same valued pixels: turning off magmap scaling!')
      }
    }else{
      z=magmap(data=z, lo=lo, hi=hi, flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale, bad=bad, clip=clip)$map
    }  
  }
  if(missing(zlim)){
    zlim=range(z, na.rm=TRUE)
  }
  image(x=x, y=y, z=z, zlim=zlim, xlim=xlim, ylim=ylim, col=col, add=add, useRaster=useRaster, axes=FALSE, asp=asp, xlab='', ylab='', main='')
  if(add==FALSE){
    if(axes){
      magaxis(...)
      if(frame.plot){box()}
    }
  }
  return=list(x=x, y=y, z=z)
}

magcutout=function(image, loc = dim(image)/2, box = c(101, 101), shiftloc=TRUE, paddim=TRUE, plot = FALSE, ...){
  loc = as.numeric(loc)
  xcen = loc[1]
  ycen = loc[2]
  loc = ceiling(loc)
  xlo = ceiling(loc[1] - (box[1]/2 - 0.5))
  xhi = ceiling(loc[1] + (box[1]/2 - 0.5))
  ylo = ceiling(loc[2] - (box[2]/2 - 0.5))
  yhi = ceiling(loc[2] + (box[2]/2 - 0.5))
  
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
  if(!paddim && !shiftloc)
  {
  	if(diffxlo < 0 && (-diffxlo > diffxhi)) xhi = xhi - max(diffxhi,0) + diffxlo
  	if(diffxhi > 0 && (-diffxlo < diffxhi)) xlo = xlo + diffxhi - min(diffxlo,0)
  	if(diffylo < 0 && (-diffylo > diffyhi)) yhi = yhi - max(diffyhi,0) + diffylo
  	if(diffyhi > 0 && (-diffylo < diffyhi)) ylo = ylo + diffyhi - min(diffylo,0)
  }
  xsel = xlo:xhi
  ysel = ylo:yhi
  image = image[xsel, ysel]
  if(paddim && !shiftloc && any(c(diffxlo,-diffxhi,diffylo,-diffyhi) < 0)) {
  	padded = matrix(NA,box[1],box[2])
  	padded[xsel-diffxlo,ysel-diffylo] = image
  	image = padded
  }
  output = list(image = image, loc = c(x=xcen-xlo+1, y=ycen-ylo+1), loc.orig = c(x=xcen, y=ycen), loc.diff = c(x=xlo-1, y=ylo-1), xsel = xsel, ysel = ysel)
  if (plot) {
    magimage(image, ...)
  }
  return = output
}

