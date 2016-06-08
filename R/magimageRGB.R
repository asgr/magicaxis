magimageRGB<-function(x, y, R, G, B, zlim, xlim, ylim, add = FALSE, useRaster=TRUE, asp=1, magmap=TRUE, lo=0.2, hi=0.995, flip=FALSE, range=c(0,1), type = "quan", stretch="asinh", stretchscale='auto', bad=range[1], clip="", axes=TRUE, frame.plot=TRUE, ...) {
  if(!missing(x)){
    if(is.list(x)){
      if('y' %in% names(x)){y=x$y}
      if('z' %in% names(x)){z=x$z}
      if('R' %in% names(x)){R=x$R}
      if('G' %in% names(x)){R=x$G}
      if('B' %in% names(x)){R=x$B}
    }
    if(is.array(x)){
      R=x[,,1]
      G=x[,,2]
      B=x[,,3]
      x=seq(0.5,dim(R)[1]-0.5)
      if(missing(y)){y=seq(0.5,dim(R)[2]-0.5)}
    }
  }
  if(!missing(R)){
    if(is.matrix(R)){
      if(missing(x)){x=seq(0.5,dim(R)[1]-0.5)}
      if(missing(y)){y=seq(0.5,dim(R)[2]-0.5)}
    }
  }
  if(missing(xlim) & length(x)==dim(R)[1]){
    xlim=range(x, na.rm=TRUE)+c(-0.5,0.5)*diff(range(x, na.rm=TRUE))/(dim(R)[1]-1)
  }
  if(missing(ylim) & length(y)==dim(R)[2]){
    ylim=range(y, na.rm=TRUE)+c(-0.5,0.5)*diff(range(y, na.rm=TRUE))/(dim(R)[2]-1)
  }
  if(missing(xlim) & length(x)==(dim(R)[1]+1)){
    xlim=range(x, na.rm=TRUE)
  }
  if(missing(ylim) & length(y)==(dim(R)[2]+1)){
    ylim=range(y, na.rm=TRUE)
  }
  if(x[1]>x[length(x)]){x=rev(x); xlim=rev(xlim)}
  if(y[1]>y[length(y)]){y=rev(y); ylim=rev(ylim)}
  if(magmap){
    R=magmap(data=R, lo=lo, hi=hi, flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale, bad=bad, clip=clip)$map
    G=magmap(data=G, lo=lo, hi=hi, flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale, bad=bad, clip=clip)$map
    B=magmap(data=B, lo=lo, hi=hi, flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale, bad=bad, clip=clip)$map
  }
  image(x=x, y=y, z=matrix(1:length(R),dim(R)[1]), xlim=xlim, ylim=ylim, col=rgb(R,G,B), add=add, useRaster=useRaster, axes=FALSE, asp=asp, xlab='', ylab='', main='')
  if(add==FALSE){
    if(axes){
      magaxis(...)
      if(frame.plot){box()}
    }
  }
  return=list(x=x, y=y, R=R, G=G, B=B)
}
