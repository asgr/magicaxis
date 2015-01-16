magimage<-function(mat, astr=NULL, main="", xlab=ifelse(is.null(astr),"X (pix)","RA (deg)"), ylab=ifelse(is.null(astr),"Y (pix)","Dec (deg)"), side=1:4,labels=c(T,T,F,F),...) {
  if (!is.null(astr)) {
    lims<-.xy2ad(c(1,length(mat[,1])),c(1,length(mat[1,])),astr)
    asp<-1/cos(mean(lims[c(3,4)], na.rm=TRUE)*pi/180)
  } else {
    lims<-cbind(c(1,length(mat[,1])),c(1,length(mat[1,])))
    asp<-1
  }
  plot.new()
  map<-magmap(mat,range=c(0,1),...)$map
  col<-rgb(map,map,map)
  ncol=dim(mat)[2]
  nrow=dim(mat)[1]
  plot.window(xlim=lims[,1],ylim=lims[,2],asp=asp)
  image(x=seq(min(lims[,1]),max(lims[,1]),length=ncol),
        y=seq(min(lims[,2]),max(lims[,2]),length=nrow),
  z=matrix(1:length(mat), ncol=ncol)[nrow:1,],col=col, axes=FALSE, add=TRUE, useRaster=TRUE)
  magaxis(main=main, xlab=xlab,ylab=ylab,labels=labels,side=side)
}
.xy2ad <-
function(x,y,astr) {
  # Converts x/y (pixels) to RA/DEC (degrees) position using the TAN Gnomonic projection system
  # point of interest x & y, anchor point RA & DEC, anchor point x & y, x & y scale (degrees per pixel)
  # FROM: http://mathworld.wolfram.com/GnomonicProjection.html
  attach(astr)
  ra0=ra0*(pi/180)
  dec0=dec0*(pi/180)
  xscale=xscale*(pi/180)
  yscale=yscale*(pi/180)
  x = (x-x0)*tan(xscale)
  y = (y-y0)*tan(yscale)
  xra = function(ra0,dec0,x,y){
      ra0 + atan2(x*sin(atan(sqrt(x^2+y^2))),sqrt(x^2+y^2)*cos(dec0)*cos(atan(sqrt(x^2+y^2))) - y*sin(dec0)*sin(atan(sqrt(x^2+y^2))))
  }
  ydec = function(dec0,x,y){
      asin(cos(atan(sqrt(x^2+y^2)))*sin(dec0) + (y*sin(atan(sqrt(x^2+y^2)))*cos(dec0) / sqrt(x^2+y^2)))
  }
  RA = xra(ra0,dec0,x,y)*180/pi
  DEC = ydec(dec0,x,y)*180/pi
  DEC[which(is.nan(DEC))] = dec0*180/pi
  return=cbind(RA,DEC)
}
