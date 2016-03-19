#
# Magimage Routine by AHW 2015
#
magimage<-function(z,x=NULL,y=NULL, main="", side=1:2, labels=TRUE,
xlab=ifelse(is.null(astr),"X (pix)","RA (deg)"), ylab=ifelse(is.null(astr),"Y (pix)","Dec (deg)"),
zlim=NULL,xlim=NULL,ylim=NULL,useRaster=TRUE,asp=NULL,
stretch="lin", stretchscale=1, lo=0, hi=1, type=NULL, flip=FALSE,
astr=NULL, ...) {

  #Set Plot limits {{{
  if (!is.null(astr)) {
    #Astrometry Provided; use RA/Dec for plotting limits and aspect ratio {{{
    #Generate limits of matrix in RA/Dec {{{
    lims<-.xy2ad(c(1,length(z[,1])+1),c(1,length(z[1,])+1),astr)
    #}}}
    #If Needed, get x-limits {{{
    if (is.null(xlim)) {
      xlim<-lims[c(1,2)]
    }
    #}}}
    #If Needed, get x-limits {{{
    if (is.null(ylim)) {
      ylim<-lims[c(3,4)]
    }
    #}}}
    #Make plot limits, and set aspect ratio {{{
    lims<-cbind(xlim,ylim)
    if (!is.null(asp)) { warning("asp value over-written by astrometric dimensions") }
    asp<-1/cos(mean(lims[c(3,4)], na.rm=TRUE)*pi/180)
    #}}}
    #}}}
  } else {
    #No Astrometry; use Pixels for plotting limits and aspect ratio {{{
    #If Needed, get x-limits {{{
    if (is.null(xlim)) {
      if (!is.null(x)) {
        #Use x-values, if supplied {{{
        xlim<-range(x,na.rm=TRUE)
        if (x[1]>x[length(x)]) { xlim=xlim[c(2,1)] }
        #}}}
      } else {
        #Use matrix dimensions {{{
        xlim<-c(1,nrow(z)+1)
        #}}}
      }
    }
    #}}}
    #If Needed, get y-limits {{{
    if (is.null(ylim)) {
      if (!is.null(y)) {
        #Use y-values, if supplied {{{
        ylim<-range(y,na.rm=TRUE)
        if (y[1]>y[length(y)]) { ylim=ylim[c(2,1)] }
        #}}}
      } else {
        #Use matrix dimensions {{{
        ylim<-c(1,ncol(z)+1)
        #}}}
      }
    }
    #}}}
    #Make plot limits, and set aspect ratio {{{
    lims<-cbind(xlim,ylim)
    if (is.null(asp)) { asp<- -1 }
    #}}}
    #}}}
  }
  #}}}

  #Start new plot {{{
  plot.new()
  #}}}

  #Matrix Dimensions {{{
  ncol=ncol(z)
  nrow=nrow(z)
  #}}}

  #Set colour limits {{{
  if (!is.null(zlim)) {
    #Use zlim values for lo and hi in magmap {{{
    if (any(!is.finite(zlim))) { stop("Need finite zlim values") }
    lo=min(zlim)
    hi=max(zlim)
    #If requested, force the magmap flip parameter to TRUE {{{
    if (zlim[2] < zlim[1]) { flip<-TRUE }
    #}}}
    #If not provided, set the type parameter (and warn) {{{
    if (is.null(type)) {
      warning("type parameter not set; assuming 'num'")
      type='num'
    }
    #}}}
    #}}}
  } else if (is.null(type)) {
    #Use the provided lo= and hi= values but warn if type is not set {{{
    warning("type parameter not set; assuming 'quan'")
    type='quan'
    #}}}
  }
  #}}}

  #Get mapped color values {{{
  map<-magmap(z,range=c(0,1),stretch=stretch,lo=lo,hi=hi,type=type,stretchscale=stretchscale,flip=flip,bad=ifelse(flip,1,0))$map
  map<-matrix(map,ncol=ncol,nrow=nrow,byrow=F)
  #}}}

  #Set plot window {{{
  plot.window(xlim=lims[,1],ylim=lims[,2],asp=asp)
  #}}}

  #If needed, generate x-values {{{
  if (is.null(x)) {
    #Use plot limits {{{
    x<-seq(min(lims[,1],na.rm=T),max(lims[,1],na.rm=T),length=nrow+1)
    #}}}
  } else {
    #Use supplied y-values (must be strictly increasing and equally spaced for image) {{{
    x<-seq(min(x,na.rm=T),max(x,na.rm=T),length=nrow+1)
    #}}}
  }
  #}}}

  #If needed, generate y-values {{{
  if (is.null(y)) {
    #Use plot limits {{{
    y<-seq(min(lims[,2],na.rm=T),max(lims[,2],na.rm=T),length=ncol+1)
    #}}}
  } else {
    #Use supplied y-values (must be strictly increasing and equally spaced for image) {{{
    y<-seq(min(y,na.rm=T),max(y,na.rm=T),length=ncol+1)
    #}}}
  }
  #}}}

  #Make image {{{
  image(x=x, y=y, z=map, axes=FALSE, add=TRUE, useRaster=useRaster, zlim=c(0,1), ...)
  magaxis(xlab=xlab,ylab=ylab,labels=labels,side=side)
  title(main=main)
  #}}}

  return=NULL

}
#Get RA/Dec from x,y and Astrometry {{{
.xy2ad <-
function(x,y,astr) {
  # Converts x/y (pixels) to RA/DEC (degrees) position using the TAN Gnomonic projection system
  # point of interest x & y, anchor point RA & DEC, anchor point x & y, x & y scale (degrees per pixel)
  # FROM: http://mathworld.wolfram.com/GnomonicProjection.html
  ra0=astr$CRVAL[1]*(pi/180)
  dec0=astr$CRVAL[2]*(pi/180)
  x0=astr$CRPIX[1]
  y0=astr$CRPIX[2]
  xscale=astr$CD[1,1]*(pi/180)
  yscale=astr$CD[2,2]*(pi/180)
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
#}}}
