\name{magimage}
\alias{magimage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Magically pretty images
}
\description{
Makes image plots based on magaxis axes and magmap scaling. Particularly designed for creating images with non-linear scaling, sensible treatement of pixels with values outside the desired z-limits, and plotting of astronomical images with appropriate axes.
}
\usage{
magimage(z, x=NULL, y=NULL, main = "",
side = 1:2, labels = TRUE,
xlab = ifelse(is.null(astr),"X (pix)","RA (deg)"),
ylab = ifelse(is.null(astr),"Y (pix)","Dec (deg)"),
zlim=NULL, xlim=NULL, ylim=NULL, useRaster = TRUE, asp=NULL,
stretch='lin', stretchscale=1, lo=0, hi=1, type=NULL, flip=FALSE,
astr=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
Image matrix for plotting
}
  \item{x}{
numeric (length nx) vector; The x coordinates (pixel-edges) of z-matrix. If NULL, pixel-number is used (i.e. [1,nx]). This is different from image.plot, which by default makes images on the [0,1] interval.
}
  \item{y}{
numeric (length ny) vector; The y coordinates (pixel-edges) of z-matrix. If NULL, pixel-number is used (i.e. [1,ny]). This is different from image.plot, which by default makes images on the [0,1] interval.
}
  \item{main}{
Title for the plot. Default is no title.
}
  \item{side}{
The side to be used for axis labelling in the same sense as the base axis function (1=bottom, 2=left, 3=top, 4=right). A vector of multiple entries is allowed. By default, bottom and left axes are drawn (i.e. side 1 and 2). If side=FALSE then no sides or labels will be drawn at all.
}
  \item{labels}{
logical; Specifies whether major-axis ticks should be labelled for each axis. If length is 1 and length of side is longer than this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. Default is to label all axes.
}
  \item{xlab}{
charachter; x axis name. Default is to use "X (pix)" or "RA (deg)", depending on whether the axes are been generated using pixels or astrometry (see the astr argument below).
}
  \item{ylab}{
character; y axis name. Default is to use "Y (pix)" or "Dec (deg)", depending on whether the axes are been generated using pixels or astrometry (see the astr argument below).
}
  \item{zlim}{
numeric (length 2) vector; z axis limits. Technically, these values are the lo= and hi= values which are passed to magmap(). zlim is included so that magimage can act as a direct replacement for image, without changes to scripts being required. If zlim is not NULL, the values of zlim replace those provided as lo= and hi=. If zlim is provided, but type is NULL, type is assumed to be numeric.
}
  \item{xlim}{
numeric (length 2) vector; x axis limits.
}
  \item{ylim}{
numeric (length 2) vector; y axis limits.
}
  \item{useRaster}{
logical; use Raster when plotting image?
}
  \item{asp}{
numeric; aspect ratio to use when plotting image. If NULL, aspect ratio is:
    if astr is null: asp is -1 and determined by matrix dimensions and/or x/ylim supplied.
    if astr is not NULL: lied: asp is set assuming will be determined by the dimensions of the matrix
}
  \item{stretch}{
character; If stretch='lin' linear output mapping is used. If stretch='log' logarithmic mapping is used. If stretch='atan' atan mapping is used. If stretch='asinh' asinh mapping is used.
}
  \item{stretchscale}{
numeric; A number to multiply the data by before applying the stretch. This only has a user impact for stretch='atan' and stretch='atanh' since it controls what parts of the data is in the linear or logarithmic regime of the stretch procedure.
}
  \item{lo}{
numeric; The low limit to clip the data at (what this means varies depending on the 'type' option).
}
  \item{hi}{
numeric; The high limit to clip the data at (what this means varies depending on the 'type' option).
}
  \item{type}{
character; The type of mapping attempted. Options are 'quan' (default), 'num', 'sig' and 'rank'.
}
  \item{flip}{
logical; Should the scaling be flipped?
}
  \item{astr}{
data.frame/list; a list of astrometric parameters, used in mapping X/Y pixels to RA/Dec. The values required in this list are:
   CRVAL: The reference values for the astrometry
   CRPIX: The reference pixels for the astrometry
   CD:    The coordinate delta matrix
Astrometry is assumed to be Tan-Gnomonic without rotation.
}
  \item{\dots}{
Further arguments to be passed to base image.
}
}

\details{
This function is designed to be a nice replacement for the base image function, with added features for scaling.
}

\value{
No output. Run for the side effect of producing an image
}

\author{
Angus H Wright
}

\seealso{
\code{\link{magaxis}},\code{\link{magmap}}
}

\examples{
#Generate an image
ncol=100
nrow=100
mat<-matrix(seq(length.out=ncol)*2/ncol+rnorm(ncol*nrow),ncol,nrow)

#Set layout
layout(cbind(1,2))

#Plot base image and magimage defaults, for comparison
image(mat, main="Base Image")
magimage(mat, main="MagImage")

#Now set some plot limits
image(mat, zlim=c(0,3), main="Base Image")
magimage(mat, zlim=c(0,3), main="MagImage")

#Try different scaling
magimage(mat, main="linear Scale")
magimage(mat,stretch="asinh", main="asinh Scale")

#Different Colours
image(mat, zlim=c(0,3), col=rainbow(1E3,start=0,end=2/3), main="Base Image")
magimage(mat, zlim=c(0,3), col=rainbow(1E3,start=0,end=2/3), main="MagImage")

#Flip the color axis
magimage(mat, zlim=c(0,3), col=rainbow(1E3,start=0,end=2/3), main="Normal" )
magimage(mat, zlim=c(0,3), col=rainbow(1E3,start=0,end=2/3), flip=TRUE, main="Flipped")
# NB: This is the same as setting zlim = c(3,0)
magimage(mat, zlim=c(0,3), col=rainbow(1E3,start=0,end=2/3), flip=TRUE, main="Flipped")
magimage(mat, zlim=c(3,0), col=rainbow(1E3,start=0,end=2/3), main="zlim=c(3,0)")


#Try a non-symetric matrix and set asp=1
nrow=40
mat<-matrix(seq(length.out=ncol)*2/ncol+rnorm(ncol*nrow),ncol,nrow)
image(mat, zlim=c(0,3), col=rev(rainbow(1E3,start=0,end=2/3)), main="Base Image", asp=1)
magimage(mat, zlim=c(3,0), col=rainbow(1E3,start=0,end=2/3), main="MagImage", asp=1)

#Make an example astronometric header
astr<-list(CRPIX=c(100,100), CRVAL=c(135,-1),CD=matrix(c(1/3600,0,0,1/3600),2,2))
#Plot with and without specifying the astrometry
magimage(mat, zlim=c(3,0), col=rainbow(1E3,start=0,end=2/3), main="No Astrometry", asp=1)
magimage(mat, zlim=c(3,0), col=rainbow(1E3,start=0,end=2/3), main="With Astrometry", astr=astr)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{plot}
\keyword{image}% __ONLY ONE__ keyword per line
\keyword{axis}
