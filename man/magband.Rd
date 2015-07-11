\name{magprojextra}
\alias{magband}
\alias{magring}
\alias{magMW}
\alias{magsun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Attractive great circles and thick bands on magproj plots
}
\description{
High level functions to add great circles and thick bands on projections plots. In astronomy these are popular for indicating regions of exclusion surrounding the ecliptic or the Milky-Way plane. Also simple functions to add either the MW bluge to the current projection (magMW) or the sun on a given date (magsun).
}
\usage{
magring(crosseq = 0, peaklat = 0, longlo = -180, offset = 0, res = 1000, ...)
magband(crosseq = 0, peaklat = 0, longlo = -180, width = 10, res = 1000, ...)
magMW(...)
magsun(Ydate='get', ...)
}
\arguments{
  \item{crosseq}{
The longitude below the latitude peak of the great circle (or centre of the band) where the centre crosses the equator. See examples to see how this is used in practice.
}
  \item{peaklat}{
The positive maximum latitude obtained by the great circle (or centre of the band). See examples to see how this is used in practice.
}
  \item{longlo}{
The low longitude limit of the projection. Users will need to set this carefully to get sensible plots.
}
  \item{offset}{
Whether the ring drawn if systematically offset from the great cirlce defined by \option{crosseq} and \option{peaklat}. Leave at 0 to draw a great circle.
}
  \item{width}{
How wide whould the band be in degrees.
}
  \item{res}{
Number of elements making up each side of the band (default should be fine for most plots).
}
  \item{Ydate}{
The date for the location of the Sun on the spherical grid. Vector in c(M,D) format. If set to 'get' then the function will return the Sun's location for today.  
}
  \item{\dots}{
Arguments passed on to \code{\link{lines}} (\code{makering}), \code{\link{polygon}} (\code{makeband}), \code{\link{points}} (\code{makeMW} and \code{makesun}).
}
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{magplot}},\code{\link{magaxis}},\code{\link{maglab}},\code{\link{magmap}},\code{\link{magrun}},\code{\link{magbar}},\code{\link{magproj}}
}
\examples{
magproj(c(129,141), c(-2,3), type='b', projection='aitoff', centre=c(180,0),
fliplong=TRUE, labloc=c(90,-45), col='lightgreen', labeltype = 'sex', crunch=TRUE)
magproj(c(211.5,223.5), c(-2,3), col='lightgreen', add=TRUE)
magproj(c(30.2,38.8), c(-10.25,-3.72), col='lightgreen', add=TRUE)
magproj(c(30.2,38.8), c(-6,-3.72), col='lightgreen', add=TRUE, lty=2)
magproj(c(339,351), c(-35,-30), col='lightgreen', add=TRUE)

magband(0,23.4,width=10,longlo=0,col=hsv(1/12,alpha=0.3),border=NA)
magband(76.75,62.6,width=20,longlo=0,col=hsv(5/6,alpha=0.3),border=NA)
magring(0,23.4,longlo=0,col='orange')
magring(76.75,62.6,longlo=0,col='purple')
magMW(pch=16, cex=2, col='purple3')
magsun(c(7,26), pch=16, cex=2, col='orange2')

magproj(c(174,186), c(-3,2), col='lightgreen', add=TRUE)

magproj(c(35,135,180,217.5,345), c(-3.72,3,2,3,-30)+10, type='t',
plottext=c('G02','G09','G12','G15','G23'), add=TRUE)

legend('topleft', legend=c('GAMA Regions'), col='lightgreen', pch=15, bty='n')
legend('topright', legend=c('Ecliptic','MW Plane'), col=c(hsv(c(1/12,5/6),
alpha=0.5)), pch=c(15,15), lty=c(1,1), bty='n')
legend('bottomleft', legend=c('Sun', 'MW Centre'), col=c('orange2','purple3'), pch=16, bty='n')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ projection }
\keyword{ map }% __ONLY ONE__ keyword per line
