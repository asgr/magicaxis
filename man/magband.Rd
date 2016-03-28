\name{magprojextra}
\alias{magprojextra}
\alias{magband}
\alias{magring}
\alias{magMW}
\alias{magsun}
\alias{magMWplane}
\alias{magecliptic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Attractive great circles and thick bands on magproj plots
}
\description{
High level functions to add great circles and thick bands on projections plots. In astronomy these are popular for indicating regions of exclusion surrounding the ecliptic or the Milky-Way plane. Also simple functions to add either the MW bluge to the current projection (magMW) or the sun on a given date (magsun).
}
\usage{
magring(crosseq = 0, peaklat = 0, offset = 0, res = 1000, ...)
magband(crosseq = 0, peaklat = 0, width = 10, res = 1000, ...)
magecliptic(width=10, ...)
magMWplane(width=10, ...)
magsun(Ydate='get', anti=FALSE, ...)
magMW(...)
}
\arguments{
  \item{crosseq}{
The longitude below the latitude peak of the great circle (or centre of the band) where the centre crosses the equator. See examples to see how this is used in practice.
}
  \item{peaklat}{
The positive maximum latitude obtained by the great circle (or centre of the band). See examples to see how this is used in practice.
}
  \item{offset}{
Whether the ring drawn if systematically offset from the great cirlce defined by \option{crosseq} and \option{peaklat}. Leave at 0 to draw a great circle.
}
  \item{width}{
How wide whould the band be in degrees. For \code{magecliptic} and \code{magMWplane}, if this is zero it will draw a line instead.
}
  \item{res}{
Number of elements making up each side of the band (default should be fine for most plots).
}
  \item{Ydate}{
The date for the location of the Sun on the spherical grid. Vector in c(M,D) format. If set to 'get' then the function will return the Sun's location for today.  
}
  \item{anti}{
Should the anti-sun position be computed (i.e. the RA and Dec of the position diametrically opposed to the Sun).  
}
  \item{\dots}{
Arguments passed on to \code{\link{lines}} (\code{makering}), \code{\link{polygon}} (\code{makeband}), \code{\link{points}} (\code{makeMW} and \code{makesun}).
}
}
\value{
  No output. Run for the side effect of producing nice projected plots.
}
\author{
  Aaron Robotham
}
\seealso{
\code{\link{magplot}}, \code{\link{magaxis}}, \code{\link{maglab}}, \code{\link{magmap}}, \code{\link{magrun}}, \code{\link{magbar}}, \code{\link{magproj}}
}
\examples{
# GAMA fields:
par(mar=c(0.1,0.1,0.1,0.1))
magproj(c(129,141), c(-2,3), type='b', projection='aitoff', centre=c(180,0),
fliplong=TRUE, labloc=c(90,-45), col='red', labeltype = 'sex', crunch=TRUE)
magproj(c(211.5,223.5), c(-2,3), col='red', add=TRUE)
magproj(c(30.2,38.8), c(-10.25,-3.72), col='red', add=TRUE)
magproj(c(30.2,38.8), -6, type='l', add=TRUE, col='grey')
magproj(c(339,351), c(-35,-30), col='red', add=TRUE)

magecliptic(width=10,col=hsv(1/12,alpha=0.3),border=NA)
magecliptic(width=0,col='orange')
# Note this a shortcut for: magring(0,23.4,col='orange')
magMWplane(width=20,col=hsv(v=0,alpha=0.1),border=NA)
magMWplane(width=0,col='darkgrey')
# Note this a shortcut for: magring(76.75,62.6,col='darkgrey')
magMW(pch=16, cex=2, col='darkgrey')
magsun(c(7,26), pch=16, cex=2, col='orange2') #An important date!

magproj(c(174,186), c(-3,2), col='red', add=TRUE)

#Plus SDSS:
magproj(c(110,260), c(-4,70), border='blue', add=TRUE)

magproj(c(35,135,180,217.5,345), c(-3.72,3,2,3,-30)+10, type='t',
plottext=c('G02','G09','G12','G15','G23'), add=TRUE)

legend('topleft', legend=c('GAMA Regions','SDSS Main Survey'), col=c('red','blue'),
pch=c(15,NA), lty=c(NA,1), bty='n')
legend('topright', legend=c('Ecliptic','MW Plane'), col=c(hsv(c(1/12,0), v=c(1,0),
alpha=0.5)), pch=c(15,15), lty=c(1,1), bty='n')
legend('bottomleft', legend=c('Sun', 'MW Centre'), col=c('orange2','darkgrey'), pch=16,
bty='n')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ projection }
\keyword{ map }% __ONLY ONE__ keyword per line
