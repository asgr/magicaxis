\name{magband}
\alias{magband}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Attractive bands on magproj plots
}
\description{
A high level function to add thick bands on projections plots. In astronomy these are popular for indicating regions of exclusion surrounding the ecliptic or the Milky-Way plane.
}
\usage{
magband(crosseq = 0, peaklat = 0, longlo = -180, width = 0, res = 1000, ...)
}
\arguments{
  \item{crosseq}{
The longitude below the latitude peak of the central great circle of the band where the centre crosses the equator. See examples to see how this is used in practice.
}
  \item{peaklat}{
The positive maximum latitude obtained by the central great circle of the band. See examples to see how this is used in practice.
}
  \item{longlo}{
The low longitude limit of the projection. Users will need to set this carefully to get sensible plots.
}
  \item{width}{
How wide whould the band be in degrees.
}
  \item{res}{
Number of elements making up each side of the band (default should be fine for most plots).
}
  \item{\dots}{
Arguments passed on to \code{\link{polygon}}.
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
fliplong=TRUE, labloc=c(270,-45), col='red')
magproj(c(174,186), c(-3,2), col='green', add=TRUE)
magproj(c(211.5,223.5), c(-2,3), col='blue', add=TRUE)
magproj(c(30.2,38.8), c(-6,-3.72), col='brown', add=TRUE)
magproj(c(30.2,38.8), c(-10.25,-6), col='orange', add=TRUE)
magproj(c(339,351), c(-35,-30), col='purple', add=TRUE)

magband(0,23.4,width=5,longlo=0,col=hsv(1/6,alpha=0.3),border=NA)
magband(76.75,62.6,width=5,longlo=0,col=hsv(5/6,alpha=0.3),border=NA)

magproj(c(35,135,180,217.5,345), c(-3.72,3,2,3,-30)+10, type='t',
plottext=c('G02','G09','G12','G15','G23'), add=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ projection }
\keyword{ map }% __ONLY ONE__ keyword per line
