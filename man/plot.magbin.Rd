\name{plot.magbin}
\alias{plot.magbin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot 2D Histogram or Bin Data
}
\description{
Plot 2D histogram counts or a third (z) axis summary statistic in 2D cells of different shapes.
}
\usage{
\method{plot}{magbin}(x, colramp = terrain.colors(10000), colstretch = "lin",
  sizestretch = "log", colref = "count", sizeref = "none", add = FALSE,
  dobar = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class 'magbin'.
}
  \item{colramp}{
Vector; a colour scaling to use. Must be a vector and not a function.
}
  \item{colstretch}{
Character scalar; colour stretch, either linear (lin) or logarithmic (log).
}
  \item{sizestretch}{
Character scalar; size stretch, either linear (lin) or logarithmic (log).
}
  \item{colref}{
Character scalar; colour reference for call, either it should be based on the counts (count) or the z-axis statistic (zstat)?
}
  \item{sizeref}{
Character scalar; colour reference for call, either it should be ignored (none, so all are the same size and closely packed), based on the counts (count) or the z-axis statistic (zstat)?
}
  \item{add}{
Logical; should bins be added to the current plot? If FALSE then a new plot will be made.  
}
  \item{dobar}{
Logical; should a colour bar be added to the plot?  
}
  \item{\dots}{
Dots to be passed to \code{\link{magplot}} and \code{\link{magmap}} for colouring bin mapping only.
}
}
\value{
Run for the side effect of making a nice plot.
}
\author{
Aaron Robotham
}

\seealso{
\code{\link{magbin}}
}
\examples{
temp = magbin(rnorm(1e4), rnorm(1e4), plot=FALSE)
plot(temp, xlab='x', ylab='y')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hist }% use one of  RShowDoc("KEYWORDS")
\keyword{ histogram }% __ONLY ONE__ keyword per line
