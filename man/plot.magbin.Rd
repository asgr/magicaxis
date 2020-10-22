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
\method{plot}{magbin}(x, colramp = hcl.colors(10000), colstretch = "lin",
  sizestretch = "lin", colref = "count", sizeref = "none", add = FALSE,
  dobar = TRUE, title = colref, colnorm = FALSE, projden = FALSE, xdata = NULL, 
  ydata = NULL, pch.dust = '.', cex.dust = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class 'magbin'.
}
  \item{colramp}{
Vector; a colour palette to use. Must be a vector and not a function.
}
  \item{colstretch}{
Character scalar; colour stretch, either linear (lin, default) or logarithmic (log, good for large dynamic ranges).
}
  \item{sizestretch}{
Character scalar; size stretch, either linear (lin, default) or logarithmic (log, good for large dynamic ranges).
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
  \item{title}{
Character scalar; title to use for the \option{\link{magbar}} label.  
}
  \item{colnorm}{
Logical; should the colour bar be normalised so the maximum value equals 1?  
}
  \item{projden}{
Logical; do you want projected density PDFs to be displayed above and to the side of the standard \code{plot.magbin} plot? If so you also need to pass the same \option{xdata} and \option{ydata} that you originally sent to \code{\link{magbin}}, since this is not stored in the object output from \code{\link{magbin}}. 
}
 \item{xdata}{
Numeric vector; the original x data sent to \code{\link{magbin}}. Only relevant if \option{projden}=TRUE.
}
 \item{ydata}{
Numeric vector; the original y data sent to \code{\link{magbin}}. Only relevant if \option{projden}=TRUE.
}
  \item{pch.dust}{
Scalar; pch symbol to use for the dust points.
}
  \item{cex.dust}{
Scalar; cex size to use for the dust points.
}
  \item{\dots}{
Dots to be passed to \code{\link{magplot}}, \code{\link{magmap}} and \code{\link{magmap}}. Relevant arguments are matched, so look in those functions for optional arguments to pass.
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
