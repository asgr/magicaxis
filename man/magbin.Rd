\name{magbin}
\alias{magbin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
2D Binning Routines
}
\description{
Allows for 2D binning (counts) and summary statistics on 2D bins (medians etc).
}
\usage{
magbin(x, y, z = NULL, xlim = NULL, ylim = NULL, step = c(diff(xlim)/50,
diff(ylim)/50), clustering = 10, dustlim = 0.1, shape = "hex",
exactcount = FALSE, plot = TRUE, colramp = terrain.colors(10000),
colstretch = "lin", sizestretch = "log", colref = "count", sizeref = "none",
funstat = median, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric vector or matrix/data.frame; x values to 2D bin. If x is a two (or more) column matrix or data.frame and y/z is missing as an argument, then the first column is used for x and the second/third column for y/z.
}
  \item{y}{
Numeric vector; the y coordinates of points in the plot, optional if \option{x} is an appropriate structure.
}
  \item{z}{
Numeric vector; the z coordinates of points in the plot, optional if \option{x} is an appropriate structure.
}
  \item{xlim}{
Numeric vector; the x limits to use for the data. Default of NULL calculates the range based on the provided \option{x} data vector.
}
  \item{ylim}{
Numeric vector; the y limits to use for the data. Default of NULL calculates the range based on the provided \option{y} data vector.
}
  \item{step}{
Numeric vector; grid steps in x and y directions. If left at
}
  \item{clustering}{
Numeric scalar; excess counts in densist bin relative to Uniform data. This is to optimise the binning, so can probably be ignored.
}
  \item{dustlim}{
Numeric scalar; if between 0 and 1 then the 2D bin count quantile to switch to showing the individual points (which visually look like 'dust'), if larger than 1 then the exact counts threshold. If this is NA or 0 then all cells are shown.
}
  \item{shape}{
Character scalar; type of binning, either hex/hexagon; sq/square or tri/triangle.
}
  \item{exactcount}{
Logical; do the bins need to represent the exact bin counts. If set to TRUE then code is much slower, so visually the approximate counts are nearly always good enough.
}
  \item{plot}{
Logical; create a plot? If FALSE then just the binning output list is created.
}
  \item{colramp}{
Vector; a colour scaling to use. Must be a vector and not a function.
}
  \item{colstretch}{
Character scalar; colour stretch, either linear (lin, default) or logarithmic (log).
}
  \item{sizestretch}{
Character scalar; size stretch, either linear (lin) or logarithmic (log, default).
}
  \item{colref}{
Character scalar; colour reference for call, either it should be based on the counts (count, default) or the z-axis statistic (zstat)?
}
  \item{sizeref}{
Character scalar; colour reference for call, either it should be ignored (none, so all are the same size and closely packed), based on the counts (count) or the z-axis statistic (zstat)?
}
  \item{funstat}{
Function; function to use to compute a statistic over the \option{z} axis.
}
  \item{\dots}{
Dots to be passed to \code{\link{magplot}} and \code{\link{magmap}} for colouring bin mapping only.
}
}
\details{
Mostly run for the side effect of making a nice plot, but the output bin statistics might also be useful.
}
\value{
List of class 'magbin' containing:
\item{bins }{Bin x / y / count / and zstat info}
\item{dust }{Dust x / y / z info}
\item{groups}{Links input \option{x} and \option{y} data to the nearest grid cell by row number of \option{bins}}
\item{xlim }{x limits}
\item{ylim }{y limits}
\item{step }{\option{step} size}
\item{dustlim}{\option{dustlim}}
\item{shape}{\option{shape}}
}

\seealso{
\code{\link{plot.magbin}}, \code{\link{maghist}}
}
\examples{
xydata = cbind(rnorm(1e4), rnorm(1e4))
magbin(xydata, shape='hexagon') #default
magbin(xydata, shape='square')
magbin(xydata, shape='triangle')

magbin(xydata, z=xydata[,1]^2-xydata[,2]^2, colref='zstat', sizeref='count')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hist }% use one of  RShowDoc("KEYWORDS")
\keyword{ histogram }% __ONLY ONE__ keyword per line
