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
magbin(x, y, z = NULL, xlim = NULL, ylim = NULL, zlim = NULL, step = NULL,
log = '', unlog = log, clustering = 10, dustlim = 0.1, shape = "hex",
plot = TRUE, colramp = hcl.colors(1e4), colstretch = "lin", sizestretch = "lin",
colref = "count", sizeref = "none", funstat = function(x) median(x, na.rm=TRUE),
direction = 'h', offset = 0, jitterseed = 666, projden = FALSE, ...)
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
Numeric vector; the z coordinates of points in the plot (optional), optional if \option{x} is an appropriate structure.
}
  \item{xlim}{
Numeric vector; the x limits to use for the data. Default of NULL calculates the range based on the provided \option{x} data vector. If length equals 1 then the argument is taken to mean the sigma range to select for plotting and the clipping is done by \code{\link{magclip}}.
}
  \item{ylim}{
Numeric vector; the y limits to use for the data. Default of NULL calculates the range based on the provided \option{y} data vector. If length equals 1 then the argument is taken to mean the sigma range to select for plotting and the clipping is done by \code{\link{magclip}}.
}
  \item{zlim}{
Numeric vector; the z limits to use for the data. Default of NULL calculates the range based on the provided \option{y} data vector. If length equals 1 then the argument is taken to mean the sigma range to select for plotting and the clipping is done by \code{\link{magclip}}.
}
  \item{step}{
Numeric vector; grid steps in x and y directions. IF NULL then this is c(diff(xlim)/50, diff(ylim)/50). If length 1, then this value is repeated.
}
  \item{log}{
Character scalar; log axis arguments to be passed to used. E.g. use 'x', 'y', 'xy' or 'yx' as appropriate. Default '' assumes no logging of any axes. For convenience you can specify the 'z' axis too, which somewhat replaces the \option{colstretch} argument. Note that in all cases the \option{x}/\option{y}/\option{z} data is explictly logged, which means the plotting window does not know it is in logged space (via the par()$xlag and par()$ylog structures). This means is you want to add points etc to the plot you will need to apply log10 yourself, so a point with coordinates [10^2,10^-3] should be plotted at [2,-3].
}
  \item{unlog}{
Character scalar; determines if x/y axis labels should be unlogged (z is ignored here). By default inherits \option{log}, since that is usually what you would want.
}
  \item{clustering}{
Numeric scalar; excess counts in densist bin relative to Uniform data. This is to optimise the binning, so can probably be ignored.
}
  \item{dustlim}{
Numeric scalar; if between 0 and 1 then the 2D bin count quantile to switch to showing the individual points (which visually look like 'dust'), if larger than 1 then the exact counts threshold. If this is NA or 0 then all cells are shown.
}
  \item{shape}{
Character scalar; type of binning, either hex/hexagon; sq/square; tri/triangle or trihex. 'trihex' is a triangle tessellation that is also arranged to have hexagonal packing (so 6 triangles can form a hexagon).
}
  \item{plot}{
Logical; create a plot? If FALSE then just the binning output list is created.
}
  \item{colramp}{
Vector; a colour scaling to use. Must be a vector and not a function.
}
  \item{colstretch}{
Character scalar; colour stretch, either linear (lin, default) or logarithmic (log, good for large dynamic ranges).
}
  \item{sizestretch}{
Character scalar; size stretch, either linear (lin, default) or logarithmic (log, good for large dynamic ranges).
}
  \item{colref}{
Character scalar; colour reference for call, either it should be based on the counts (count, default) or the z-axis statistic (zstat)?
}
  \item{sizeref}{
Character scalar; colour reference for call, either it should be ignored (none, so all are the same size and closely packed), based on the counts (count) or the z-axis statistic (zstat)?
}
  \item{funstat}{
Function; function to use to compute a statistic over the \option{z} axis. The default is \code{\link{median}}, but other good options might be \code{\link{mean}}, \code{\link{sd}}, \code{\link{mad}}. Note, to change default arguments you might need to send through a new function, e.g. \option{funstat} = function(x) median(x, na.rm=TRUE) and similar, but if you are happy with the defaults then you can use the simpler \option{funstat} = mean etc.
}
  \item{direction}{
Character scalar;  should there be a shape side aligned horizontally ('h', the default) or vertically ('v')? This is only relevant for hexagon and triangle bin shapes, and has the effect of leading the eye differently with some scatter structures.
}
  \item{offset}{
Numeric/character scalar; only relevant for \option{shape}='sq' or \option{shape}='tri'. Either a numeric value specifying the offset (relative to \option{step}) to apply to alternating rows (\option{direction}='h') or columns (\option{direction}='v'); or 'jitter' which means the rows or columns are randomly jittered (only used for \option{shape}='sq' bins. This option is useful for visually breaking up strong patterns in certain types of data.
}
  \item{jitterseed}{
Integer scalar; the random seed to use for jittering (means you can recreate your plots exactly if the seed is the same). This argument is only used for \option{shape}='sq' bins.
}
  \item{projden}{
Logical; do you want projected density PDFs to be displayed above and to the side of the standard \code{plot.magbin} plot? If so you also need to pass the same \option{xdata} and \option{ydata} that you originally sent to \code{\link{magbin}}, since this is not stored in the object output from \code{\link{magbin}}. 
}
  \item{\dots}{
Dots to be passed to \code{\link{magplot}}, \code{\link{magmap}} and \code{\link{magmap}}. Relevant arguments are matched, so look in those functions for optional arguments to pass.
}
}
\details{
Mostly run for the side effect of making a nice plot, but the output bin statistics might also be useful.

Re performance, \code{magbin} works pretty well on a modern computer for up to ~1e6 points, taking only a few seconds to run usually. Beyond this you might need to carefully tune the performance with \option{clutering} otherwise it might run very slower and/or you run out of memory.
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
set.seed(666)
xydata = cbind(rnorm(1e4), rnorm(1e4))
magbin(xydata, shape='hexagon') #default
magbin(xydata, shape='square')
magbin(xydata, shape='triangle')
magbin(xydata, shape='trihex')
magbin(xydata, shape='hexagon', direction='v')
magbin(xydata, shape='triangle', direction='v')
magbin(xydata, shape='trihex', direction='v')
magbin(xydata, shape='hexagon', step=c(0.2,0.4)) #different aspect ratio hexagons

magbin(xydata, z=xydata[,1]^2-xydata[,2]^2, colref='zstat', sizeref='count')

magbin(xydata, z=xydata[,1]^2-xydata[,2]^2, colref='zstat', sizeref='count',
funstat=mad)
magbin(xydata, z=xydata[,1]^2-xydata[,2]^2, colref='zstat', sizeref='count',
funstat=function(x){quantile(x,0.9)})

xydata = cbind(10^rnorm(1e4), 10^rnorm(1e4))
magbin(xydata, log='xy')
magbin(xydata, z=xydata[,1]*xydata[,2], colref='zstat', sizeref='count',
log='xyz')
magbin(xydata, log='xy', unlog='xy', xlim=3, ylim=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hist }% use one of  RShowDoc("KEYWORDS")
\keyword{ histogram }% __ONLY ONE__ keyword per line
