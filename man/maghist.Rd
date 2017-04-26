\name{maghist}
\alias{maghist}
\title{
Magically pretty histograms
}
\description{
A fairly simple function that produces pretty histograms. The main difference to base \code{\link{hist}} is that it allows for easy truncation of the data provided via \option{xlim}.
}
\usage{
maghist(x, breaks = "Sturges", freq = TRUE, include.lowest = TRUE, right = TRUE,
density = NULL, angle = 45, col = NULL, border = NULL, xlim = NULL, ylim = NULL,
plot = TRUE, verbose=TRUE, add=FALSE, log='', ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of values for which the histogram is desired.
}
  \item{breaks}{
One of:
\itemize{
  \item A vector giving the breakpoints between histogram cells,
  \item A function to compute the vector of breakpoints,
  \item A single number giving the number of cells for the histogram,
  \item A character string naming an algorithm to compute the number of cells,
  \item A function to compute the number of cells.
}
In the last three cases the number is a suggestion only; the breakpoints will be set to \code{\link{pretty}} values. If breaks is a function, the x vector is supplied to it as the only argument.
}
  \item{freq}{
Logical; if TRUE, the histogram graphic is a representation of frequencies, the counts component of the result; if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one). Defaults to TRUE if and only if breaks are equidistant (and probability is not specified).
}
  \item{include.lowest}{
Logical; if TRUE, an x[i] equal to the breaks value will be included in the first (or last, for right = FALSE) bar. This will be ignored (with a warning) unless breaks is a vector.
}
  \item{right}{
Logical; if TRUE, the histogram cells are right-closed (left open) intervals.
}
  \item{density}{
The density of shading lines, in lines per inch. The default value of NULL means that no shading lines are drawn. Non-positive values of density also inhibit the drawing of shading lines.
}
  \item{angle}{
The slope of shading lines, given as an angle in degrees (counter-clockwise).
}
  \item{col}{
A colour to be used to fill the bars. The default of NULL yields unfilled bars.
}
  \item{border}{
The color of the border around the bars. The default is to use the standard foreground color.
}
  \item{xlim}{
Vector; range of \option{x} values to use for both counting and plotting. The default NULL will span the range of histogram breaks. If length equals 1 then the argument is taken to mean the sigma range to select for plotting and the clipping is done by \code{\link{magclip}}. If this is set to 'auto' then the limits will be estimated from the data dynamically. See examples.
}
  \item{ylim}{
Vector; range of y limits to show in the histogram plot.
}
  \item{plot}{
Logical; draw the histogram (otherwise it just returns the count data).
}
  \item{verbose}{
Logical; if TRUE and \option{xlim} is used then the followign is printed out: summary of the data selected, standard-deviation the 1/2-sigma implied quantiles, and number and fraction of displayed data. Note all numbers are computed for the logged values of the \option{x} input if \option{log}= x | xy | yx.
}
  \item{add}{
Logical, if TRUE the histogram will be added to the current plot. Be careful to match \option{log} properties if adding, else the comparison will be of little use and hard to interpret.
}
  \item{log}{
Log axis arguments to be passed to hist and plot. E.g. use 'x', 'y', 'xy' or 'yx' as appropriate. Default '' assumes no logging of any axes. If the x axis is logged then the histogram will be calculated in log-space.
}
  \item{\dots}{
Arguments to be parsed to \code{\link{magplot}}.
}
}
\details{
To better replicate the base \code{\link{hist}} plot you might consider setting \option{frame.plot}=FALSE, which will be parsed to \code{\link{magplot}} and turn off the outer box. The default behaviour might change in the future.
}
\value{
An object of class "histogram", basically the same output as produced by \code{\link{hist}}. Note where axes are logged, the corresponding hist list values will not be logged when returned. This is to make it easy to take a histogram object and plot it with different log scalings on the axes (see Examples). For the x axis this means the "breaks" and the "mids" items, and for the y axis this means the "counts" and the "density" items.

Appended to the end of the usual \code{\link{hist}} output are the summary of the sample (list element "summary") and the standard-deviation / 1 and 2-sigma quantile range (list element "ranges").
}
\author{
Aaron Robotham
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{hist}}
}
\examples{
maghist(rnorm(1e4))
maghist(rnorm(1e4), xlim=c(-2,4))

#Notice the x-limits are close to -3/3, since  if we ask for xlim=3 (a 3-sigma range)

maghist(rnorm(1e4), xlim=3, verbose = FALSE)

#The 'auto' option allows magclip to dynamically estimate a clip value (which is similar
#in this case, but need not be in general).

maghist(rnorm(1e4), xlim='auto', verbose = FALSE)

#Test of log histograms:

testdata=10^(runif(1e3,0,4))
maghist(testdata)
maghist(testdata,log='x')
maghist(testdata,log='y')
maghist(testdata,log='xy')

maghist(testdata,freq=FALSE)
maghist(testdata,freq=FALSE,log='x')
maghist(testdata,freq=FALSE,log='y')
maghist(testdata,freq=FALSE,log='xy')

#Test of plotting histogram objects:

testhist=maghist(testdata,log='xy')
maghist(testhist)
maghist(testhist,log='x')
magplot(testhist,log='y')
magplot(testhist,log='xy')

#Nice to see a grid with large ranges:

maghist(rnorm(1e6), grid=TRUE)
maghist(rnorm(1e6), log='y', grid=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hist }% use one of  RShowDoc("KEYWORDS")
\keyword{ histogram }% __ONLY ONE__ keyword per line
