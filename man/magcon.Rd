\name{magcon}
\alias{magcon}

\title{
2D quantile images and contours
}
\description{
This function generates pretty images and contours that reflect the 2D quantile levels of the data. This means the user can immediately assess the 2D regime that contains an arbitrary percentage of the data. This function was designed particularly with the output of MCMC posteriors in mind, where visualising the location of the 68\% and 95\% 2D quantiles for covariant parameters is a necessary part of the post MCMC analysis.
}
\usage{
magcon(x, y, h, doim = TRUE, docon = TRUE, dobar = TRUE, n = 100, add = FALSE,
xlab='', ylab='', imcol = rev(rainbow(1000, start = 0, end = 2/3)),
conlevels = c(0.5, pnorm(1) - pnorm(-1), 0.95), barposition = "topright",
barorient = "v",bartitle = "Contained \%", bartitleshift=0,xlim=NULL,ylim=NULL,
weights=NA,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x values to contour. If x is a two (or more) column matrix or data.frame and y is missing as an argument, then the first column is used for x and the second column for y.
}
  \item{y}{
y values to contour.
}
  \item{h}{
Smoothing parameter to pass to kde2d. Can take 1 or 2 arguments for x and optionally y smoothing.
}
  \item{doim}{
Should an image be generated.
}
  \item{docon}{
Should contours be overlain.
}
  \item{dobar}{
Should a magbar colour bar be added describing the image levels (doim must also be true for this to appear).
}
  \item{n}{
The n to send to kde2d to determine the resolution of the smoothing.
}
  \item{add}{
Should the output of this function be added to the current plot. If FALSE then a new plot is generated.
}
  \item{xlab}{
Label for x-axis, only used if add=FALSE.
}
  \item{ylab}{
Label for y-axis, only used if add=FALSE.
}
  \item{imcol}{
The colour palette to use for the image (this is also sent to magbar).
}
  \item{conlevels}{
Specific quantile contours to add. Default is for 50\%, 68\% and 95\% contours, i.e. these contours contain that perecentage of the data.
}
  \item{barposition}{
The position to use for magbar. See magbar help for more details.
}
  \item{barorient}{
The orientation to use for magbar. See magbar help for more details.
}
  \item{bartitle}{
Title to use for magbar.
}
  \item{bartitleshift}{
Control of how far the magbar title is shifted away from its default position.
}
  \item{xlim}{
The x limits to use for the data. Default of NULL caculates the range based on the provided x data vector. Data will be clipped between the extremes given. If xlim[1]>xlim[2] plotted axes will be flipped compared to default.
}
  \item{ylim}{
The y limits to use for the data. Default of NULL caculates the range based on the provided y data vector. Data will be clipped between the extremes given. If ylim[1]>ylim[2] plotted axes will be flipped compared to default.
}
  \item{weights}{
A vector of weights to pass onto sm.density (that does the 2D density estimate). This must be the same length as the x and y vectors if specified.
}
  \item{\dots}{
Other arguments to pass to the \code{\link{contour}} function, e.g. lty=c(2,1,3).
}
}
\details{
This function is particularly designed to assess the output for MCMC posteriors since it highlights the confidence regimes quite clearly. More generally it can show the quantile distributions for any 2D data.
}
\value{
Called for the side effect of generating images and contours representing quantile in 2D data.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{magplot}},\code{\link{magaxis}},\code{\link{maglab}},\code{\link{magmap}},\code{\link{magrun}},\code{\link{magbar}}
}
\examples{
temp=cbind(rnorm(1e3),rnorm(1e3))
magcon(temp[,1],temp[,2])
}
\keyword{quantile}
\keyword{contour}

