\name{magimage}
\alias{magimage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Magically pretty images
}
\description{
A high level replacement for base image with hooks into magaxis for the tick marks and magmap for the image scaling. The default behavious is a bit different to base (e.g. x/y scales are automatically the number of pixels in the image matrix).
}
\usage{
magimage(x, y, z, zlim, xlim, ylim, col = grey((0:1000)/1000), add = FALSE,
useRaster = TRUE, asp = 1, magmap=FALSE, lo = 0, hi = 1, flip = FALSE, range = c(0, 1),
type = "quan", stretch = "lin", stretchscale = 1, bad = NA, clip = "", axes = TRUE,
frame.plot = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{
Locations of grid lines at which the values in z are measured. These must be finite and non-missing (order may be reversed). By default, equally spaced values from 0 to dim(z)[1] are used. If x is a list, its components x$x and x$y are used for x and y, respectively. If the list has component z this is used for z.
}
  \item{z}{
A numeric or logical matrix containing the values to be plotted (NAs are allowed). Note that x can be used instead of z for convenience.
}
  \item{zlim}{
The z limit with respect to the output of magmap$map. If \option{magmap}=FALSE (default) \option{zlim} should be with respect to the provided z matrix (like base \code{image}). If \option{magmap}=TRUE \option{zlim} should be with respect to the \option{range} output of \code{magmap}. By default the \code{magmap} function scales between 0 and 1, so to only show the brighter pixels \option{zlim} could be set to c(0.5,1).
}
  \item{xlim, ylim}{
Ranges for the plotted x and y values, defaulting to the ranges of x and y.
}
  \item{col}{
A list of colours for the \code{magmap} re-mapping of z to be parsed into (e.g. \code{\link{rainbow}}, \code{\link{heat.colors}}, \code{\link{topo.colors}}, \code{\link{terrain.colors}} or similar).
}
  \item{add}{
If true add the new image to the current plot.
}
  \item{useRaster}{
If TRUE a bitmap raster is used to plot the image instead of polygons. The grid must be regular in that case, otherwise an error is raised. Raster is much faster, so use when pixels are equal sized.
}
  \item{asp}{
The y/x aspect ratio
}
  \item{magmap}{
  
}
  \item{lo}{
The low limit to clip the z data at (what this means varies depending on the 'type' option). This should be a single value. See \code{\link{magmap}} for more information.
}
  \item{hi}{
The high limit to clip the z data at (what this means varies depending on the 'type' option). This should be a single value. See \code{\link{magmap}} for more information.
}
  \item{flip}{
Should the z scaling be flipped. This allows numbers from 0 to 10 to be mapped from 1 to 0 (so ordered back to front with respect to the input). See \code{\link{magmap}} for more information.
}
  \item{range}{
The numerical range of the output z mapping which should be a vector of length two specifying c(low,high). See \code{\link{magmap}} for more information.
}
  \item{type}{
The type of z mapping attempted. Options are 'quan' (default), 'num', 'sig' and 'rank'. See \code{\link{magmap}} for more information.
}
  \item{stretch}{
stretch='lin' gives linear mapping. stretch='log' gives logarithmic mapping. stretch='atan' gives atan mapping. stretch='asinh' gives asinh mapping. stretch='sqrt' gives sqrt mapping. See \code{\link{magmap}} for more information.
}
  \item{stretchscale}{
A number to multiply the z data by before applying the stretch. This only has a user impact for stretch='atan' and stretch='atanh' since it controls what parts of the data is in the linear or logarithmic regime of the stretch procedure. See \code{\link{magmap}} for more information.
}
  \item{bad}{
Sets the value that NA, NaN and infinite input z data should be set to in the final map output. This should be thought of in the context of the range argument, i.e. if bad=range[1] then bad values will be the low range value and if bad=range[2] bad values will be the high range value. See \code{\link{magmap}} for more information.
}
  \item{clip}{
By default clipped z values inherit the nearest lo/hi value (depending on which side they are clipped). Setting clip='NA' will set values outside the 'lo' and 'hi' values to be NA (currently this is the only other clip option). See \code{\link{magmap}} for more information.
}
  \item{axes}{
Specify if any axes be drawn on the image. If FALSE then only the pixels (with appropriate \code{magmap} scaling) are shown.
}
  \item{frame.plot}{
Specify if a box be drawn around the image frame. Only happens if \option{add}=TRUE and \option{axes}=TRUE.
}
  \item{\dots}{
Arguments to be parsed to \code{magaxis}. See \code{\link{magaxis}} for details.
}
}
\details{
See \code{\link{image}}, \code{\link{magmap}} and \code{\link{magaxis}} for more details.
}
\value{
Outputs the final image list containing x,y and z. Generally run for the side effect of producing nice image plots.
}
\author{
Aaron Robotham
}

\seealso{
\code{\link{image}}, \code{\link{magmap}}, \code{\link{magaxis}}
}
\examples{
#Basic
magimage(matrix(1:9,3))

#Mid pixel versus pixel edge:
magimage(3:0,1:3,matrix(1:9,3))

#Standard scaling is not very useful in this instance:
magimage(matrix(10^(1:9),3))
#Linear scaling is not very useful in this instance, though it does now map from [0,1]:
magimage(matrix(10^(1:9),3),magmap=TRUE,zlim=c(0,0.5))
#Log scaling with magmap makes it much clearer:
magimage(matrix(10^(1:9),3),magmap=TRUE,stretch='log')
#And it's easy just to show the lowest half now:
magimage(matrix(10^(1:9),3),magmap=TRUE,stretch='log',zlim=c(0,0.5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{image}