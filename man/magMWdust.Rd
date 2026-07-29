\name{magMWdust}
\alias{magMWdust}
\alias{SFD_dust}
\title{
Plot a Milky Way dust overlay
}
\description{
Adds a projected Milky Way dust layer to an existing sky plot using a lookup-table 
constructed from the SFD dust map on an equatorial grid, and drawing the result as 
points or filled polygons.
}
\usage{
magMWdust(dust.data = NULL, 
  dlon = NULL, dlat = NULL, 
  type = "p", pch = 16, pt.cex = 0.5,
  opacity.range = c(0, 0.5),
  whiteblack.percentile = c(0.5, 0.95),
  stretch = "lin",
  min.opacity.plot = 0.01, show.status = TRUE, ...)
}
\arguments{
  \item{dust.data}{
The data set to use for drawing the dust map. If \code{NULL} (the default), the function 
lazy-loads the SFD dust map data provided with the package (\code{data("SFD_dust")}). The 
code expects this data to be a data frame containing columns "ra", "dec", and "ebv". 
}
  \item{dlon}{
Step size between element of longitude. Required when specifying dust.data directly, 
and uses \code{dlon=1} when lazy-loading the internal dust map. 
}
  \item{dlat}{
Step size between element of latitude. Required when specifying dust.data directly, 
and uses \code{dlat=1} when lazy-loading the internal dust map. 
}
  \item{type}{
Character scalar giving the drawing mode: \code{"p"} for points or \code{"pl"}
for polygons.
}
  \item{pch}{
Plotting symbol used when \code{type = "p"}.
}
  \item{pt.cex}{
Point expansion factor used when \code{type = "p"}.
}
  \item{opacity.range}{
Two-element numeric vector giving the output opacity range passed to
\code{\link{magmap}}.
}
  \item{whiteblack.percentile}{
Two-element numeric vector giving the lower and upper percentiles used when
mapping dust values to opacity.
}
  \item{stretch}{
Stretch mode passed through to \code{\link{magmap}}.
}
  \item{min.opacity.plot}{
Minimum mapped opacity required for a grid cell to be drawn.
}
  \item{show.status}{
Option to show a progress bar when drawing polygons. 
}
  \item{\dots}{
Additional arguments passed to \code{\link{magproj}}.
}
}
\details{
The function plots \code{E(B-V)} values from a provided dust map data.frame, or if none is provided uses a lazy-loaded
SFD dust map which is sampled on a 1x1 deg grid. This lookup table is then mapped to points (or polygons, see below) 
with opacity determined by \code{\link{magmap}}, using the requested white/black points defined using percentiles 
(\code{whiteblack.percentile}) of the map, and throwing away cells whose opacity is below the requested 
minimum (\code{min.opacity.plot}).

When \code{type = "pl"} each retained grid cell is drawn as a projected
four-corner polygon. Otherwise the retained cells are drawn as projected
points. Polygon drawing is very slow when drawing to X11 (many minutes) except 
with very low resolutions. Polygon drawing may nonetheless be preferable when 
writing to file, where the operation is much faster and reduces aliasing effects 
caused by over- (or under-)lapping points. 
}
\value{
Returns \code{NULL} invisibly. The function is called for its plotting side
effects.
}
\references{
  Schlegel, Finkbeiner, & Davis, 1998, ApJ, 500, 2, 525
}
\author{
  Angus H Wright
}
\seealso{
\code{\link{magmap}}, \code{\link{magproj}}
}
\examples{
magproj(0,0,type='n') #Construct a base magproj plot 
magMWdust() #add MW dust map as points 
\dontrun{
magMWdust(type = "pl") #add MW dust map as polygons 
}
}
\keyword{plot}
