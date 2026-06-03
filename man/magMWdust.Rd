\name{magMWdust}
\alias{magMWdust}
\title{
Plot a Milky Way dust overlay
}
\description{
Adds a projected Milky Way dust layer to an existing sky plot by sampling the
SFD dust maps on an equatorial grid and drawing the result as points or filled
polygons.
}
\usage{
magMWdust(type = "p", pch = 16, pt.cex = 0.5,
  dlon = 1, dlat = 1,
  opacity.range = c(0, 0.5),
  whiteblack.percentile = c(0.5, 0.95),
  stretch = "lin",
  min.opacity.plot = 0.01, ...)
}
\arguments{
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
  \item{dlon}{
Grid spacing in longitude, in degrees.
}
  \item{dlat}{
Grid spacing in latitude, in degrees.
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
  \item{\dots}{
Additional arguments passed to \code{\link{magproj}}.
}
}
\details{
The function first builds an equatorial sky grid using \code{dlon} and
\code{dlat}, converts that grid to galactic coordinates, looks up
\code{E(B-V)} values with \code{\link{dust_getval}}, and maps those values to
plot opacity with \code{\link{magmap}}.

When \code{type = "pl"} each retained grid cell is drawn as a projected
four-corner polygon. Otherwise the retained cells are drawn as projected
points.
}
\value{
Returns \code{NULL} invisibly. The function is called for its plotting side
effects.
}
\author{
Angus H Wright 
}
\seealso{
\code{\link{dust_getval}}, \code{\link{magmap}}, \code{\link{magproj}}
}
\examples{
  magproj(0,0,type='n') #Construct a base magproj plot 
  magMWdust() #add MW dust map as points 
  \dontrun{
  magMWdust(type = "pl") #add MW dust map as polygons 
  }
}
\keyword{dust}
\keyword{plot}
