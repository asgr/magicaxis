\name{magproj}
\alias{magproj}
\alias{magprojgrid}
\alias{magprojlabels}
\title{
Magic longitude / latitude projection function
}
\description{
High level methods for producing pretty plot of projected data. Particularly useful in astronomy or geography, where many datasets are in longitude (right ascension) / latitude (declination) format. magproj is the highest level function, creating a projected image grid with labels and data. magprojgrid and magprojlabels are functions to simply overplot a grid and add labels respectively.
}
\usage{
magproj(long, lat, type = "b", plottext, longlim = c(-180, 180), latlim = c(-90, 90),
projection = "aitoff", parameters = NULL, centre = c(0, 0), add = FALSE, fliplong=FALSE,
nlat=6, nlong=6, prettybase=30, labels = TRUE, grid=TRUE, grid.col = "grey", grid.lty = 2,
auto = FALSE, upres = 100, box = TRUE, labloc = c(90, -45), labeltype = "deg",
crunch=FALSE, ...)
magprojgrid(nlat=6, nlong=6, prettybase=30, box=TRUE, ...)
magprojlabels(nlat=6, nlong=6, prettybase=30, labloc = c(90, -45), labeltype='deg',
crunch=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long}{
Vector of longitude values to use. If this is a matrix or data.frame with two columns and \option{lat} is missing then column 1 is taken to be longitude values and column 2 is taken to be latitude values. long should have 2 elements only when \option{type}="b".
}
  \item{lat}{
Vector of latitude values to use. If the input for \option{long} is a matrix or data.frame with two columns and lat is missing then column 1 is taken to be longitude values and column 2 is taken to be latitude values. lat should have 2 elements only when type="b".
}
  \item{type}{
The display type, either points (p), lines (l), polygon (pl), text (t), or box (b, the default). Points simply projects longitude and latitude positions into particle positions. Lines will join the positions together into a line, using \command{approxfun} to interpolate between positions at resolution \option{upres}. Polygon will join the positions together into a polygon, using \command{approxfun} to interpolate between positions at resolution \option{upres}. Text will display the text provided in plottext at the positions. Box will draw a polygon box, where the limits are given as a two element vector for \option{long} and a two element vector for \option{lat}.
}
  \item{plottext}{
A vector of text to display at the provided longitude and latitude positions. Only used if \option{type='t'}.
}
  \item{longlim}{
The longitude limits to use in the plot. Vector of length 2.
}
  \item{latlim}{
The latitude limits to use in the plot. Vector of length 2.
}
  \item{projection}{
Map projection to use. This function directly uses \code{\link{mapproject}}, and all of the inputs allowed for the \option{projection} argument in that function are also allowed here.
}
  \item{parameters}{
Map parameters to use. For details see the \option{parameters} argument in \code{\link{mapproject}}.
}
  \item{centre}{
For most popular projections this argument specifies the longitude and latitude that is centred in the plot. Strictly \option{orientation} in \code{\link{mapproject}} is set to \code{c(90+centre[2], centre[1], 0)}.
}
  \item{add}{
Should a fresh plot be drawn (\option{add=FALSE}), or should the new data be added to the current plot (\option{add=TRUE}).
}
  \item{fliplong}{
Should the the longitude axis be flipped so that low values are on the right hand side (normal for celestial sphere plots in astronomy).
}
  \item{nlong}{
The target number of gridlines in the longitude direction. Uses \code{\link{pretty}}, so the result may not be what is requested.
}
  \item{nlat}{
The target number of gridlines in the latitude direction. Uses \code{\link{pretty}}, so the result may not be what is requested.
}
  \item{prettybase}{
The unit of repitition desired for the grid lines and labels. See \option{prettybase} in \code{\link{maglab}}. By default it is 30, implying a pretty plot is one with marks at 30, 60, 90 etc (i.e. attractive for large scale plots covering large longitude and latitude limits).
}
  \item{labels}{
Should text coordinate labels be added to the plot.
}
  \item{grid}{
Should a background grid be drawn.
}
  \item{grid.col}{
The colour of the background grid.
}
  \item{grid.lty}{
The line type for the background grid.
}
  \item{auto}{
If \option{auto=FALSE} the plot is set up using all options specified. If \option{auto=TRUE} then \option{longlim}, \option{latlim}, \option{centre} and \option{labloc} is estimated from the data. This mostly behaves sensibly, but do not be too surprised if the automatic plot is not ideal, and some manual tweaking is required.
}
  \item{upres}{
The resolution at which to do internal interpolation when drawing lines and boxes.
}
  \item{box}{
Should a black outline be drawn following the \option{longlim} and \option{latlim} limits.
}
  \item{labloc}{
The longitude and latitude at which labels should be drawn.
}
  \item{labeltype}{
Should the labels be drawn using degrees (deg) or column delimited sexigesimal (sex).
}
  \item{crunch}{
If set to FALSE the full output of \code{\link{deg2hms}} and \code{\link{deg2dms}} is printed. If set to TRUE a simplified output is used, where only the hours and degrees parts are extracted and appended with a 'h' and a degree symbol respectively.
}
  \item{\dots}{
For magproj, Extra options that are either passed to \code{\link{points}} (\option{type='p'}), \code{\link{lines}} (\option{type='l'}), \code{\link{polygon}} (\option{type='pl'}), \code{\link{text}} (\option{type='t'}), or \code{\link{polygon}} (\option{type='b'}). For magprojgrid dots are pased to \code{\link{lines}} for drawing the grid lines. For magprojlabels dots are passed to \code{\link{text}} for adding text labels.
}
}
\value{
  No output. Run for the side effect of producing nice projected plots.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{magplot}}, \code{\link{magaxis}}, \code{\link{maglab}}, \code{\link{magmap}}, \code{\link{magrun}}, \code{\link{magbar}}, \code{\link{magprojextra}}
}
\examples{
# GAMA fields:
par(mar=c(0.1,0.1,0.1,0.1))
magproj(c(129,141), c(-2,3), type='b', projection='aitoff', centre=c(180,0),
fliplong=TRUE, labloc=c(90,-45), col='red', labeltype = 'sex', crunch=TRUE)
magproj(c(211.5,223.5), c(-2,3), col='red', add=TRUE)
magproj(c(30.2,38.8), c(-10.25,-3.72), col='red', add=TRUE)
magproj(c(30.2,38.8), -6, type='l', add=TRUE, col='grey')
magproj(c(339,351), c(-35,-30), col='red', add=TRUE)

magecliptic(width=10,col=hsv(1/12,alpha=0.3),border=NA)
magecliptic(width=0,col='orange')
magMWplane(width=20,col=hsv(v=0,alpha=0.1),border=NA)
magMWplane(width=0,col='darkgrey')
magMW(pch=16, cex=2, col='darkgrey')
magsun(c(7,26), pch=16, cex=2, col='orange2') #An important date!

magproj(c(174,186), c(-3,2), col='red', add=TRUE)

#Plus SDSS:
magproj(c(110,260), c(-4,70), border='blue', add=TRUE)

magproj(c(35,135,180,217.5,345), c(-3.72,3,2,3,-30)+10, type='t',
plottext=c('G02','G09','G12','G15','G23'), add=TRUE)

legend('topleft', legend=c('GAMA Regions','SDSS Main Survey'), col=c('red','blue'),
pch=c(15,NA), lty=c(NA,1), bty='n')
legend('topright', legend=c('Ecliptic','MW Plane'), col=c(hsv(c(1/12,0), v=c(1,0),
alpha=0.5)), pch=c(15,15), lty=c(1,1), bty='n')
legend('bottomleft', legend=c('Sun', 'MW Centre'), col=c('orange2','darkgrey'), pch=16,
bty='n')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ projection }
\keyword{ map }% __ONLY ONE__ keyword per line
