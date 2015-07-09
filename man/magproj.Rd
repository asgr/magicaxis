\name{magproj}
\alias{magproj}
\title{
Magic longitude / latitude projection function
}
\description{
High level methods for producing pretty plot of projected data. Particularly useful in astronomy or geography, where many datasets are in longitude (right ascension) / latitude (declination) format.
}
\usage{
magproj(long, lat, type = "b", plottext, longlim = c(-180, 180), latlim = c(-70, 70), projection = "mollweide", parameters = NULL, centre = c(0, 0), add = FALSE, nx = 6, ny = 6, labels = TRUE, grid=TRUE, grid.col = "grey", grid.lty = 2, auto = FALSE, upres = 100, box = TRUE, labloc = c(-75, -50), labeltype = "deg", ...)
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
The display type, either points (p), lines (l), text (t), or box (b, the default). Points simply projects longitude and latitude positions into particle positions. Lines will join the poitions together into a line, using \command{approxfun} to interpolate between positions at resolution \option{upres}. Text will display the text provided in plottext at the positions. Box will draw a polygon box, where the limits are given as a two element vector for \option{long} and a two element vector for \option{lat}.
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
  \item{nx}{
The target number of gridlines on the x-axis. Uses \code{\link{pretty}}, so the result may not be what is requested.
}
  \item{ny}{
The target number of gridlines on the y-axis. Uses \code{\link{pretty}}, so the result may not be what is requested.
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
  \item{\dots}{
Extra options that are either passed to \code{\link{points}} (\option{type='p'}), \code{\link{lines}} (\option{type='l'}), \code{\link{text}} (\option{type='t'}), or \code{\link{polygon}} (\option{type='b'}).
}
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{magplot}},\code{\link{magaxis}},\code{\link{maglab}},\code{\link{magmap}},\code{\link{magrun}},\code{\link{magbar}}
}
\examples{
magproj(c(129,141), c(-2,3), type='b', centre=c(180,0), col='red', projection='aitoff',
labloc=c(75,-50))
magproj(c(174,186), c(-3,2), col='green', projection='aitoff', add=TRUE)
magproj(c(211.5,223.5), c(-2,3), col='blue', projection='aitoff', add=TRUE)
magproj(c(30.2,38.8), c(-6,-3.72), col='brown', projection='aitoff', add=TRUE)
magproj(c(30.2,38.8), c(-10.25,-6), col='orange', projection='aitoff', add=TRUE)
magproj(c(339,351), c(-35,-30), col='purple', projection='aitoff', add=TRUE)
magproj(c(35,135,180,217.5,345), c(-3.72,3,2,3,-30)+10, type='t',
plottext=c('G02','G09','G12','G15','G23'), add=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ projection }
\keyword{ map }% __ONLY ONE__ keyword per line
