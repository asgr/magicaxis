\name{magcutout}
\alias{magcutout}
\alias{magcutoutWCS}
\alias{magWCSradec2xy}
\alias{magWCSxy2radec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Image Cutout Utilities
}
\description{
Functions to subset both raw images and images with associated WCS systems.
}
\usage{
magcutout(image, loc = dim(image)/2, box = c(100, 100), shiftloc = FALSE, paddim = TRUE,
plot = FALSE, ...)
magcutoutWCS(image, header, loc, box = c(100, 100), shiftloc = FALSE, paddim = TRUE,
plot = FALSE, CRVAL1 = 0, CRVAL2 = 0, CRPIX1 = 0, CRPIX2 = 0, CD1_1 = 1, CD1_2 = 0,
CD2_1 = 0, CD2_2 = 1, coord.type = "deg", sep = ":", loc.type=c('coord','coord'),
approx.map = FALSE, ...)
magWCSradec2xy(RA, Dec, header, CRVAL1 = 0, CRVAL2 = 0, CRPIX1 = 0, CRPIX2 = 0, CD1_1 = 1,
CD1_2 = 0, CD2_1 = 0, CD2_2 = 1, CTYPE1 = 'RA--TAN', CTYPE2 = 'DEC--TAN',
loc.diff = c(0, 0), coord.type = "deg", sep = ":")
magWCSxy2radec(x, y, header, CRVAL1 = 0, CRVAL2 = 0, CRPIX1 = 0, CRPIX2 = 0, CD1_1 = 1,
CD1_2 = 0, CD2_1 = 0, CD2_2 = 1, CTYPE1 = 'RA--TAN', CTYPE2 = 'DEC--TAN',
loc.diff = c(0, 0))
}
\arguments{
  \item{image}{
Numeric matrix; required, the image we want to decorate. If \option{image} is a list as created by \code{readFITS}, \code{read.fits} of \code{\link{magcutoutWCS}} then the image part of these lists is parsed to \option{image} and the correct header part is parsed to \option{header}.
}
  \item{header}{
Full FITS header in table or vector format. Legal table format headers are provided by the \code{read.fitshdr} function or the \option{hdr} list output of \code{read.fits} in the \code{astro} package; the \option{hdr} output of \code{readFITS} in the \code{FITSio} package or the \option{header} output of \code{magcutoutWCS}. If a header is provided then key words will be taken from here as a priority. Missing header keywords are printed out and other header option arguments are used in these cases.
}
  \item{loc}{
Numeric vector; the [x,y] (\code{magcutout}) or [x,y] / [RA,Dec] (\code{magcutoutWCS}) location where we want to cutout the image. For \code{magcutoutWCS} the unit type can be specified with the \option{loc.type} option. Either it is WCS in degrees [RA,Dec] (coord) or  pixel [x,y] of the \option{image} (image).
}
  \item{box}{
Numeric vector; the dimensions of the box to cut out from \option{image} centred on \option{loc}. For \code{magcutout} the \option{box} unit is always pixels. For \code{magcutoutWCS} the unit type can be specified with the \option{loc.type} option. Either it is pixels or asec (see \option{loc.type}). 
}
  \item{RA}{
Vector or matrix; target right ascension in degrees. If matrix then the first column will be used as \option{RA} and the second column as \option{Dec}.  
}
  \item{Dec}{
Vector; target declination in degrees. Ignored if \option{RA} is a matrix.  
}
  \item{x}{
Vector or matrix; target x-pixel. If Matrix then the first column will be used as the x-axis and the second column as y-axis. Note this is the R convention of [x,y] (see Notes) not FITS.
}
  \item{y}{
Vector; target y-pixel. Ignored if x is a matrix. Note this is the R convention of [x,y] not FITS (see Notes).
}
  \item{loc.diff}{
The pixel offset to apply. Only relevant if the image being plotted is a cutout from within a FITS legal image.
}
	\item{shiftloc}{
Logical; should the cutout centre shift from \option{loc} away from the \option{image} edge if the desired \option{box} size extends beyond the edge of the \option{image}?
}
	\item{paddim}{
Logical; should the cutout be padded with \option{image} data until it meets the desired \option{box} size (if \option{shiftloc} is TRUE) or padded with NAs for data outside the \option{image} boundary otherwise?
}
  \item{plot}{
Logical; should a \code{\link{magimage}} (\code{magcutout}) or \code{\link{magimageWCS}} (\code{magcutoutWCS}) plot of the output be generated?
}
  \item{CRVAL1}{
FITS header CRVAL1 for the \option{CTYPE1} projection system. This is the RA in degrees at the location of \option{CRPIX1}.
}
  \item{CRVAL2}{
FITS header CRVAL2 for the \option{CTYPE2} projection system. This is the Dec in degrees at the location of \option{CRPIX2}.
}
  \item{CRPIX1}{
FITS header CRPIX1 for the \option{CTYPE1} projection system. This is the x pixel value at the location of \option{CRVAL1}.
}
  \item{CRPIX2}{
FITS header CRPIX2 for the \option{CTYPE2} projection system. This is the y pixel value at the location of \option{CRVAL2}.
}
  \item{CD1_1}{
FITS header CD1_1 for the \option{CTYPE1} projection system. Change in \option{CTYPE1} in degrees along x-Axis.
}
  \item{CD1_2}{
FITS header CD1_2 for the \option{CTYPE1} projection system. Change in \option{CTYPE1} in degrees along y-Axis.
}
  \item{CD2_1}{
FITS header CD2_1 for the \option{CTYPE2} projection system. Change in \option{CTYPE2} in degrees along x-Axis.
}
  \item{CD2_2}{
FITS header CD2_2 for the \option{CTYPE2} projection system. Change in \option{CTYPE2} in degrees along y-Axis.
}
  \item{CTYPE1}{
The RA projection system type. Either 'RA--TAN' for Tan Gnomonic (default), or 'RA--SIN' for Sine Orthographic. 'RA--NCP' is approximated by Sine Orthographic with a warning. Over-ridden by the FITS header.  
}
  \item{CTYPE2}{
The DEC projection system type. Either 'DEC--TAN' for Tan Gnomonic (default), or 'DEC--SIN' for Sine Orthographic. 'DEC--NCP' is approximated by Sine Orthographic with a warning. Over-ridden by the FITS header.  
}
  \item{coord.type}{
The units of \option{loc} for \code{magcutoutWCS}. Allowed options are 'deg' for degress and 'sex' for sexigesimal (i.e. HMS for RA and DMS for Deg).
}
  \item{sep}{
When \option{coord.type}='sex', \option{sep} defines the type of separator used for the HMS and DMS strings (i.e. H:M:S and D:M:S would be sep=':', which is the default). See \code{\link{hms2deg}} and  \code{\link{dms2deg}} for more details.
}
  \item{loc.type}{
Character vector; specifies what type of location is being provided. The first element specifies the coordinate type for \option{loc} and the second element is the coordinate type for \option{box}. Either it is WCS in degrees [RA,Dec] / asec ('coord') or  pixel [x,y] of the \option{image} ('image'). If only one element is provided then the same coordinate type is used for both \option{loc} and \option{box}.
}
\item{approx.map}{
Logical; should an approximate coordinate mapping scheme be computed? This should be left as the default FALSE if saving the cut down object, and only TRUE if you are experimenting with the image cutouts within the same R session.
}
  \item{\dots}{
Dots are parsed to either \code{\link{magimage}} (\code{magcutout}) or \code{\link{magimageWCS}} (\code{magcutoutWCS}).
}
}
\details{
These functions are on a level trivial, since it is easy to subset matrices and therefore images within R. However these functions track important properties of the subset region that makes it easy to track its location with respect to the original image. Also, they allow direct plotting of the resultant cutout with the most appropriate image functions. In many cases these functions will be used purely for their plotting side effects.

The \option{shiftloc} and \option{paddim} control the behaviour of the function in the non-trivial case when the desired box size extendeds beyond the edge of the image. If \option{shiftloc} is FALSE (the default behaviour), the cutout is guaranteed to be centred on the pixel specified by \option{loc}. Then, if \option{paddim} is FALSE, the cutout extends only as far as possible until it reaches the edge of the image; otherwise if \option{paddim} is TRUE the cutout image is padded with NAs in regions outside the supplied \option{image} (the default behaviour). If \option{shiftloc} is TRUE, the centre of the cutout will be shifted. In this case, if \option{paddim} is FALSE, the cutout will extend at most half of the supplied \option{box} size from the given \option{loc}; otherwise if \option{paddim} is TRUE the cutout will be expanded until it reaches the desired \option{box} size or spans the entire image.

Note that if \option{shiftloc} is TRUE and \option{paddim} is FALSE, the cutout can be larger than \option{box}; otherwise, the cutout is guaranteed to be no larger than the specified \option{box} size.
}
\value{
A list containing:

  \item{image}{Numeric matrix; the cutout region of interst centred around \option{loc}.}
  \item{loc}{The new \option{loc} vector that tranforms the input \option{loc} x and y location to the new \option{cutim} coordinates. This is in ProFit coordinates, so these values can be used when, e.g., constructing a ProFit modellist structure.}
  \item{loc.orig}{The original location is provided by the input \option{loc}.}
  \item{loc.diff}{The x and y offsets of the cutout compared to the original image, where \option{loc} + \option{loc.diff} = \option{loc.orig} exactly.}
  \item{xsel}{Integer vector; the extracted x pixels from the original \option{image} that form \option{cutim}.}
  \item{ysel}{Integer vector; the extracted y pixels from the original \option{image} that form \option{cutim}.}
  \item{loc.WCS}{Only output from \code{magcutoutWCS}. Numeric vector of the central RA and Dec of the cutout region in degrees.}
  \item{scale.WCS}{Only output from \code{magcutoutWCS}. Numeric scalar value of the pixel scale in asec/pixel.}
  \item{usr.WCS}{Only output from \code{magcutoutWCS}. Numeric matrix of the RA/Dec extremes of the image in order BL/TL/BR/TR.}
  \item{approx.map}{Only output from \code{magcutoutWCS}. A helper function that will usually approximately map RA and Dec numeric vector inputs to a matrix with columns x and y. This is less accurate than \code{\link{magWCSradec2xy}}, so use that function is the projection is too extreme. Should work for most N-S aligned data pretty well though, and it saves having to correctly track the appropriate header.}
  \item{header}{Only output from \code{magcutoutWCS}. The updated header with the correct WCS for the cutout region. Basically this means CRPIX1.new=CRPIX1.org-loc.diff[1] and CRPIX2.new=CRPIX2.org-loc.diff[2].}
}
\author{
Aaron Robotham & Dan Taranu
}
\note{
By R convention the bottom-left part of the bottom-left pixel when plotting the image matrix is c(0,0) and the top-right part of the bottom-left pixel is c(1,1), i.e. the mid-point of pixels are half integer values in x and y. This differs to the FITS convention of pixel mid points being integer values. As such the R [x,y] = FITS [x-0.5,y-0.5]. This rarely matters too much in practice, but for accurate overlays you will want to get it right (see Examples).

It is ambiguous what the desired outcome is in some cutting scenarios, e.g. what should be returned if a 3x3 cutout is requested at the "centre" of a 8x8 image? For this reason, and to avoid unexpected results due to numerical precision, you should only cut out even pixel dimensions if integer pixel coordinates are provided, and odd pixel dimensions if half-integer pixel coordinates are provided. Regardless, the \option{loc} and \option{loc.orig} outputs will always help you locate the absolute coordinates of your desired cut out centre in both the cut out and the original image coordinate system.
}
\seealso{
\code{\link{magimageWCS}}
}
\examples{
\dontrun{
temp=matrix(1:121,11)

#The central value is at:

temp[6,6]

print(magcutout(temp, dim(temp)/2, box=c(3,3))$image)

#Given we cutout around the centre of the central pixel [5.5,5.5], the new centre
#relative to the cutout image output should be at [1.5,1.5]:

print(magcutout(temp, dim(temp)/2, box=c(3,3))$loc.orig)
print(magcutout(temp, dim(temp)/2, box=c(3,3))$loc)

# A simple WCS cutout example:

image=readFITS(system.file("extdata", 'VIKING/mystery_VIKING_Z.fits', package="ProFound"))

par(mar=c(3.1,3.1,1.1,1.1))
magcutout(image$imDat, loc=c(50.5,50.5), plot=TRUE)
magcutoutWCS(image, loc=c(50.5,50.5), loc.type = 'image', plot=TRUE)

#We can cut out using the coordinates instead:
print({tempcoord=magWCSxy2radec(50.5,50.5,header=image$hdr)})
magcutoutWCS(image, loc=tempcoord, loc.type=c('coord','image'), plot=TRUE)

#You can select coordinates too:

magcutoutWCS(image, loc=c(352.29167, -31.827777), box=c(30,30), plot=TRUE)$loc.WCS
magcutoutWCS(image, loc=c("23:29:10", "-31:49:40"), box=c(30,30) , coord.type = 'sex',
plot=TRUE)$loc.WCS

#We can add RA Dec specific decorations easily:

cutim=magcutoutWCS(image, loc=c(352.2918, -31.82652), box=c(30,30), plot=TRUE,
approx.map=TRUE)

#Approx overlays:

points(cutim$approx.map(c(352.2918, 352.2897), c(-31.82652, -31.8252)), pch=3, col='blue')

#Exact overlays:

points(magWCSradec2xy(c(352.2918, 352.2897), c(-31.82652, -31.8252), header=cutim$header),
col='red')

#Given we correctly modify the header, we can actually use the cut down image directly:

magimageWCS(cutim)

# Now test the various cutout size options for a large cutout near the image boundary

loc = c(300,340)
box = c(200,200)
loc.type = c("image","image")

magimage(image$imDat)
points(loc[1], loc[2], col='red')

# Setting shiftloc=FALSE and paddim=TRUE pads the image with NAs (default):

cutim=magcutout(image$imDat, loc=loc, box=box, plot=TRUE, shiftloc=FALSE, paddim=TRUE)
points(cutim$loc[1], cutim$loc[2], col='red')

cutim=magcutoutWCS(image, loc=loc, box=box, coord.type="image", loc.type=loc.type,
	plot=TRUE, shiftloc=FALSE, paddim=TRUE)
points(cutim$loc[1], cutim$loc[2], col='red')
	
# The cutout is exactly the request size, but the centre is shifted:

cutim=magcutout(image$imDat, loc=loc, box=box, plot=TRUE, shiftloc=TRUE, paddim=TRUE)
points(cutim$loc[1], cutim$loc[2], col='red')

cutim=magcutoutWCS(image, loc=loc, box=box, coord.type="image", loc.type=loc.type,
	plot=TRUE, shiftloc=TRUE, paddim=TRUE)
points(cutim$loc[1], cutim$loc[2], col='red')

# Setting paddim=FALSE returns the largest possible cutout within the image bounds,
# without shifting the centre:

cutim=magcutout(image$imDat, loc=loc, box=box, plot=TRUE, shiftloc=FALSE, paddim=FALSE)
points(cutim$loc[1], cutim$loc[2], col='red')

cutim=magcutoutWCS(image, loc=loc, box=box, coord.type="image", loc.type=loc.type,
  plot=TRUE, shiftloc=FALSE, paddim=FALSE)
points(cutim$loc[1], cutim$loc[2], col='red')

# Setting paddim=FALSE and shiftloc=TRUE returns a larger cutout, but with at most
# box/2 padding on either side:

cutim=magcutout(image$imDat, loc=loc, box=box, plot=TRUE, shiftloc=TRUE, paddim=FALSE)
points(cutim$loc[1], cutim$loc[2], col='red')

cutim=magcutoutWCS(image, loc=loc, box=box, coord.type="image", loc.type=loc.type,
  plot=TRUE, shiftloc=TRUE, paddim=FALSE)
points(cutim$loc[1], cutim$loc[2], col='red')

# Setting shiftloc=FALSE and requesting a box size larger than the image returns a cutout
# with the requested box size:

box = c(400,400)
cutim=magcutoutWCS(image, loc=loc, box=box, coord.type="image", loc.type=loc.type,
plot=TRUE, shiftloc=FALSE, paddim=TRUE)
points(cutim$loc[1], cutim$loc[2], col='red')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cutout }% use one of  RShowDoc("KEYWORDS")
\keyword{ WCS }% __ONLY ONE__ keyword per line
