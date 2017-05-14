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
magcutout(image, loc = dim(image)/2, box = c(101, 101), plot = FALSE, ...)
magcutoutWCS(image, header, loc, box = c(30, 30), plot = FALSE, CRVAL1 = 0, CRVAL2 = 0,
CRPIX1 = 0, CRPIX2 = 0, CD1_1 = 1, CD1_2 = 0, CD2_1 = 0, CD2_2 = 1, unit = "deg",
sep = ":", ...)
magWCSradec2xy(RA, Dec, header, CRVAL1 = 0, CRVAL2 = 0, CRPIX1 = 0, CRPIX2 = 0, CD1_1 = 1,
CD1_2 = 0, CD2_1 = 0, CD2_2 = 1, loc.diff = c(0, 0), unit = "deg", sep = ":")
magWCSxy2radec(x, y, header, CRVAL1 = 0, CRVAL2 = 0, CRPIX1 = 0, CRPIX2 = 0, CD1_1 = 1,
CD1_2 = 0, CD2_1 = 0, CD2_2 = 1, loc.diff = c(0, 0))
}
\arguments{
  \item{image}{
Numeric matrix; required, the image we want to analyse.
}
  \item{header}{
Full FITS header in table or vector format. Legal table format headers are provided by the \code{read.fitshdr} function or the \option{hdr} list output of \code{read.fits} in the astro package). Also the \option{hdr} output of \code{readFITS} in the \code{FITSio} package provides legal vector format inputs. If a header is provided then key words will be taken from here as a priority. Missing header keywords are printed out and other header option arguments are used in these cases.
}
  \item{loc}{
Numeric vector; the [x,y] (\code{magcutout}) or [RA,Dec] (\code{magcutoutWCS}) location where we want to estimate the sky and sky RMS.
}
  \item{box}{
Numeric vector; the dimensions of the box in pixels (\code{magcutout}) or asecs (\code{magcutoutWCS}) to cut out from \option{image} centred on \option{loc}.
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
  \item{plot}{
Logical; should a \code{\link{magimage}} (\code{magcutout}) or \code{\link{magimageWCS}} (\code{magcutoutWCS}) plot of the output be generated?
}
  \item{CRVAL1}{
FITS header CRVAL1 for the Tan Gnomonic projection system. This is the RA in degrees at the location of \option{CRPIX1}.
}
  \item{CRVAL2}{
FITS header CRVAL2 for the Tan Gnomonic projection system. This is the Dec in degrees at the location of \option{CRPIX2}.
}
  \item{CRPIX1}{
FITS header CRPIX1 for the Tan Gnomonic projection system. This is the x pixel value at the location of \option{CRVAL1}.
}
  \item{CRPIX2}{
FITS header CRPIX2 for the Tan Gnomonic projection system. This is the y pixel value at the location of \option{CRVAL2}.
}
  \item{CD1_1}{
FITS header CD1_1 for the Tan Gnomonic projection system. Change in RA-Tan in degrees along x-Axis.
}
  \item{CD1_2}{
FITS header CD1_2 for the Tan Gnomonic projection system. Change in RA-Tan in degrees along y-Axis.
}
  \item{CD2_1}{
FITS header CD2_1 for the Tan Gnomonic projection system. Change in Dec-Tan in degrees along x-Axis.
}
  \item{CD2_2}{
FITS header CD2_2 for the Tan Gnomonic projection system. Change in Dec-Tan in degrees along y-Axis.
}
  \item{unit}{
The units of \option{loc} for \code{magcutoutWCS}. Allowed options are 'deg' for degress and 'sex' for sexigesimal (i.e. HMS for RA and DMS for Deg).
}
  \item{sep}{
When \option{unit}='sex', \option{sep} defines the type of separator used for the HMS and DMS strings (i.e. H:M:S and D:M:S would be sep=':', which is the default). See \code{\link{hms2deg}} and  \code{\link{dms2deg}} for more details.
}
  \item{\dots}{
Dots are parsed to either \code{\link{magimage}} (\code{magcutout}) or \code{\link{magimageWCS}} (\code{magcutoutWCS}).
}
}
\details{
These functions are on a level trivial, since it is easy to subset matrices and therefore images within R. However these functions track important properties of the subset region that makes it easy to track its location with respect to the original image. Also, they allow direct plotting of the resultant cutout with the most appropriate image functions. In many cases these functions will be used purely for their plotting side effects.
}
\value{
A list containing:

  \item{cutim}{Numeric matrix; the cutout region of interst centred around \option{loc}.}
  \item{loc}{The new \option{loc} vector that tranforms the input \option{loc} x and y location to the new \option{cutim} coordinates. This is in ProFit coordinates, so these values can be used when, e.g., constructing a ProFit modellist structure.}
  \item{loc.orig}{The original location is provided by the input \option{loc}.}
  \item{loc.diff}{The x and y offsets of the cutout compared to the original image, where \option{loc} + \option{loc.diff} = \option{loc.orig} exactly.}
  \item{xsel}{Integer vector; the extracted x pixels from the original \option{image} that form \option{cutim}.}
  \item{ysel}{Integer vector; the extracted y pixels from the original \option{image} that form \option{cutim}.}
  \item{loc.WCS}{Only output from \code{magcutoutWCS}. Numeric vector of the central RA and Dec of the cutout region in degrees.}
  \item{scale.WCS}{Only output from \code{magcutoutWCS}. Numeric vector of the x/y pixel scale in degrees.}
}
\author{
Aaron Robotham
}
\note{
By R convention the bottom-left part of the bottom-left pixel when plotting the image matrix is c(0,0) and the top-right part of the bottom-left pixel is c(1,1), i.e. the mid-point of pixels are half integer values in x and y. This differs to the FITS convention of pixel mid points being integer values. As such the R [x,y] = FITS [x-0.5,y-0.5]. This rarely matters too much in practice, but for accurate overlays you will want to get it right (see Examples).
}
\seealso{
\code{\link{magimageWCS}}
}
\examples{
\dontrun{
temp=matrix(1:121,11)

#The central value is at:

temp[6,6]

print(magcutout(temp, dim(temp)/2, box=c(3,3))$cutim)

#Given we cutout around the centre of the central pixel [5.5,5.5], the new centre
#relative to the cutim output should be at [1.5,1.5]:

print(magcutout(temp, dim(temp)/2, box=c(3,3))$loc.orig)
print(magcutout(temp, dim(temp)/2, box=c(3,3))$loc)

image=readFITS(system.file("extdata", 'VIKING/mystery_VIKING_Z.fits', package="ProFit"))
magcutout(image$imDat, box=c(89,89), plot=TRUE)
magcutoutWCS(image$imDat, image$hdr, plot=TRUE)
magcutoutWCS(image$imDat, image$hdr, loc=c(352.29167, -31.827777), plot=TRUE)$loc.WCS
magcutoutWCS(image$imDat, image$hdr, loc=c("23:29:10", "-31:49:40"), unit = 'sex',
plot=TRUE)$loc.WCS

#Here we have to remember that R [x,y] = FITS [x-0.5,y-0.5]
temp=magcutoutWCS(image$imDat, image$hdr, loc=c(352.2918, -31.82652), plot=TRUE)
points(magWCSradec2xy(c(352.2918, 352.2897), c(-31.82652, -31.8252), header=image$hdr,
loc.diff=temp$loc.diff), col='red')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cutout }% use one of  RShowDoc("KEYWORDS")
\keyword{ WCS }% __ONLY ONE__ keyword per line
