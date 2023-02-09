\name{magcutout}
\alias{magcutout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Image Cutout Utility
}
\description{
Function to subset raw images/matrices.
}
\usage{
magcutout(image, loc = dim(image)/2, box = c(100, 100), shiftloc = FALSE, paddim = TRUE,
  padval = NA, plot = FALSE, ...)
}
\arguments{
  \item{image}{
Numeric matrix; required, the image we want to cutout. If \option{image} is a path to an \code{fst} format file then the cutout can be done on disk directly.
}
  \item{loc}{
Numeric vector; the [x,y] location where we want to cutout the image.
}
  \item{box}{
Numeric vector; the dimensions of the box to cut out from \option{image} centred on \option{loc}. For \code{magcutout} the \option{box} unit is always pixels.
}
	\item{shiftloc}{
Logical; should the cutout centre shift from \option{loc} away from the \option{image} edge if the desired \option{box} size extends beyond the edge of the \option{image}?
}
	\item{paddim}{
Logical; should the cutout dimensions be padded with with value of \option{padval} for data outside the \option{image} boundary (TRUE)? If FALSE the dimensions will truncate when the edge of the input \option{image} has been reached.
}
	\item{padval}{
Numeric scalar; the value to use for padding if \option{paddim}=TRUE.
}
  \item{plot}{
Logical; should a \code{\link{magimage}} plot of the output be generated?
}
  \item{\dots}{
Dots are parsed to \code{\link{magimage}}.
}
}
\details{
This function is on a level trivial, since it is easy to subset matrices and therefore images within R. However \code{magcutout} tracks important properties of the subset region that makes it easy to track its location with respect to the original matrix/image. Also, it allows direct plotting of the resultant cutout with the most appropriate image functions. In many cases these functions will be used purely for their plotting side effects.

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
}
\author{
Aaron Robotham & Dan Taranu
}
\note{
By R convention the bottom-left part of the bottom-left pixel when plotting the image matrix is c(0,0) and the top-right part of the bottom-left pixel is c(1,1), i.e. the mid-point of pixels are half integer values in x and y. This differs to the FITS convention of pixel mid points being integer values. As such the R [x,y] = FITS [x-0.5,y-0.5]. This rarely matters too much in practice, but for accurate overlays you will want to get it right (see Examples).

It is ambiguous what the desired outcome is in some cutting scenarios, e.g. what should be returned if a 3x3 cutout is requested at the "centre" of a 8x8 image? For this reason, and to avoid unexpected results due to numerical precision, you should only cut out even pixel dimensions if integer pixel coordinates are provided, and odd pixel dimensions if half-integer pixel coordinates are provided. Regardless, the \option{loc} and \option{loc.orig} outputs will always help you locate the absolute coordinates of your desired cut out centre in both the cut out and the original image coordinate system.
}
\examples{
temp=matrix(1:121,11)

#The central value is at:

temp[6,6]

print(magcutout(temp, dim(temp)/2, box=c(3,3))$image)

#Given we cutout around the centre of the central pixel [5.5,5.5], the new centre
#relative to the cutout image output should be at [1.5,1.5]:

print(magcutout(temp, dim(temp)/2, box=c(3,3))$loc.orig)
print(magcutout(temp, dim(temp)/2, box=c(3,3))$loc)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cutout }% use one of  RShowDoc("KEYWORDS")

