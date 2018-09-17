\name{magimageWCS}
\alias{magimageWCS}
\alias{magimageWCSRGB}
\alias{magimageWCSGrid}
\alias{magimageWCSLabels}
\alias{magimageWCSCompass}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tan Gnomonic WCS Image Decoration
}
\description{
These functions add various WCS information to R images. It is particularly designed to work in unison with \option{magimage} that is used extensively in the wider ProFit package.
}
\usage{
magimageWCS(image, header, n, grid.col = "grey", grid.lty = 2, grid.lwd = 0.5,
lab.col = "green", coord.type = "sex", margin = TRUE, loc.diff = c(0, 0),
xlab = "Right Ascension", ylab = "Declination", mgp = c(2, 0.5, 0), mtline = 2,
position = "topright", com.col = "green", com.length = 0.05, coord.axis = 'auto',
pretty = 'auto', CRVAL1 = 0, CRVAL2 = 0, CRPIX1 = 0, CRPIX2 = 0, CD1_1 = 1, CD1_2 = 0,
CD2_1 = 0, CD2_2 = 1, CTYPE1 = 'RA--TAN', CTYPE2 = 'DEC--TAN', ...)

magimageWCSRGB(R, G, B, header_out, Rheader, Gheader, Bheader, dowarp='auto',
direction = "auto", boundary = "dirichlet", interpolation = "cubic", n, grid.col='grey',
grid.lty=2, grid.lwd=0.5, lab.col='green', coord.type='sex', margin=TRUE, loc.diff=c(0,0),
xlab='Right Ascension', ylab='Declination', mgp=c(2,0.5,0), mtline=2, position='topright',
com.col="green", com.length=0.05, coord.axis='auto', pretty='auto', CRVAL1=0, CRVAL2=0,
CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, CTYPE1 = 'RA--TAN',
CTYPE2 = 'DEC--TAN', ...)

magimageWCSGrid(header, n, grid.col = "grey", grid.lty = 1, grid.lwd = 1,
coord.type = "sex", loc.diff = c(0, 0), pretty= 'auto', CRVAL1 = 0, CRVAL2 = 0,
CRPIX1 = 0, CRPIX2 = 0, CD1_1 = 1, CD1_2 = 0, CD2_1 = 0, CD2_2 = 1, CTYPE1 = 'RA--TAN',
CTYPE2 = 'DEC--TAN', ...)

magimageWCSLabels(header, n, lab.col = "green", coord.type = "sex", margin = TRUE,
loc.diff = c(0, 0), xlab = "Right Ascension", ylab = "Declination", mgp = c(2, 0.5, 0),
mtline = 2, coord.axis='auto', pretty= 'auto', CRVAL1 = 0, CRVAL2 = 0, CRPIX1 = 0,
CRPIX2 = 0, CD1_1 = 1, CD1_2 = 0, CD2_1 = 0, CD2_2 = 1, CTYPE1 = 'RA--TAN',
CTYPE2 = 'DEC--TAN', ...)

magimageWCSCompass(header, position = "topright", com.col = "green", com.length = 0.05, 
loc.diff = c(0, 0), CRVAL1 = 0, CRVAL2 = 0, CRPIX1 = 0, CRPIX2 = 0, CD1_1 = 1, CD1_2 = 0,
CD2_1 = 0, CD2_2 = 1, CTYPE1 = 'RA--TAN', CTYPE2 = 'DEC--TAN', ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image}{
Numeric matrix; required, the image we want to decorate. If \option{image} is a list as created by \code{readFITS}, \code{read.fits} of \code{\link{magcutoutWCS}} then the image part of the list is parsed to \option{image} and the correct header part is parsed to \option{header}.
}
  \item{header}{
Full FITS header in table or vector format. Legal table format headers are provided by the \code{read.fitshdr} function or the \option{hdr} list output of \code{read.fits} in the astro package; the \option{hdr} output of \code{readFITS} in the \code{FITSio} package or the \option{header} output of \code{magcutoutWCS}. If a header is provided then key words will be taken from here as a priority. Missing header keywords are printed out and other header option arguments are used in these cases.
}
  \item{R}{
Numeric matrix; containing the red colour values to be plotted (NAs are allowed). If \option{R} is a list as created by \code{readFITS}, \code{read.fits} of \code{\link{magcutoutWCS}} then the image part of the list is parsed to \option{R} and the correct header part is parsed to \option{Rheader}.
}
  \item{G}{
Numeric matrix; containing the green colour values to be plotted (NAs are allowed). If \option{G} is a list as created by \code{readFITS}, \code{read.fits} of \code{\link{magcutoutWCS}} then the image part of the list is parsed to \option{G} and the correct header part is parsed to \option{Gheader}.
}
  \item{B}{
Numeric matrix; containing the blue colour values to be plotted (NAs are allowed). If \option{B} is a list as created by \code{readFITS}, \code{read.fits} of \code{\link{magcutoutWCS}} then the image part of the list is parsed to \option{B} and the correct header part is parsed to \option{Bheader}.
}
  \item{header_out}{
Full FITS header in table or vector format. This is the target WCS projection that the RGB image will be mapped onto. Legal table format headers are provided by the \code{read.fitshdr} function or the \option{hdr} list output of \code{read.fits} in the astro package; the \option{hdr} output of \code{readFITS} in the \code{FITSio} package or the \option{header} output of \code{magcutoutWCS}. If a header is provided then key words will be taken from here as a priority. Missing header keywords are printed out and other header option arguments are used in these cases.
}
  \item{Rheader}{
Full \option{R} FITS header in table or vector format. It is usually easier to include the header with the \option{R} list input as described above.
}
  \item{Gheader}{
Full \option{G} FITS header in table or vector format. It is usually easier to include the header with the \option{G} list input as described above.
}
  \item{Bheader}{
Full \option{B} FITS header in table or vector format. It is usually easier to include the header with the \option{B} list input as described above.
}
  \item{dowarp}{
Image warping flag, either TRUE, FALSE or 'auto' (default). If TRUE then the images will be warped onto the \option{header_out} WCS scheme, if FALSE they will not be (which will break if the WCSs are not truly the same and the iamges are pixel matched). 'auto' tries to detect if the headers are all the same, and if they are then it will set \option{dowarp}=FALSE, otherwise it will set to TRUE. This can be fooled by small (and unimportant) differences in the headers, e.g. they have different SWarp dates etc, since it just checks if the text differs anywhere.
}
  \item{direction}{
Only used if re-mapping via \code{\link{magwarp}}. "auto" (default), "forward" or "backward", see \code{imwarp}. Since it is usally better to go from the higher resolution image and map this onto the lower resolution grid, "auto" selects the better direction given the pixel scales recovered from the header information.
}
  \item{boundary}{
Only used if re-mapping via \code{\link{magwarp}}. Boundary conditions: "dirichlet", "neumann", "periodic" (default "dirichlet"), see \code{imwarp}
}
  \item{interpolation}{
Only used if re-mapping via \code{\link{magwarp}}. "nearest", "linear", "cubic" (default "linear"), see \code{imwarp}
}
  \item{n}{
The target number of major-axis sub-divisions. Will not necessarily be achieved.
}
  \item{grid.col}{
The colour of the overlaid grid lines.
}
  \item{grid.lty}{
The line type of the overlaid grid lines.
}
  \item{grid.lwd}{
The line width of the overlaid grid lines.
}
  \item{lab.col}{
The colour of the labels when \option{margin}=FALSE.
}
  \item{coord.type}{
Should the labels be drawn using degrees (deg) or colon delimited sexigesimal (sex).
}
  \item{margin}{
Should the labels be drawn in the outer margin region (default).
}
  \item{loc.diff}{
The pixel offset to apply. Only relevant if the image being plotted is a cutout from within a FITS legal image.
}
  \item{xlab}{
x axis name. If left as default either H:M:S or D:M:S (\option{coord.type}='sex') or deg (\option{coord.type}='deg') will be appended.
}
  \item{ylab}{
y axis name. If left as default either H:M:S or D:M:S (\option{coord.type}='sex') or deg (\option{coord.type}='deg') will be appended.
}
  \item{mgp}{
The margin line (in mex units) for the axis title, axis labels and axis line. This has different (i.e. prettier) defaults than R of c(2,0.5,0) rather than c(3,1,0). This pushes the numbers and labels nearer to the plot compared to the defaults. For \option{margin}=FALSE \option{mgp} = -\option{mgp} - 3, which has the effect of shifting the tick labels nicely inside the margin.
}
  \item{mtline}{
Number of lines separating axis name from axis. For \option{margin}=FALSE \option{mtline} = -\option{mtline}, which has the effect of shifting the axis labels nicely inside the margin.
}
\item{coord.axis}{
Integer vector; specifies whether the RA and Dec axes should be 1 or 2 (i.e. x or y axis). The default 'auto' tries to guess based on the header information (and this usually works okay). Otherwise \option{coord.axis}=c(1,2) would be sensible for a N/S vertical aligend frame and \option{coord.axis}=c(2,1) would be sensible for a E/W vertically aligned frame. When the orientation is between the two then it is not always obvious which will work better.
}
\item{pretty}{
If 'auto' then it will try to compute the prettiest grid and label scaling *usually this is pretty good). Otherwise set to 1 to be degrees major ticks, 60 for minutes (DMS or HMS) and 3600 for seconds (DMS or HMS).
}
  \item{position}{
Relative position of the compass bar. This argument is used like the 'legend' function. Specify one of 'bottom', 'bottomleft', 'left', 'topleft', 'top', 'topright', 'right', 'bottomright' and 'centre'. 
}
  \item{com.col}{
Colour of the compass.
}
  \item{com.length}{
Length of the edges of the arrow head.
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
  \item{\dots}{
These are parsed to \code{\link{magimage}}, (\code{magimageWCS}), \code{\link{magimageRGB}}, (\code{magimageWCSRGB}), \code{\link{lines}} (\code{magimageWCSGrid}), \code{\link{axis}} (\code{magimageWCSLabels}) or \code{\link{arrows}} (\code{magimageWCSCompass}).
}
}
\details{
Most people will be content to use the higher level \code{magimageWCS} function, which calls (in order) \code{magimage}, \code{magimageWCSGrid}, \code{magimageWCSLabels} and \code{magimageWCSCompass}.

\code{magimageWCSRGB} can be used in a few different ways, but the recommended route is to supply combined image and header list objects to the \option{R}, \option{G} and \option{B} arguments. If \option{header_out} is not supplied then by default it will project the three images onto the first available WCS header it finds searching \option{R}, \option{G} and \option{B} in order. If you are happy to map the colour image into the \option{R} WCS then you do not need to supply \option{header_out} at all. Note if image remapping is required (the images are different sizes or the headers differ) then the \code{imager} library will need to be installed in order to use \code{\link{magwarp}}.
}
\value{
Outputs the final image list containing x,y and z (\code{magimageWCS}) or R/G/B (\code{magimageWCSRGB}). Generally run for the side effect of producing rapid and well-scaled image plots.
}
\author{
Aaron Robotham
}
\note{
For convenience users can use the header outputs produced by both the \code{readFITS} and \code{astro} package.

By R convention the bottom-left part of the bottom-left pixel when plotting the image matrix is c(0,0) and the top-right part of the bottom-left pixel is c(1,1), i.e. the mid-point of pixels are half integer values in x and y. This differs to the FITS convention of pixel mid points being integer values. As such the R [x,y] = FITS [x-0.5,y-0.5]. This rarely matters too much in practice, but for accurate overlays you will want to get it right.
}
\seealso{
\code{\link{magimage}}, \code{\link{magcutoutWCS}}, \code{\link{radec2xy}}, \code{\link{xy2radec}}
}
\examples{
\dontrun{
image=readFITS(system.file("extdata", 'VIKING/mystery_VIKING_Z.fits', package="ProFound"))

#Convenient image plotting for lists containing headers:

magimageWCS(image$imDat, header=image$hdr)
magimageWCS(image)

#First using the outer margins for tick labels:

par(mar=c(3.1,3.1,1.1,1.1))
magimageWCS(image)
magimageWCS(image, coord.type='deg')

#Now removing the margins and putting labels inside the image:

par(mar=c(0,0,0,0))
magimageWCS(image, margin=FALSE)
magimageWCS(image, margin=FALSE, coord.type='deg')

#We can make a WCS colour image of mismatched images:

VISTA_K=readFITS(system.file("extdata", 'VISTA_K.fits', package="magicaxis"))
VST_r=readFITS(system.file("extdata", 'VST_r.fits', package="magicaxis"))
GALEX_NUV=readFITS(system.file("extdata", 'GALEX_NUV.fits', package="magicaxis"))

magimageWCSRGB(VISTA_K, VST_r, GALEX_NUV)
magimageWCSRGB(VISTA_K, VST_r, GALEX_NUV, saturation=0.5)

#To make direct magimageRGB plots of the outputs you must turn off magmap scaling:

temp=magimageWCSRGB(VISTA_K, VST_r, GALEX_NUV)
magimageRGB(R=temp$R, G=temp$G, B=temp$B, magmap=FALSE)

#We can map onto various WCS schemes easily too:

magimageWCSRGB(VISTA_K, VST_r, GALEX_NUV, VISTA_K$hdr)
magimageWCSRGB(VISTA_K, VST_r, GALEX_NUV, VST_r$hdr)
magimageWCSRGB(VISTA_K, VST_r, GALEX_NUV, GALEX_NUV$hdr)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ WCS }% use one of  RShowDoc("KEYWORDS")
\keyword{ gnomonic }% __ONLY ONE__ keyword per line
