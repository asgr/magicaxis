\name{magwarp}
\alias{magwarp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remap Image WCS via Warping
}
\description{
Remaps an input Tan Gnomonic or Sine Orthographic projection projection system to a different target WCS.
}
\usage{
magwarp(image_in, header_out = NULL, header_in = NULL, dim_out, direction = "auto",
boundary = "dirichlet", interpolation = "cubic", doscale = TRUE, CRVAL1_in = 0,
CRVAL2_in = 0, CRPIX1_in = 0, CRPIX2_in = 0, CD1_1_in = 1, CD1_2_in = 0, CD2_1_in = 0,
CD2_2_in = 1, CRVAL1_out = 0, CRVAL2_out = 0, CRPIX1_out = 0, CRPIX2_out = 0,
CD1_1_out = 1, CD1_2_out = 0, CD2_1_out = 0, CD2_2_out = 1, plot = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image_in}{
Numeric matrix; required, the image we want to remap. If \option{image_in} is a list as created by \code{readFITS}, \code{read.fits} of \code{\link{magcutoutWCS}} then the image part of the list is parsed to \option{image_in} and the correct header part is parsed to \option{header_in}.
}
  \item{header_out}{
Full FITS header in table or vector format. This is the target WCS projection that \option{image_in} will be mapped onto. Legal table format headers are provided by the \code{read.fitshdr} function or the \option{hdr} list output of \code{read.fits} in the astro package; the \option{hdr} output of \code{readFITS} in the \code{FITSio} package or the \option{header} output of \code{magcutoutWCS}. If \option{header_out} is provided then key words will be taken from here as a priority. Missing header keywords are printed out and other header option arguments are used in these cases.
}
  \item{header_in}{
Full FITS header in table or vector format. This should be the header WCS that matches \option{image_in}. Legal table format headers are provided by the \code{read.fitshdr} function or the \option{hdr} list output of \code{read.fits} in the astro package; the \option{hdr} output of \code{readFITS} in the \code{FITSio} package or the \option{header} output of \code{magcutoutWCS}. If \option{header_in} is provided then key words will be taken from here as a priority. Missing header keywords are printed out and other header option arguments are used in these cases.
}
  \item{dim_out}{
Integer vector; this defines the desired dimensions of the output image. If this is not provided then the output image is made to be the same size as the NAXIS1 and NAXIS2 arguments taken from \option{header_out} (which is usually what you will want TBH).
}
  \item{direction}{
"auto" (default), "forward" or "backward", see \code{imwarp}. Since it is usally better to go from the higher resolution image and map this onto the lower resolution grid, "auto" selects the better direction given the pixel scales recovered from the header information.
}
  \item{boundary}{
boundary conditions: "dirichlet", "neumann", "periodic" (default "dirichlet"), see \code{imwarp}
}
  \item{interpolation}{
"nearest", "linear", "cubic" (default "linear"), see \code{imwarp}
}
  \item{doscale}{
Logical; if TRUE (default) then the image is scaled by the relative change in the pixel scale. This is usually what you want when converting images between different WCS (since we really want to make sure we conserve overall flux). If FALSE then nothing is done to rescale the image. This is useful when the input \option{image_in} is something like a segmentation map, where the actual values matter.
}
  \item{CRVAL1_in}{
FITS header CRVAL1 for the Tan Gnomonic projection system of the input WCS. This is the RA in degrees at the location of \option{CRPIX1}.
}
  \item{CRVAL2_in}{
FITS header CRVAL2 for the Tan Gnomonic projection system of the input WCS. This is the Dec in degrees at the location of \option{CRPIX2}.
}
  \item{CRPIX1_in}{
FITS header CRPIX1 for the Tan Gnomonic projection system of the input WCS. This is the x pixel value at the location of \option{CRVAL1}.
}
  \item{CRPIX2_in}{
FITS header CRPIX2 for the Tan Gnomonic projection system of the input WCS. This is the y pixel value at the location of \option{CRVAL2}.
}
  \item{CD1_1_in}{
FITS header CD1_1 for the Tan Gnomonic projection system of the input WCS. Change in RA-Tan in degrees along x-Axis.
}
  \item{CD1_2_in}{
FITS header CD1_2 for the Tan Gnomonic projection system of the input WCS. Change in RA-Tan in degrees along y-Axis.
}
  \item{CD2_1_in}{
FITS header CD2_1 for the Tan Gnomonic projection system of the input WCS. Change in Dec-Tan in degrees along x-Axis.
}
  \item{CD2_2_in}{
FITS header CD2_2 for the Tan Gnomonic projection system of the input WCS. Change in Dec-Tan in degrees along y-Axis.
}
  \item{CRVAL1_out}{
FITS header CRVAL1 for the Tan Gnomonic projection system of the output WCS. This is the RA in degrees at the location of \option{CRPIX1}.
}
  \item{CRVAL2_out}{
FITS header CRVAL2 for the Tan Gnomonic projection system of the output WCS. This is the Dec in degrees at the location of \option{CRPIX2}.
}
  \item{CRPIX1_out}{
FITS header CRPIX1 for the Tan Gnomonic projection system of the output WCS. This is the x pixel value at the location of \option{CRVAL1}.
}
  \item{CRPIX2_out}{
FITS header CRPIX2 for the Tan Gnomonic projection system of the output WCS. This is the y pixel value at the location of \option{CRVAL2}.
}
  \item{CD1_1_out}{
FITS header CD1_1 for the Tan Gnomonic projection system of the output WCS. Change in RA-Tan in degrees along x-Axis.
}
  \item{CD1_2_out}{
FITS header CD1_2 for the Tan Gnomonic projection system of the output WCS. Change in RA-Tan in degrees along y-Axis.
}
  \item{CD2_1_out}{
FITS header CD2_1 for the Tan Gnomonic projection system of the output WCS. Change in Dec-Tan in degrees along x-Axis.
}
  \item{CD2_2_out}{
FITS header CD2_2 for the Tan Gnomonic projection system of the output WCS. Change in Dec-Tan in degrees along y-Axis.
}
  \item{plot}{
Logical; should a \code{\link{magimageWCS}} plot of the output be generated? 
}
  \item{\dots}{
Dots are parsed to either \code{\link{magimageWCS}} (only relevant if \option{plot}=TRUE).
}  
}
\details{
The function allows for arbitrary WCS remapping, as long as the input and output WCS both use the Tan Gnomonic projection system (which is by far the most common with modern survey imaging data). The process internally does the following:

\itemize{
  \item xy2radec; maps the input image to RA and Dec per pixel using the input header
  \item radec2xy; maps the pixel RA and Dec coordinates onto the desired output pixel grid using the output header
}  

There are a few different ways to consider doing pixel remapping (or warping). The main question is usually whether to operate on a forward or backwards manner (see \code{imwarp}). Backwards mapping (the default) finds the best interpolation of every output pixel in the input image, whereas forward mapping finds the best interpolation of every input pixel on the output image. Backwards mapping usually provides the fewest artefacts (hence it is the default), but in general if the input image is higher resolution then you might prefer forward mapping and vica-versa.

The actual warping is done using the \code{imwarp} function in the \code{imager} package, so this will need to be installed prior to use (it is available on CRAN).
}
\value{
A list containing:

  \item{image}{Numeric matrix; the remapped image using the target WCS.}
  \item{header}{The target \option{header_out}.}
}
\author{
Aaron Robotham
}
\note{
This function uses a bi-cubic interpolation scheme by default. It should approximately conserve the flux in \option{image_in}, but this is not guaranteed. The 'linear' interpolation scheme may be closer to conserving flux in images with sharp features. The conservation is usually good to about 0.1\% (i.e. 0.01 mag). If you require better conservation than this then alternative schemes (e.g. SWarp) will be required.
}

\seealso{
\code{\link{magimageWCSRGB}}
}
\examples{
\dontrun{
VISTA_K=readFITS(system.file("extdata", 'VISTA_K.fits', package="magicaxis"))
VST_r=readFITS(system.file("extdata", 'VST_r.fits', package="magicaxis"))
GALEX_NUV=readFITS(system.file("extdata", 'GALEX_NUV.fits', package="magicaxis"))

magwarp(VST_r, GALEX_NUV$hdr, plot=TRUE)
magwarp(VISTA_K, GALEX_NUV$hdr, plot=TRUE)

magwarp(GALEX_NUV, VST_r$hdr, plot=TRUE)
magwarp(VISTA_K, VST_r$hdr, plot=TRUE)

magwarp(GALEX_NUV, VISTA_K$hdr, plot=TRUE)
magwarp(VST_r, VISTA_K$hdr, plot=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ WCS }% use one of  RShowDoc("KEYWORDS")
\keyword{ gnomonic }
\keyword{ warp }% __ONLY ONE__ keyword per line
