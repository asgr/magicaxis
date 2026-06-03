\name{dust_getval}
\alias{dust_getval}
\alias{dust_getval_resolve_coords}
\alias{dust_getval_resolve_ipath}
\alias{dust_getval_map_files}
\alias{dust_getval_load_library}
\alias{dust_getval_compile_library}
\alias{dust_getval_format_output}
\alias{dust_getval_write_output}
\title{
Dust map lookup helpers
}
\description{
Utilities for querying the Schlegel, Finkbeiner and Davis dust maps and for
preparing the supporting inputs, outputs and shared-library interface used by
that lookup.
}
\usage{
dust_getval(gall = NULL, galb = NULL,
  map = c("Ebv", "I100", "X", "T", "mask"),
  ipath = system.file("extdata", package = "magicaxis"),
  interp = FALSE, noloop = FALSE, verbose = FALSE,
  infile = NULL, outfile = NULL,
  code_dir = system.file("src", package = "magicaxis"),
  shared_object = NULL, rebuild = FALSE)

dust_getval_resolve_coords(gall = NULL, galb = NULL, infile = NULL)

dust_getval_resolve_ipath(ipath = NULL)

dust_getval_map_files(map, ipath)

dust_getval_load_library(code_dir = system.file("src", "", package = "magicaxis"),
  shared_object = NULL, rebuild = FALSE)

dust_getval_compile_library(code_dir, shared_object)

dust_getval_format_output(coords, values, map)

dust_getval_write_output(parsed, map, outfile)
}
\arguments{
  \item{gall}{
Galactic longitude in degrees, supplied either as a numeric vector or through
the first column of \code{infile}.
}
  \item{galb}{
Galactic latitude in degrees, supplied either as a numeric vector or through
the second column of \code{infile}.
}
  \item{map}{
The map product to query. Supported values are \code{"Ebv"}, \code{"I100"},
\code{"X"}, \code{"T"} and \code{"mask"}.
}
  \item{ipath}{
Directory containing the north and south SFD FITS map files.
}
  \item{interp}{
Logical; should the lookup interpolate between map pixels? Interpolation is
disabled automatically for the \code{"mask"} map.
}
  \item{noloop}{
Logical flag passed through to the native lookup code.
}
  \item{verbose}{
Logical flag passed through to the native lookup code.
}
  \item{infile}{
Optional path to a text file whose first two columns are galactic longitude and
latitude.
}
  \item{outfile}{
Optional path to a text file that receives the formatted lookup output.
}
  \item{code_dir}{
Directory containing the native C sources and, by default, the built shared
object.
}
  \item{shared_object}{
Optional path to the compiled shared library used by \code{dust_getval}.
}
  \item{rebuild}{
Logical; should the shared library be rebuilt before loading?
}
  \item{coords}{
Coordinate data frame produced by \code{dust_getval_resolve_coords}.
}
  \item{values}{
Numeric vector returned by the native lookup routine.
}
  \item{parsed}{
Parsed data frame produced by \code{dust_getval_format_output}.
}
}
\details{
\code{dust_getval} is the main user-facing lookup wrapper. It validates the
requested map, resolves input coordinates, loads or rebuilds the native shared
library and then calls the C implementation that reads the SFD FITS maps.

The remaining functions are lower-level helpers. They resolve coordinate input,
discover map files, manage compilation and loading of the shared object, format
the native output into R data frames and optionally write legacy text output
files.
}
\value{
\code{dust_getval} returns a data frame containing \code{gall}, \code{galb}
and the requested dust value column, or decoded mask columns when
\code{map = "mask"}.

\code{dust_getval_resolve_coords} returns a coordinate data frame.
\code{dust_getval_resolve_ipath} returns a character scalar path.
\code{dust_getval_map_files} and \code{dust_getval_load_library} return named
lists.
\code{dust_getval_compile_library} is called for its side effect of producing a
shared object.
\code{dust_getval_format_output} returns a parsed data frame.
\code{dust_getval_write_output} writes a text file and returns \code{NULL}
invisibly.
}
\author{
Angus H Wright
}
\examples{
dust_getval_resolve_coords(gall = c(10, 20), galb = c(-5, 5))
dust_getval_resolve_ipath(NULL)
dust_getval(gall = 120, galb = 45, map = "Ebv")
}
\keyword{dust}
\keyword{utilities}
