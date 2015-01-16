\name{magimage}
\alias{magimage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Image a matrix using magmap scaling
}
\description{
This function uses the magmap scalings to create images that are scaled in
a desirable manner. The function also accepts a data frame containing astrometry
parameters, and uses this to make accurate plot limits (useful when imaging
astronomical photometry), assuming a TAN Gnomonic projection.
}
\usage{
magimage<-function(mat, astr=NULL, main="", xlab=ifelse(is.null(astr),"X (pix)","RA (deg)"), ylab=ifelse(is.null(astr),"Y (pix)","Dec (deg)"), side=1:4, labels=c(T,T,F,F), ...) {
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mat}{ The matrix to image }
\item{astr}{ A data frame containing the named values of astrometry parameters: ra0, dec0, x0, y0, xscale, yscale. These parameters are used in determining the plot axis limits }
\item{main}{ The main title for the plot }
\item{xlab}{ The x-axis label for the plot. Default is to use RA when astrometry is provided, otherwise use X }
\item{ylab}{ The y-axis label for the plot. Default is to use Dec when astrometry is provided, otherwise use Y }
\item{side}{ The magaxis side argument; which sides to plot axes on }
\item{labels}{ The magaxis labels argument; which sides to plot axes labels on }
\item{...}{ Arguments to be passed to the magmap function }

}
\details{
This function uses magmap scaling when creating an image of a matrix.
}
\value{
Returns the mapped matrix
}

\author{
Angus H. Wright
}

\seealso{
\code{\link{magplot}},\code{\link{magaxis}},\code{\link{magmap}}
}

\examples{
#Create matrix of random noise
mat<-matrix(rnorm(1E4),100,100)
#Add a signal
amp=4
sig=5
mat<-mat+amp*exp(-((matrix(-49:50,100,100)^2/(2*sig^2))+(matrix(-49:50,100,100,T)^2/(2*sig^2))))
#Plot
magimage(mat)
#Plot using magmap asinh scaling
magimage(mat, stretch='asinh')

#Display Axes using input astrometry values
astr<-data.frame(ra0=170.0,dec0=-2.5,x0=20,y0=80,xscale=2E-4,yscale=2E-4)
magimage(mat, astr, stretch='asinh')

}
\keyword{image}

