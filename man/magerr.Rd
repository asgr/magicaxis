\name{magerr}
\alias{magerr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Error bar plotting
}
\description{
A function to dd x and y error bars to plots. Low and high error bars can be generated.
}
\usage{
magerr(x, y, xlo = FALSE, ylo = FALSE, xhi = xlo, yhi = ylo, log = "",
length=0.02, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x location of data.
}
  \item{y}{
y location of data.
}
  \item{xlo}{
Error on the low side for x values. This can be positive or negative- the absolute vaue is used.
}
  \item{ylo}{
Error on the low side for y values. This can be positive or negative- the absolute vaue is used.
}
  \item{xhi}{
Error on the high side for x values. This can be positive or negative- the absolute vaue is used. By default this will inherit the xlo value.
}
  \item{yhi}{
Error on the high side for y values. This can be positive or negative- the absolute vaue is used. By default this will inherit the ylo value.
}
  \item{log}{
Set which axes are logged. Allowed values are 'x', 'y', 'xy', 'yx' or '' (default, which means axes are not logged). This is not strictly required, but it is used to catch errors that drop below 0, setting them instead to a very small value that will ensure a bar is still drawn.
}
  \item{length}{
Length of error bar ends.
}
  \item{\dots}{
Further arguments to be passed to the arrows function used to draw the error bars.
}
}

\value{
Called for the side effect of plotting error bars.
}

\author{
Aaron Robotham
}

\seealso{
\code{\link{magplot}},\code{\link{magaxis}},\code{\link{maglab}},\code{\link{magmap}},\code{\link{magrun}}
}

\examples{
# Basic x and y errors added to plot
temp=cbind(runif(10),runif(10),runif(10,0.05,0.2),runif(10,0.1,0.3))
magplot(temp[,1:2])
magerr(x=temp[,1],y=temp[,2],xlo=temp[,3],ylo=temp[,4])
# Example of errors on plots wityh log axes
magplot(temp[,1:2],log='xy')
magerr(x=temp[,1],y=temp[,2],xlo=temp[,3],ylo=temp[,4],log='xy')
}
\keyword{error}

