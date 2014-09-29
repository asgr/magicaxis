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
magerr(x, y, xlo, ylo, xhi = xlo, yhi = ylo, corxy, length = 0.02,
col = 'black',fill=FALSE,...)
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
  \item{corxy}{
If this paramter exists then error ellipses will be drawn instead of error bars. It takes the value of the sigma_x sigma_y correlation, i.e. corxy=covxy/(xlo*ylo).  
}
  \item{length}{
Length of error bar ends.
}
  \item{col}{
Either the colour of the error bars or the outline colour of the error ellipses.  
}
  \item{fill}{
If fill=TRUE then the error ellipses will be filled, if FALSE then only the border will be drawn.
}
  \item{\dots}{
Further arguments to be passed to the arrows / draw.ellipse functions used to draw the error bars / error ellipses.
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
temp=cbind(x=runif(10),y=runif(10),xerr=runif(10,0.05,0.2),yerr=runif(10,0.1,0.3),
corxy=runif(10,-1,1))
magplot(temp[,1:2])
magerr(x=temp[,1],y=temp[,2],xlo=temp[,3],ylo=temp[,4])
# Example of errors on plots wityh log axes
magplot(temp[,1:2],log='xy')
magerr(x=temp[,1],y=temp[,2],xlo=temp[,3],ylo=temp[,4])

#Example of error ellipses

magplot(temp[,1:2])
magerr(x=temp[,1],y=temp[,2],xlo=temp[,3],ylo=temp[,4])
magerr(x=temp[,1],y=temp[,2],xlo=temp[,3],ylo=temp[,4],corxy=temp[,5])

}
\keyword{error}

