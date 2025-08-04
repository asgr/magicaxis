\name{magerr}
\alias{magerr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Error bar plotting
}
\description{
A function to add x and y error bars to plots. Low and high error bars can be generated.
}
\usage{
magerr(x, y, xlo, ylo, xhi = xlo, yhi = ylo, corxy, length = 0.02,
col = 'black', fill = FALSE, poly = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric vector; x location of data.
}
  \item{y}{
Numeric vector; y location of data.
}
  \item{xlo}{
Numeric vector; error on the low side for x values. This can be positive or negative- the absolute vaue is used. If length 1, then will be repeated length(\option{x}) times.
}
  \item{ylo}{
Numeric vector; error on the low side for y values. This can be positive or negative- the absolute vaue is used. If length 1, then will be repeated length(\option{x}) times.
}
  \item{xhi}{
Numeric vector; error on the high side for x values. This can be positive or negative- the absolute vaue is used. By default this will inherit the xlo value. If length 1, then will be repeated length(\option{x}) times.
}
  \item{yhi}{
Numeric vector; error on the high side for y values. This can be positive or negative- the absolute vaue is used. By default this will inherit the ylo value. If length 1, then will be repeated length(\option{x}) times.
}
  \item{corxy}{
Numeric vector; if this parameter exists then error ellipses will be drawn instead of error bars. It takes the value of the sigma_x sigma_y correlation, i.e. corxy=covxy/(xlo*ylo). If length 1, then will be repeated length(\option{x}) times.
}
  \item{length}{
Numeric vector; length of error bar ends. If length 1, then will be repeated length(\option{x}) times.
}
  \item{col}{
Either the colour of the error bars or the outline colour of the error ellipses.  If length 1, then will be repeated length(\option{x}) times.
}
  \item{fill}{
Logical; if TRUE then the error ellipses will be filled, if FALSE then only the border will be drawn.
}
  \item{poly}{
Logical; is FALSE then error bars or ellipses will be drawn, if TRUE then approximate error polygon will be shown instead.
}
  \item{\dots}{
Further arguments to be passed to the \code{\link{arrows}} / \code{\link[plotrix]{draw.ellipse}} / \code{\link{polygon}} functions used to draw the error bars / error ellipses (\option{corxy} not missing) / error polygon (\option{poly}=TRUE).
}
}
\details{
Note that with \option{poly}=TRUE the x values are used igoring any error terms, and the point value y errors are used to define the limits of the polygon, with straight lines joining the points. The \option{col} option is used to fill the polygon with a colour (so the default black is probably not a great choice). The \code{\link{polygon}} function takes the argument \option{border} (parsed by dots from the \code{magerr} function) to colour the outer lines, so for a more subtle error polygon you might want to use \option{col}=lightgrey, \option{border}=NA, where NA means no outer border lines are drawn.
}
\value{
Called for the side effect of plotting error bars.
}

\author{
Aaron Robotham
}

\seealso{
\code{\link{magplot}}, \code{\link{magaxis}}, \code{\link{maglab}}, \code{\link{magmap}}, \code{\link{magrun}}, \code{\link{arrows}}, \code{\link[plotrix]{draw.ellipse}}, \code{\link{polygon}}
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

