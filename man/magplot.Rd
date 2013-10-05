\name{magplot}
\alias{magplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Magically pretty plots
}
\description{
Makes scientific plots based on magaxis axes. Particularly designed for log plotting. Utilises base plot for the most part, but the axis drawing is replaced by a call to the magaxis fuction.
}
\usage{
magplot(x, y, log='', xlab=NULL, ylab=NULL, unlog='Auto', majorn=5, minorn=5,
main='', labels=TRUE, crunch=TRUE, logpretty=TRUE, prettybase=10, hersh=FALSE,
family='sans',...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{	
The x coordinates of points in the plot. Alternatively, a single plotting structure, function or any R object with a plot method can be provided.
}
  \item{y}{
The y coordinates of points in the plot, optional if x is an appropriate structure.
}
  \item{log}{
log axis arguments to be passed to plot. E.g. use 'x', 'y', 'xy' or 'yx' as appropriate. Default '' assumes no logging of any axes.
}
  \item{xlab}{
If desired, label for x-axis. Default produces no label.
}
  \item{ylab}{
If desired, label for y-axis. Default produces no label.
}
  \item{unlog}{
unlog argument to be passed to magaxis. If axis has been explicitly logged (e.g. log10(x)) then this will produce logged axis marking/ labelling if set to TRUE. If length is 1 then this value is used for all axes. Otherwise should be of length 4 (for the 4 sides: c(bottom,left,top,right)). Can also take the text argument 'x', 'y', 'xy' or 'yx', where these refer to which axes have been logged.
}
  \item{majorn}{
The target number of major axis sub-divisions for pretty plotting. If length is 1 and length of side is longer than this value is used for all axes. Otherwise should be of length 4 (for the 4 sides: c(bottom,left,top,right)). Obvious reason for varying this is different pretty labelling between x and y axes.
}
  \item{minorn}{
The target number of major axis sub-divisions for pretty plotting. If length is 1 and length of side is longer than this value is used for all axes. Otherwise should be of length 4 (for the 4 sides: c(bottom,left,top,right)). Obvious reason for varying this is different pretty labelling between x and y axes.
}
  \item{main}{
Title for the plot. Default is no title.
}
  \item{labels}{
labels argument to be passed to magaxis. Specifies whether major-axis ticks should be labelled for each axis. If length is 1 then this value is used for all axes. Otherwise should be of length 4 (for the 4 sides: c(bottom,left,top,right)). Default is to label all axes.
}
  \item{crunch}{
In cases where the scientific text would be written as 1x10^8, should the 1x be removed so it reads 10^8. TRUE by default.If length is 1 then this value is used for all axes. Otherwise should be of length 4 (for the 4 sides: c(bottom,left,top,right)).
}
  \item{logpretty}{
Should the major-ticks only be located at powers of 10. This changes cases where ticks are placed at 1, 3.1, 10, 31, 100 etc to 1, 10, 100. If length is 1 then this value is used for all axes. Otherwise should be of length 4 (for the 4 sides: c(bottom,left,top,right)). TRUE by default.
}
  \item{prettybase}{
The unit of repitition desired. By default it is 10, implying a pretty plot is one with marks at 10, 20, 30 etc. If you are plotting degrees then it might be prettier to display 90, 180, 270 etc. In which case prettybase should be set to 90. If log=TRUE then the reference location of 10 is changed, so in the previous example the labels generated would be at 9, 90, 900 etc rather than the deafult of 1, 10, 100 etc. If length is 1 and length of side is longer then this value is used for all axes. Otherwise should be of length 4 (for the 4 sides: c(bottom,left,top,right)).
}
  \item{hersh}{
To determines whether all plot text should be passed using Hershey vector fonts. This applies to the axis labels (which are handled automatically) and the axes names. In the case of axis names the user must be careful to use the correct plot utils escape characters: http://www.gnu.org/software/plotutils/manual/en/html_node/Text-String-Format.html.
magaxis will return back to the current plotting family after the function has executed.}
  \item{family}{
Specifies the plotting family to be used. Allowed options are 'sans' and 'serif'. Depending on whether hersh is TRUE or FALSE these otions are either applied to the Hershey vector fonts (hersh=TRUE) or the default R Helvetica font (hersh=FALSE). magaxis will return back to the current plotting family after the function has executed.
}
  \item{\dots}{
Further arguments to be passed to base plot.
}
}
\details{
This is a simple function that just turns off most of the plotting output of base plot, and replaces where possible those present in magaxis.
}
\value{
No output. Run for the side effect of producing nice plotting axes.
}

\author{
Aaron Robotham
}

\seealso{
\code{\link{magaxis}},\code{\link{maglab}},\code{\link{magerr}},\code{\link{magmap}},\code{\link{magrun}}
}

\examples{
x=10^{1:9}
y=1:9
magplot(log10(x),y,unlog='x')

magplot(x,y,log='x')

magplot(x,y,log='x',minorn=c(10,10,5,5))

magplot(x,y,log='xy',minorn=c(10,5,5,5))

#Some astronomy related examples:

temp=cbind(runif(10,8,12),runif(10,0,5))

magplot(temp[,1:2], xlab=expression(M['\u0298']), ylab=expression(M['\u0298']/Yr), unlog='xy')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{plot}
\keyword{axis}% __ONLY ONE__ keyword per line
\keyword{log}
