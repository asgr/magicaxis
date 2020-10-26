\name{magaxis}
\alias{magaxis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Magically pretty axes
}
\description{
This function generates nicely arranged axes for scientific plots, including minor tick marks. It supports log settings and can unclog axes that have been logged inline by the user. When the dynamic range is 50 or less and axis is logged, axis range factors of 10 times 1, 2 and 5 are used instead of powers of 10 alone.
}
\usage{
magaxis(side = 1:2, majorn = 5, minorn = 'auto', tcl = 0.5, ratio = 0.5, labels = TRUE,
unlog = 'auto', mgp = c(2,0.5,0), mtline = 2, xlab = NULL, ylab = NULL, crunch = TRUE,
logpretty = TRUE, prettybase = 10, powbase = 10, hersh = FALSE, family = 'sans',
frame.plot = FALSE, usepar = FALSE, grid = FALSE, grid.col = 'grey', grid.lty = 1,
grid.lwd = 1, axis.lwd = 1, ticks.lwd = axis.lwd, axis.col = 'black', do.tick = TRUE,
...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{side}{
The side to be used for axis labelling in the same sense as the base axis function (1=bottom, 2=left, 3=top, 4=right). A vector of multiple entries is allowed. By default, bottom and left axes are drawn (i.e. side 1 and 2).
}
  \item{majorn}{
The target number of major-axis sub-divisions for pretty plotting. If length is 1 and length of side is longer than this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. Obvious reason for varying this is different pretty labelling between a and y axes.
}
  \item{minorn}{
The exact number of minor-axis divisions (i.e. desired minor ticks + 1) to display in plotting. Auto will produce \code{\link{pretty}} ticks for linear scaling, and powbase-2 minor ticks for logged (this might seem odd, but for base 10 this means ticks at 2/3/4/5/6/7/8/9, which is probably as desired). If set manually, must be greater than 1 to have a visible effect. Minor ticks are always calculated to be equally spaced in linear space, so tick spaces vary when using log plotting. If length is 1 and length of side is longer than this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. An obvious reason for varying this is different pretty labelling between x and y axes.
}
  \item{tcl}{
The length of major tick marks as a fraction of the height of a line of text. By default these face into the plot (in common with scientific plotting) with a value of 0.5, rather than the R default of -0.5. It is possible to force magaxis to inherit directly from par by setting usepar=TRUE (see below). See \code{\link{par}} for more details.
}
  \item{ratio}{
Ratio of minor to major tick mark lengths.
}
  \item{labels}{
Specifies whether major-axis ticks should be labelled for each axis. If length is 1 and length of side is longer than this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. Default is to label all axes.
}
  \item{unlog}{
Determines if axis labels should be unlogged. If axis is found to be logged in par('usr') then the minor ticks are automatically log spaced, however "unlog" still controls how the labelling is done: either logged form (FALSE) or exponent form (TRUE). If axis has been explicitly logged (e.g. log10(x)) then this will can produce exponential axis marking/ labelling if set to TRUE. This case will also produce log minor tick marks. If length of unlog is 1 and length of side is longer than 1 then the assigned unlog value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. Can also take the text argument 'x', 'y', 'xy' or 'yx', where these refer to which axes have been logged. If left at the default of `Auto' then unlog is assumed to be true when the axis in question is logged, and false otherwise.
}
  \item{mgp}{
The margin line (in mex units) for the axis title, axis labels and axis line. This has different (i.e. prettier) defaults than R of c(2,0.5,0) rather than c(3,1,0). This pushes the numbers and labels nearer to the plot compared to the defaults. It is possible to force magaxis to inherit directly from par by setting usepar=TRUE (see below). See \code{\link{par}} for more details.
}
  \item{mtline}{
Number of lines separating axis name from axis. If length 2 then specifies x and y axis separation respectively (else these are the same).
}
  \item{xlab}{
x axis name.
}
  \item{ylab}{
y axis name.
}
  \item{crunch}{
In cases where the scientific text would be written as 1x10^8, should the 1x be removed so it reads 10^8. If length is 1 and length of side is longer then this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. TRUE by default.
}
  \item{logpretty}{
Should the major-ticks only be located at powers of 10. This changes cases where ticks are placed at 1, 3.1, 10, 31, 100 etc to 1, 10, 100. If length is 1 and length of side is longer then this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. TRUE by default.
}
  \item{prettybase}{
The unit of repitition desired. By default it is 10, implying a pretty plot is one with marks at 10, 20, 30 etc. If you are plotting degrees then it might be prettier to display 90, 180, 270 etc. In which case prettybase should be set to 90. If log=TRUE then the reference location of 10 is changed, so in the previous example the labels generated would be at 9, 90, 900 etc rather than the deafult of 1, 10, 100 etc. If length is 1 and length of side is longer then this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side.
}
  \item{powbase}{
Set the base to use for logarithmic axes. Default is to use 10.
}
  \item{hersh}{
To determines whether all plot text should be passed using Hershey vector fonts. This applies to the axis labels (which are handled automatically) and the axis names. In the case of axis names the user must be careful to use the correct plot utils escape characters: http://www.gnu.org/software/plotutils/manual/en/html_node/Text-String-Format.html.
magaxis will return back to the current plotting family after the function has executed.
}
  \item{family}{
Specifies the plotting family to be used. Allowed options are 'sans' and 'serif'. Depending on whether hersh is TRUE or FALSE these otions are either applied to the Hershey vector fonts (hersh=TRUE) or the default R Helvetica font (hersh=FALSE). magaxis will return back to the current plotting family after the function has executed.
}
  \item{frame.plot}{
Logical indicating whether a box should be drawn around the plot.
}
  \item{usepar}{
Logical indicating whether tcl and mgp should be forced to inherit the global par values. This might be preferred when you want to define global plot settings at the start of a script.
}
  \item{grid}{
Logical indicating whether a background grid should be drawn onto the plotting area. This will only be done for side=1 (i.e. vertical grid lines) and side=2 (i.e. horizontal grid lines). If length is 1 and length of side is longer then this value is used for all axes. If length of arguments is longer than 1 then these should tally with the relevant axes in side. FALSE by default.
}
  \item{grid.col}{
The colour of the grid to be drawn.
}
  \item{grid.lty}{
The line type of the grid to be drawn.
}
  \item{grid.lwd}{
The line width of the grid to be drawn.
}
  \item{axis.lwd}{
The line width of the axis to be drawn. This is passed to \option{lwd} argument in \code{\link{axis}}.
}
  \item{ticks.lwd}{
The line width of the ticks to be drawn. This is passed to \option{ticks.lwd} argument in \code{\link{axis}}.
}
  \item{axis.col}{
Colour argument to pass directly to \option{col} in axis. It is a bit clunky to have to specify this, but the option 'col' clashes too much with line and point colours.  
}
  \item{do.tick}{
Logical; should ticks be drawn? Passed to \code{\link{axis}} argument \option{tick}.  
}
  \item{\dots}{
Other arguments to be passed to base \code{\link{axis}} and \code{\link{mtext}} functions as relevant. Options for axis are as described in \code{\link{axis}}, but note \option{cex.lab}, \option{col.lab} and \option{font.lab} are parsed as \option{cex}, \option{col} and \option{font} into \code{\link{mtext}} as required.
}
}
\details{
This function tries hard to make nice plots for scientific papers.
}
\value{
No output. Run for the side effect of producing nice plotting axes.
}

\author{
Aaron Robotham
}

\seealso{
\code{\link{magplot}}, \code{\link{maglab}}, \code{\link{magerr}}, \code{\link{magmap}}, \code{\link{magrun}}
}

\examples{
x=10^{1:9}
y=1:9
plot(log10(x),y,axes=FALSE)
magaxis(unlog='x')

plot(log10(x),y,axes=FALSE)
magaxis(side=c(1,3),unlog=c(TRUE,FALSE))

plot(x,y,axes=FALSE,log='x')
magaxis()

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{axis}
\keyword{plot}
\keyword{log}
