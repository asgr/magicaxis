\name{magbar}
\alias{magbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pretty colour bar
}
\description{
This function is a high level interface to the plotrix 'color.legend' function. It makes reasonable assumptions on the plottin window to place the colour and allows the user to specify log spacing for the colour gradient and labels, as well as add a title.
}
\usage{
magbar(position = "topright", range = c(0, 1), orient = "v", log = FALSE,
  col = hcl.colors(21), scale = c(1/4, 1/20), inset = 1/40, labN = 5, title = "",
  titleshift = 0, centrealign = "rb", clip = '', cex=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{position}{
Relative position of the colour bar. This argument is used like the 'legend' function. Specify one of 'bottom', 'bottomleft', 'left', 'topleft', 'top', 'topright', 'right', 'bottomright' and 'centre'.
}
  \item{range}{
The text label limits used to label the colour bar.
}
  \item{orient}{
Orientation. Allowed options are 'v' for vertical and 'h' for horizontal.
}
  \item{log}{
Should the colour spacing and labelling be log spaced.
}
  \item{col}{
Colour palette to use for the colouring of the bar.
}
  \item{scale}{
The relative (to the plot window) length and width of the colour bar.
}
  \item{inset}{
Relative (to the plot window) inset of the colour bar.
}
  \item{labN}{
The number of text labels to draw on the colour bar.
}
  \item{title}{
Optional title (or axis label for the labels) to add to the colour bar.
}
  \item{titleshift}{
Extra shift to apply to the 'title' position.
}
  \item{centrealign}{
Option to control the labeling position used when the position='centre'.
}
  \item{clip}{
Setting clip='bg' will set values outside the 'range' values to be blank on the magbar (i.e. you can see through to the background).
}
  \item{cex}{
Character expansion factor for the labels.  
}
  \item{\dots}{
Other arguments to pass to the \code{\link{color.legend}} function.
}
}
\details{
This function creates pretty default colour bars by assessing the current plot window. It is a higher level implementation of the plotrix 'color.legend' function.
}
\value{
Called for the side effect of plotting a colour bar.
}

\author{
Aaron Robotham
}

\seealso{
\code{\link{magplot}}, \code{\link{magaxis}}, \code{\link{maglab}}, \code{\link{magmap}}, \code{\link{magrun}}
}

\examples{
magplot(sin)
magbar('top')
magbar('right',title='Just looking',titleshift=0.5)
magbar('topleft',orient='h',title='Hello!')
magbar('bottom',range=c(0.3,30),orient='h',log=TRUE,title='Log test col')
magbar('bottomleft',range=c(0.3,30),orient='v',log=TRUE,title='Log test bg',clip='bg')

}
\keyword{bar}
\keyword{legend}

