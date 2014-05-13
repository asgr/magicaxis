\name{magmap}
\alias{magmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Value remapper
}
\description{
This function allows the use to remap a vector of values onto a different system. For instance you might have values stretching from -10 to 100 which you want mapped from 0 to 2/3 so you can then sue the output as an input for point colour or size. It allows clipping of values, rejection of bad values, and log stretching.
}
\usage{
magmap(data, lo = 0, hi = 1, flip = FALSE, range = c(0, 2/3), type = "quan",
log = FALSE, bad = NA, clip='')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A vector of values. This can contain bad values (NA, NaN, infinite), but these will be ignored during mapping and set to the value of input parameter 'bad'.
}
  \item{lo}{
The low limit to clip the data at (what this means varies depending on the 'type' option).
}
  \item{hi}{
The high limit to clip the data at (what this means varies depending on the 'type' option).
}
  \item{flip}{
Should the scaling be flipped. This allows numbers from 0 to 10 to be mapped from 1 to 0 (so ordered back to front with respect to the input).
}
  \item{range}{
The range of the output mapping.
}
  \item{type}{
The type of mapping attempted. Options are 'quan' (default), 'num', 'sig' and 'rank'. 'quan' means the lo and hi options are interpreted as the quantile limits to clip the data at (so lo=0.05 and hi 0.95 would clip the data at the 5\% and 95\% quantile limits and scale values between these). 'num' interprets lo and hi as the exact values to clip the data at and scale between. 'sig' treats lo and hi as the sigma offsets in a Normal distribution, with the probabilities at these positions used to clip and scale that data (so lo=-1 and hi=1 is interpretted as +/- 1 sigma, so the data is clipped and scaled at the 16\% and 84\% levels, i.e. the 1 sigma range). 'rank' means the data mapping is done by rank value only, with lo and hi specifying the quantile limits used to clip and scale the ranks. In all cases lo and hi clipped values are set to the relevant extreme values of 'range'.
}
  \item{log}{
If TRUE log mapping is used. If range is 1 and 100 and log=FALSE the midpoint in the mapping will be 50.5, if log=TRUE the midpoint becomes 10. This enhances the local dynamic range of the mapping for data that has a logarithmic distribution.
}
  \item{bad}{
Sets the value that NA, NaN and infinite input data should be set to in the final map output.
}
  \item{clip}{
By default clipped values inherit the nearest lo/hi value (depending on which side they are clipped). Setting clip='NA' will set values outside the 'lo' and 'hi' values to be NA (currently this is the only other clip option).
}
}

\value{
   \item{map}{
The remapped data. This is the same length and order as the input data.
}
   \item{datalim}{
The a vector of the low and high limits actually applied to the data. Unless type='num' this will probably be different to 'lo' and 'hi'.
}
    \item{maplim}{
The output range (same is the requested input range, but included for book-keeping).
}
    \item{loclip}{
The fraction of objects clipped from the input data at the low end.
}
    \item{hiclip}{
The fraction of objects clipped from the input data at the high end.
}
}

\author{
Aaron Robotham
}

\examples{
temp=cbind(runif(100),runif(100))
temp=cbind(temp,sqrt(temp[,1]^2+temp[,2]^2))
magplot(temp)
magplot(temp[,1:2],col=hsv(h=magmap(temp[,3])$map))
# A different mapping type:
magplot(temp[,1:2],col=hsv(h=magmap(temp[,3],type='rank')$map))
# Flipped:
magplot(temp[,1:2],col=hsv(h=magmap(temp[,3],flip=TRUE,type='rank')$map))
# Example of log mapping:
temp=cbind(temp,10^temp[,3])
magplot(temp[,1:2],col=hsv(h=magmap(temp[,4])$map))
magplot(temp[,1:2],col=hsv(h=magmap(temp[,4],log=TRUE)$map))
#Combination of mapping:
magplot(temp[,1:2],col=hsv(h=magmap(temp[,4],log=TRUE)$map),
cex=magmap(temp[,3],lo=0.5,hi=1,range=c(1,6),type='num')$map)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{mapping}

