\name{magclip}
\alias{magclip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Magical sigma clipping
}
\description{
This function does intelligent autoamtic sigma-clipping of data. This is optionally used by \code{\link{magplot}} and \code{\link{maghist}}.
}
\usage{
magclip(x, sigma='auto', clipiters=5, sigmasel=1, estimate='both')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric; the values to be clipped. This can reasonably be a vector, a matrix or a dataframe.
}
  \item{sigma}{
The level of sigma clipping to be done. If set to default of 'auto' it will dynamically choose a sigma level to cut at based on the length of x (or the clipped version once iterations have started), i.e.: sigma=qnorm(1-2/length(x)). This have the effect of removing unlikely values based on the chance of them occurring, i.e. there is roughly a 50\% chance of a 3.5 / 4.6 sigma Normal fluctuation occurring when you have 10,000 / 10,000,000 values, hence choosing this value dynamically is usually the best option.
}
  \item{clipiters}{
The maximum number of sigma clipping iterations to attempt. It will break out sooner than this if the iterations have converged. The default of 5 is usually plenty (up to the contamination being towards the 50\% level).
}
  \item{sigmasel}{
The quantile to use when trying to estimate the true standard-deviation of the Normal distribution. if contamination is low then the default of 1 is about optimal in terms of S/N, but you might need to make the value lower when contamination is very high.
}
  \item{estimate}{
Character; determines which side/s of the distribution are used to estimate Normal properties. The default is to use both sides (both) giving better S/N, but if you know that your contamination only comes from positive flux sources (e.g., astronomical data when trying to select sky pixels) then you should only use the lower side to determine Normal statistics (lo). Similarly if the contamination is on the low side then you should use the higher side to determine Normal statistics (hi).
}
}
\details{
If you know more sepcific details about your data then you should probably carry out a thorough likelihood analysis, but the ad-hoc clipping done in \code{magclip} works pretty well in practice.
}
\value{
A list containing three items:

\item{x}{Numeric vector; the cliped \option{x} values.}
\item{clip}{Locial; logic of which values were clipped with the same type and shape attributes as the input \option{x} (i.e. if the original \option{x} was a matrix then \option{clip} would also be a matrix that matches element to element).}
\item{range}{The data range of clipped \option{x} values returned.}
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{maghist}}, \code{\link{magplot}}
}
\examples{
#A highly contaminated Normal distribution:
temp=c(rnorm(1e3),runif(500,-10,10))
magplot(density(temp))
lines(seq(-5,5,len=1e3),dnorm(seq(-5,5,len=1e3)),col='red')

magplot(density(magclip(temp)$x))
lines(seq(-5,5,len=1e3),dnorm(seq(-5,5,len=1e3)),col='red')

#Now we put the contamination on the high side:

temp=c(rnorm(1e3),runif(500,0,10))
magplot(density(magclip(temp)$x))
lines(seq(-5,5,len=1e3),dnorm(seq(-5,5,len=1e3)),col='red')

magplot(density(magclip(temp, estimate='lo')$x))
lines(seq(-5,5,len=1e3),dnorm(seq(-5,5,len=1e3)),col='red')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sigma }% use one of  RShowDoc("KEYWORDS")
\keyword{ clip }% __ONLY ONE__ keyword per line
