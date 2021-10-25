\name{magrun}
\alias{magrun}
\title{
Running averages
}
\description{
Computes running averages (medians / means / modes), user defined quantiles and standard deviations for x and y scatter data.
}
\usage{
magrun(x, y, bins = 10, type='median', ranges = pnorm(c(-1, 1)), binaxis = "x",
equalN = TRUE, xcut, ycut, log = '', Nscale = FALSE, diff = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Data x coordinates. This can be a 1D vector (in which case y is required) or a 2D matrix or data frame, where the first two columns will be treated as x and y.
}
  \item{y}{
Data y coordinates, optional if x is an appropriate structure.
}
  \item{bins}{
If a single integer value, how many bins the data should be split into. If a vector is provoided then these values are treated as the explicit bin limits to use.
}
  \item{type}{
The type of running average to determine. Options are 'median' (the default), 'mean', 'mode' and 'mode2d'. 'median' calculates the median for binned x and y values. 'mean' calculates the mean for binned x and y values. 'mode' uses the default R 'density' function, and finds the mode of the resulting smoothed 1D distributions for binned x and y values. 'mode2d' uses the MASS package 'kde2d' function, and finds the mode of the resulting smoothed 2d distribution for binned x and y values. 'cen' just calucates the geometric centre of the bin in x and y directions and is useful for using in conjuction with another 'type' option for plotting purposes. 'mean', 'mode' and 'mode2d' should be used with some thought if 'log' is used, since the central values will be determined for the logged data, which may or may not be desired.
}
  \item{ranges}{
The quantile ranges desired, can set to NULL if quantiles are not desired. The default adds 1-sigma equivilant quantile ranges.
}
  \item{binaxis}{
Which axis to bin across. Must be set to 'x' or 'y'.
}
  \item{equalN}{
Should the data be split into bins with equal numbers of objects (default, TRUE), or into regular spaces from min to max (FALSE). Only relevant if 'bins' paramter is set to a single integer value and 'magrun' is determining the explicit bin limits automatically.
}
  \item{xcut}{
A two element vector containing optional lower and upper x limits to apply to the data.
}
  \item{ycut}{
A two element vector containing optional lower and upper y limits to apply to the data.
}
  \item{log}{
Specify axes that should be logged. Allowed arguments are 'x', 'y' and 'xy'
}
  \item{Nscale}{
Sets whether the quantile ranges and standard deviations calculated are reduced with respect to the median by the square-root of the number of contributing data within each bin. The result of setting Nscale to TRUE is to scale the data like you are calculating the error-in-the-mean, rather than the scatter. For describing the 'significance' of trends in scatter data this is often what you want to show.
}
  \item{diff}{
Should the output quantiles and standard deviations be expressed as differences from the chosen type of running avergage (TRUE) or the actual values (default, FALSE). The advantage of the former is plotting the results as errorbars using magerr, which expects differences (so error like values). If set to TRUE then the output of 'xsd' and 'ysd' is a 1D vector rather than a data.frame with x/y-sd and x/y+sd columns. See the examples below for usage guidance.
}
}
\details{
This function will be default calculate the running median along the x axis for y values, it is intended to be used to trace the spread in scattered data.
}
\value{
  \item{x}{
The chosen averages (default median) of the x bins.
}
  \item{y}{
The chosen averages (default median) of the y bins.
}
  \item{xquan}{
Matrix containing the extra user defined x quantile ranges (columns are in the same order as the requested quantiles). If Nscale is set to TRUE then this is also divided by sqrt the contributing objects in each bin.
}
  \item{yquan}{
Matrix containing the extra user defined y quantile ranges (columns are in the same order as the requested quantiles). If Nscale is set to TRUE then this is also divided by sqrt the contributing objects in each bin.
}
  \item{xsd}{
The standard deviations in the x bins. This is a two column data.frame if 'diff' is set to FALSE, giving the x-sd and x+sd values, or a single vector if 'diff' is set to TRUE. If Nscale is set to TRUE then this is also divided by sqrt the contributing objects in each bin.
}
  \item{ysd}{
The standard deviations in the y bins. This is a two column data.frame if 'diff' is set to FALSE, giving the y-sd and y+sd values, or a single vector if 'diff' is set to TRUE. If Nscale is set to TRUE then this is also divided by sqrt the contributing objects in each bin.
}
  \item{bincen}{
The bin centres used in the chosen binning direction.
}
  \item{binlim}{
The bin limits used in the chosen binning direction.
}
  \item{Nbins}{
The number of items contributing to each running bin. This effectively produces a histogram counts output for the final bin limits.
}
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{magplot}}, \code{\link{magaxis}}, \code{\link{maglab}}, \code{\link{magerr}}, \code{\link{magmap}}
}
\examples{
#Simple example

temp=cbind(seq(0,2,len=1e4),rnorm(1e4))
temprun=magrun(temp)
magplot(temp,col='lightgreen',pch='.')
lines(temprun,col='red')
lines(temprun$x,temprun$yquan[,1],lty=2,col='red')
lines(temprun$x,temprun$yquan[,2],lty=2,col='red')
temprun=magrun(temp,binaxis='y')
lines(temprun,col='blue')
lines(temprun$xquan[,1],temprun$y,lty=2,col='blue')
lines(temprun$xquan[,2],temprun$y,lty=2,col='blue')

#Now with a gradient- makes it clear why the axis choice matters for simple line fitting.

temp=cbind(seq(0,2,len=1e4),rnorm(1e4)+1+seq(0,2,len=1e4))
temprun=magrun(temp)
magplot(temp,col='lightgreen',pch='.')
lines(temprun,col='red')
lines(temprun$x,temprun$yquan[,1],lty=2,col='red')
lines(temprun$x,temprun$yquan[,2],lty=2,col='red')
temprun=magrun(temp,binaxis='y')
lines(temprun,col='blue')
lines(temprun$xquan[,1],temprun$y,lty=2,col='blue')
lines(temprun$xquan[,2],temprun$y,lty=2,col='blue')

#Compare the different centres.

temp=cbind(seq(0,2,len=1e4),rnorm(1e4)^2+seq(0,2,len=1e4))
temprunmedian=magrun(temp,type='median')
temprunmean=magrun(temp,type='mean')
temprunmode=magrun(temp,type='mode')
temprunmode2d=magrun(temp,type='mode2d')
magplot(temp,col='grey',pch='.',ylim=c(-2,5))
lines(temprunmedian,col='red')
lines(temprunmean,col='green')
lines(temprunmode,col='blue')
lines(temprunmode2d,col='orange')

#Choose your own bins.

temp=cbind(seq(0,2,len=1e4),rnorm(1e4)+1+seq(0,2,len=1e4))
temprun=magrun(temp,bins=c(0.1,0.5,0.7,1.2,1.3,2))
magplot(temp,col='lightgreen',pch='.')
points(temprun,col='red')

#Show the 'error in the mean' type data points. Comparing to the best fit line,
#it is clear they are much more meaningful at reflecting the error in the trend seen,
#but not the distribution (or scatter) of data around this.

temp=cbind(seq(0,2,len=1e3),rnorm(1e3)+1+seq(0,2,len=1e3))
temprun=magrun(temp,bins=5)
temprunNscale=magrun(temp,bins=5,Nscale=TRUE)
magplot(temp,col='lightgreen',pch='.')
magerr(temprun$x,temprun$y,temprun$x-temprun$xquan[,1], temprun$y-temprun$yquan[,1],
temprun$xquan[,2]-temprun$x, temprun$yquan[,2]-temprun$y, lty=2,length=0,col='blue')
magerr(temprunNscale$x,temprunNscale$y,temprunNscale$x-temprunNscale$xquan[,1],
temprunNscale$y-temprunNscale$yquan[,1],temprunNscale$xquan[,2]-temprunNscale$x,
temprunNscale$yquan[,2]-temprunNscale$y,col='red')
abline(lm(temp[,2]~temp[,1]),col='black')

#Or the above type of plot can be done more simply using the 'diff' flag.

temprun=magrun(temp,bins=5,diff=TRUE)
temprunNscale=magrun(temp,bins=5,Nscale=TRUE,diff=TRUE)
magplot(temp,col='lightgreen',pch='.')
magerr(temprun$x,temprun$y,temprun$xquan[,1], temprun$yquan[,1], temprun$xquan[,2],
temprun$yquan[,2],lty=2,length=0,col='blue')
magerr(temprunNscale$x,temprunNscale$y,temprunNscale$xquan[,1], temprunNscale$yquan[,1],
temprunNscale$xquan[,2],temprunNscale$yquan[,2],col='red')
abline(lm(temp[,2]~temp[,1]),col='black')

#Similar, but using the 'sd' output.

magplot(temp,col='lightgreen',pch='.')
magerr(temprun$x,temprun$y,temprun$xsd,temprun$ysd,lty=2,length=0,col='blue')
magerr(temprunNscale$x,temprunNscale$y,temprunNscale$xsd,temprunNscale$ysd,col='red')
abline(lm(temp[,2]~temp[,1]),col='black')
}

\keyword{median}
\keyword{mean}
\keyword{mode}
\keyword{quantile}
