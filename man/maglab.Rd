\name{maglab}
\alias{maglab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pretty scientific labelling
}
\description{
Utilises pretty for the major-tick locations, but makes prettier decisions if log axes are being used. Translates the default text into nicely formatted expressions- this is particularly successful when axes are logged and exponents are used since formats like 1e5 should not be used in scientific academic journals.
}
\usage{
maglab(lims, n, log=FALSE, exptext = TRUE, crunch = TRUE, logpretty = TRUE,
usemultloc = FALSE, multloc = c(1,2,5), prettybase = 10, powbase = 10, hersh = FALSE,
trim = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lims}{	
Limits over which pretty major-tick locations will be calculated.
}
  \item{n}{
The target number of major-axis sub-divisions. Will not necessarily be achieved.
}
  \item{log}{
Should the limits be evenly distributed over log space. Usually what you want if an axis has been logged.
}
  \item{exptext}{
Should log==TRUE then should the text be written in exponent form (e.g. 10^8, default when exptext==TRUE) or logged (e.g. 8 in this case).
}
  \item{crunch}{
In cases where the scientific text would be written as 1x10^8, should the 1x be removed so it reads 10^8. TRUE by default.
}
  \item{logpretty}{
Should the major-ticks only be located at powers of 10, or when dynamic range is small (less than 50) powers of 10 times 1, 2 and 5. This changes cases where ticks are placed at 1, 3.1, 10, 31, 100 etc to 1, 10, 100.
}
  \item{usemultloc}{
For log=TRUE, if usemultloc=FALSE then label locations are only at powers of 10, if usemultloc=TRUE then they are at multiples of powers of 10 as defined by multloc.
}
  \item{multloc}{
If usemultloc is TRUE then multloc provides the multiples of powers of 10 for the location of labels. Default will give them at 0.1, 0.2, 0.5, 1 etc.
}
  \item{prettybase}{
The unit of repitition desired. By default it is 10, implying a pretty plot is one with marks at 10, 20, 30 etc. If you are plotting degrees then it might be prettier to display 90, 180, 270 etc. In which case prettybase should be set to 90. If log=TRUE then the reference location of 10 is changed, so in the previous example the labels generated would be at 9, 90, 900 etc rather than the deafult of 1, 10, 100 etc.
}
  \item{powbase}{
Set the base to use for logarithmic axes. Default is to use 10.   
}
  \item{hersh}{
Determines whether the text format output by maglab should be Hershey vector font compatable text (TRUE), or normal plotmath style expressions (FALSE).
}
  \item{trim}{
If trim is TRUE the outputs are not allowed to exceed the stated limits, if FALSE then the whole range of pretty values calculated are used. This will usually extend beyond the limits to ensure the plots look pretty, but if maglab is being used for something other than plotting axis labels then trimmed values might be useful.
}
}
\details{
This function is a mid level routine for producing nice ticks and text, with particularly effort on improving the outcome of logged axis cases. The end user will probably not require axis to it except in unusual circumstances. I note that my method of translating the default representation of the exponents is not very elegant, so any suggestions for improvement are welcome!
}
\value{
   \item{tickat}{
   Location of proposed major-tick marks.
}
   \item{labat}{
   Location of proposed label locations (not necessarily the same as major-tick locations).
}
   \item{exp}{
   Expressions to be used at label locations.
}
}

\author{
Aaron Robotham
}

\seealso{
\code{\link{magplot}}, \code{\link{magaxis}}, \code{\link{magerr}}, \code{\link{magmap}}, \code{\link{magrun}}
}

\examples{
x=10^{1:9}
y=1:9
plot(log10(x),y,axes=FALSE)
ticks=maglab(range(x),log=TRUE)
print(ticks)
axis(1,at=log10(ticks$labat),labels=ticks$exp)

# Same outcome a different way:

plot(x,y,axes=FALSE,log='x')
ticks=maglab(range(x),log=TRUE)
print(ticks)
axis(1,at=ticks$labat,labels=ticks$exp)

# For small dynamic range

x=seq(1,40,len=9)
y=1:9
plot(x,y,axes=FALSE,log='x')
ticks=maglab(range(x),log=TRUE,usemultloc=TRUE)
axis(1,at=ticks$labat,labels=ticks$exp,tick=FALSE)
axis(1,at=ticks$tickat,labels=FALSE)

# Different base prettiness

x=0:270
y=sin(x*pi/180)
plot(x,y,axes=FALSE,type='l')
ticks=maglab(range(x))
axis(1,at=ticks$labat,labels=ticks$exp)
# Not very pretty for degree plotting
ticks=maglab(range(x),prettybase=45)
axis(3,at=ticks$labat,labels=ticks$exp)
# Much nicer!

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{plot}
\keyword{axis}% __ONLY ONE__ keyword per line
\keyword{log}
