\name{magtri}
\alias{magtri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
High level triangle plotting code for MCMC chains
}
\description{
A very high level (minimal options) MCMC chain triangle (AKA corner) plot function. The default is deliberately spartan in terms of options, but the result should be a clear set of covariance plots that should give quick insight into the stationary sampling quality of a set of MCMC posterior chains.
}
\usage{
magtri(chains, samples = 1000, thin = 1, samptype = 'end', grid = FALSE, do.tick = FALSE,
refvals = NULL, lab = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chains}{
A matrix or data.frame of the posterior chains, arranged so that the columns are the parameters and rows are the individual chain samples. The column names are inherited as the parameter names from the input to \code{chains}.
}
  \item{samples}{
Specify the number of sub-samples desired. To speed up plotting it is often a good idea not to plot all chain samples (the reduced set is plotted as the top-left points and used to generate the bottom-right contours). The default plot 1000 samples.
}
  \item{thin}{
Specify the thinning of chain samples, default (1) processes the whole chain.
}
  \item{samptype}{
Specifies whether to take all of the samples from the end of the supplied \code{chains} ('end', the default since samples are usually better towards the end of a set of psoterior chain samples), randomly selected ('ran', should only be used if you are confident the posterior chains supplied are true stationary samples) or evenly selected ('thin', which behaves much like thinning except we specify the target number of outputs, not the fraction of samples kept).
}
  \item{grid}{
Should a background grid be added to the sub-panels? See \code{\link{magaxis}} for details.  
}
   \item{do.tick}{
Logical; should ticks be drawn on each sub-panel? Passed to \code{\link{axis}} argument \option{tick}.  
}
  \item{refvals}{
Numeric vector; this gives reference values to overdraw. If provided it must be the same length as dim(chains)[2], i.e. this would usually be the number of parameters being compared in the triangle plot.
}
  \item{lab}{
Character vector; optional over-ride for column names when plotting the grid of scatter plots.  
}
  \item{\dots}{
Extra arguments are passed to \option{\link{magcon}} for plotting.  
}
}
\details{
    This interface is deliberately very high level with few options. It is really designed to allow quick exploratory views of posterior samples from MCMC chains, and publication grade plots should be designed by the user. That said, in many situations the plots generated are of pleasant, clear and publishable quality.
    
    Other types of data can be plotted using this function of course, but the default setup is tuned towards being useful for MCMC posterior chain samples.
    
    The contour levels shown are the defaults for magcon, i.e. they contain 50\% (lty=2), 68\% (lty=1) and 95\% (lty=3) of the posterior chains.
    
    The red cross shows the mean for the sampled posterior chain. The red vertical dashed line traces this over the contour plots. The red dotted line shows the +/- SD range of the sampled posterior chain.
}
\value{
  Outputs a two column matrix containing the means and standard deviations fo the parameters. Generally run for the side effect of producing nice projected plots.
}
\author{
  Aaron Robotham
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{magcon}}
}
\examples{
Sigma=matrix(c(10,3,-5,3,12,8,-5,8,20),3,3)
chains=MASS::mvrnorm(n=1000, mu=1:3, Sigma=Sigma)
magtri(chains,tick=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{MCMC}
\keyword{mcmc}
\keyword{posterior}
\keyword{triangle}
