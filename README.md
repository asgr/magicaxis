
<!-- README.md is generated from README.Rmd. Please edit that file -->

# magicaxis

<!-- badges: start -->

![R-CMD-check](https://github.com/asgr/magicaxis/workflows/R-CMD-check/badge.svg)
<!-- badges: end -->

The goal of magicaxis is provide functions to make pretty axes (major
and minor) on scientific plots. Particularly effort is made on producing
nice log plot outputs. The core function produces pretty axis labelling
in a number of circumstances that are often used in scientific plotting.
There is a higher level interface to a generic plot function that will
usually produce nice plots, even without much though on the users part.

## Installation

You can install the released version of magicaxis from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("magicaxis")
```

And the development version from [GitHub](https://github.com/asgr/magicaxis) with:

``` r
# install.packages("remotes")
remotes::install_github("asgr/magicaxis")
```

## Example

An example of using **magplot**:

``` r
library(magicaxis)
## basic example code
x=10^{1:9}
y=1:9
magplot(log10(x),y,unlog='x')
```

<img src="man/figures/README-example1-1.png" width="100%" />

``` r
magplot(x,y,log='x')
```

<img src="man/figures/README-example1-2.png" width="100%" />

``` r

#Not ideal to have two decades between major labels:

magplot(x,y,log='x',majorn=c(10,5))
```

<img src="man/figures/README-example1-3.png" width="100%" />

``` r
magplot(x,y,log='xy',majorn=c(10,5,5,5),side=1:4)
```

<img src="man/figures/README-example1-4.png" width="100%" />

``` r

#Sometimes it is helpful to focus on where most of the data actually is.
#Using a single value for xlim and ylim sigma clips the data to that range.
#Here a value of 2 means we only show the inner 2-sigma (2% to 98%) range.
#The 'auto' option allows magclip to dynamically estimate a clip value.

temp=cbind(rt(1e3,1.5),rt(1e3,1.5))
magplot(temp)
```

<img src="man/figures/README-example1-5.png" width="100%" />

``` r
magplot(temp, xlim=2, ylim=2)
```

<img src="man/figures/README-example1-6.png" width="100%" />

``` r
magplot(temp, xlim='auto', ylim='auto')
```

<img src="man/figures/README-example1-7.png" width="100%" />

``` r

#Some astronomy related examples (and how to display the solar symbol):

temp=cbind(runif(10,8,12),runif(10,0,5))

magplot(temp[,1:2], xlab=expression(M['\u0298']), ylab=expression(M['\u0298']/Yr), unlog='xy')
```

<img src="man/figures/README-example1-8.png" width="100%" />

``` r

#With z scaling

z=sqrt(9:1)
magplot(x, y, z, log='x', position='topleft')
```

<img src="man/figures/README-example1-9.png" width="100%" />

An example of using **maghist**:

``` r
maghist(rnorm(1e4))
#> [1] "Summary of used sample:"
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -3.347802 -0.680214  0.002396  0.006846  0.682562  3.731962 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 1.004617 1.010325 1.006027 2.016355
#> [1] "Using 10000 out of 10000"
```

<img src="man/figures/README-example2-1.png" width="100%" />

``` r
maghist(rnorm(1e4), xlim=c(-2,4))
#> [1] "Summary of used sample:"
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -1.99729 -0.61235  0.03331  0.06389  0.69167  3.59493 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 0.9504591 0.9671921 0.9747294 1.8847496
#> [1] "Using 9763 out of 10000 (97.63%) data points (237 < xlo & 0 > xhi)"
```

<img src="man/figures/README-example2-2.png" width="100%" />

``` r

#Notice the x-limits are close to -3/3, since  if we ask for xlim=3 (a 3-sigma range)

maghist(rnorm(1e4), xlim=3, verbose = FALSE)
```

<img src="man/figures/README-example2-3.png" width="100%" />

``` r

#The 'auto' option allows magclip to dynamically estimate a clip value (which is similar
#in this case, but need not be in general).

maghist(rnorm(1e4), xlim='auto', verbose = FALSE)
```

<img src="man/figures/README-example2-4.png" width="100%" />

``` r

#Test of log histograms:

testdata=10^(runif(1e3,0,4))
maghist(testdata)
#> [1] "Summary of used sample:"
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>    1.005   10.600   97.882  970.827  842.495 9893.141 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 1888.6374  141.8079  972.3256 3684.0541
#> [1] "Using 1000 out of 1000"
```

<img src="man/figures/README-example2-5.png" width="100%" />

``` r
maghist(testdata,log='x')
#> [1] "Summary of used sample:"
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> 0.001991 1.025305 1.990641 1.978397 2.925567 3.995334 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 1.126441 1.407976 1.335352 1.878412
#> [1] "Using 1000 out of 1000"
```

<img src="man/figures/README-example2-6.png" width="100%" />

``` r
maghist(testdata,log='y')
#> [1] "Summary of used sample:"
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>    1.005   10.600   97.882  970.827  842.495 9893.141 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 1888.6374  141.8079  972.3256 3684.0541
#> [1] "Using 1000 out of 1000"
```

<img src="man/figures/README-example2-7.png" width="100%" />

``` r
maghist(testdata,log='xy')
#> [1] "Summary of used sample:"
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> 0.001991 1.025305 1.990641 1.978397 2.925567 3.995334 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 1.126441 1.407976 1.335352 1.878412
#> [1] "Using 1000 out of 1000"
```

<img src="man/figures/README-example2-8.png" width="100%" />

``` r

maghist(testdata,freq=FALSE)
#> [1] "Summary of used sample:"
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>    1.005   10.600   97.882  970.827  842.495 9893.141 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 1888.6374  141.8079  972.3256 3684.0541
#> [1] "Using 1000 out of 1000"
```

<img src="man/figures/README-example2-9.png" width="100%" />

``` r
maghist(testdata,freq=FALSE,log='x')
#> [1] "Summary of used sample:"
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> 0.001991 1.025305 1.990641 1.978397 2.925567 3.995334 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 1.126441 1.407976 1.335352 1.878412
#> [1] "Using 1000 out of 1000"
```

<img src="man/figures/README-example2-10.png" width="100%" />

``` r
maghist(testdata,freq=FALSE,log='y')
#> [1] "Summary of used sample:"
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>    1.005   10.600   97.882  970.827  842.495 9893.141 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 1888.6374  141.8079  972.3256 3684.0541
#> [1] "Using 1000 out of 1000"
```

<img src="man/figures/README-example2-11.png" width="100%" />

``` r
maghist(testdata,freq=FALSE,log='xy')
#> [1] "Summary of used sample:"
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> 0.001991 1.025305 1.990641 1.978397 2.925567 3.995334 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 1.126441 1.407976 1.335352 1.878412
#> [1] "Using 1000 out of 1000"
```

<img src="man/figures/README-example2-12.png" width="100%" />

``` r

#Test of plotting histogram objects:

testhist=maghist(testdata,log='xy')
#> [1] "Summary of used sample:"
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> 0.001991 1.025305 1.990641 1.978397 2.925567 3.995334 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 1.126441 1.407976 1.335352 1.878412
#> [1] "Using 1000 out of 1000"
```

<img src="man/figures/README-example2-13.png" width="100%" />

``` r
maghist(testhist)
```

<img src="man/figures/README-example2-14.png" width="100%" />

``` r
maghist(testhist,log='x')
```

<img src="man/figures/README-example2-15.png" width="100%" />

``` r
magplot(testhist,log='y')
```

<img src="man/figures/README-example2-16.png" width="100%" />

``` r
magplot(testhist,log='xy')
```

<img src="man/figures/README-example2-17.png" width="100%" />

``` r

#Nice to see a grid with large ranges:

maghist(rnorm(1e6), grid=TRUE)
#> [1] "Summary of used sample:"
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -4.798760 -0.672827  0.000144  0.000752  0.675977  4.768394 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 1.0001413 0.9998855 0.9993710 2.0024501
#> [1] "Using 1000000 out of 1000000"
```

<img src="man/figures/README-example2-18.png" width="100%" />

``` r
maghist(rnorm(1e6), log='y', grid=TRUE)
#> [1] "Summary of used sample:"
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -4.568203 -0.672258  0.002986  0.001804  0.676802  5.046734 
#> [1] "sd / MAD / 1-sig / 2-sig range:"
#> [1] 0.9998797 1.0001524 1.0000371 2.0002543
#> [1] "Using 1000000 out of 1000000"
```

<img src="man/figures/README-example2-19.png" width="100%" />

An example of using **magbin**:

``` r
set.seed(666)
xydata = cbind(rnorm(1e4), rnorm(1e4))
magbin(xydata, shape='hexagon') #default
```

<img src="man/figures/README-example3-1.png" width="100%" />

``` r
magbin(xydata, shape='square')
```

<img src="man/figures/README-example3-2.png" width="100%" />

``` r
magbin(xydata, shape='triangle')
```

<img src="man/figures/README-example3-3.png" width="100%" />

``` r
magbin(xydata, shape='trihex')
```

<img src="man/figures/README-example3-4.png" width="100%" />

``` r
magbin(xydata, shape='hexagon', direction='v')
```

<img src="man/figures/README-example3-5.png" width="100%" />

``` r
magbin(xydata, shape='triangle', direction='v')
```

<img src="man/figures/README-example3-6.png" width="100%" />

``` r
magbin(xydata, shape='trihex', direction='v')
```

<img src="man/figures/README-example3-7.png" width="100%" />

``` r
magbin(xydata, shape='hexagon', step=c(0.2,0.4)) #different aspect ratio hexagons
```

<img src="man/figures/README-example3-8.png" width="100%" />

``` r

magbin(xydata, z=xydata[,1]^2-xydata[,2]^2, colref='zstat', sizeref='count')
```

<img src="man/figures/README-example3-9.png" width="100%" />

``` r

magbin(xydata, z=xydata[,1]^2-xydata[,2]^2, colref='zstat', sizeref='count',
funstat=mad)
```

<img src="man/figures/README-example3-10.png" width="100%" />

``` r
magbin(xydata, z=xydata[,1]^2-xydata[,2]^2, colref='zstat', sizeref='count',
funstat=function(x){quantile(x,0.9)})
```

<img src="man/figures/README-example3-11.png" width="100%" />

``` r

xydata = cbind(10^rnorm(1e4), 10^rnorm(1e4))
magbin(xydata, log='xy')
```

<img src="man/figures/README-example3-12.png" width="100%" />

``` r
magbin(xydata, z=xydata[,1]*xydata[,2], colref='zstat', sizeref='count',
log='xyz')
```

<img src="man/figures/README-example3-13.png" width="100%" />

``` r
magbin(xydata, log='xy', unlog='xy', xlim=3, ylim=3)
```

<img src="man/figures/README-example3-14.png" width="100%" />
