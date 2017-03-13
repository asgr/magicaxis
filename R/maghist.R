maghist=function(x, breaks = "Sturges", freq = NULL, include.lowest = TRUE, right = TRUE,
                density = NULL, angle = 45, col = NULL, border = NULL, xlim = range(x),
                ylim = NULL, plot = TRUE, verbose=TRUE, ...){
  
  if(!missing(xlim)){
    if(length(xlim)==1){
      xlim=quantile(x,pnorm(c(-xlim,xlim)))
    }
    sel=x>=xlim[1] & x<=xlim[2]
    if(verbose){
      print(paste('Showing ',length(which(sel)),' out of ',length(x),' (',round(100*length(which(sel))/length(x),2),'%) data points!',sep=''))
    }
    x=x[sel]
  }
  
  out=hist(x=x, breaks=breaks, freq=freq, include.lowest=include.lowest, right=right, density=density, angle=angle, col=col, border=border, main='', xlim=xlim, ylim=ylim, xlab='', ylab='', axes=FALSE, plot=plot, labels=FALSE, warn.unused=FALSE)
  if(plot){magaxis(...)}
  return=out
}