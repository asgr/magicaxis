maghist=function(x, breaks = "Sturges", freq = NULL, include.lowest = TRUE, right = TRUE,
                density = NULL, angle = 45, col = NULL, border = NULL, xlim = range(x),
                ylim = NULL, plot = TRUE, verbose=TRUE, add=FALSE, log='', ...){
  
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
  
  if (log[1] == "x" | log[1] == "xy" | log[1] == "yx") {
    x=log10(x)
  }
  
  out=hist(x=x, breaks=breaks, freq=freq, include.lowest=include.lowest, right=right, density=density, angle=angle, col=col, border=border, main='', xlim=xlim, ylim=ylim, xlab='', ylab='', plot=FALSE, warn.unused=FALSE)
  
  if (log[1] == "y" | log[1] == "xy" | log[1] == "yx") {
    out$counts=log10(out$counts)
    out$density=log10(out$density)
    out$counts[is.infinite(out$counts)]=NA
    out$density[is.infinite(out$density)]=NA
    ylim=c(1,max(out$counts,na.rm = TRUE))
  }else{
    ylim=c(0,max(out$counts,na.rm = TRUE))
  }
  
  if(plot){
    if(add==FALSE){
      plot(x=out$mids, y=out$counts, type='n', axes=FALSE,xlab='',ylab='',main='',ylim=ylim)
      plot(out,axes=FALSE, labels=FALSE, add=TRUE)
      if (log[1] == "x" | log[1] == "xy" | log[1] == "yx"){
        lims=par("usr")
        par(xlog=TRUE)
        par(usr=lims)
      }
      if (log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
        lims=par("usr")
        par(ylog=TRUE)
        par(usr=lims)
      }
      magaxis(...)
    }else{
      plot(out,axes=FALSE, labels=FALSE, add=TRUE)
    }
  }
  return=out
}