maghist=function(x, breaks = "Sturges", freq = NULL, include.lowest = TRUE, right = TRUE,
                density = NULL, angle = 45, col = NULL, border = NULL, xlim = range(x),
                ylim = NULL, plot = TRUE, verbose=TRUE, add=FALSE, log='', ...){
  
  if(!missing(xlim)){
    if(length(xlim)==1){
      xlim=quantile(x,pnorm(c(-xlim,xlim)))
    }
    sel=x>=xlim[1] & x<=xlim[2] & !is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)
  }else{
    if(is.numeric(breaks) & length(breaks)>1){
      xlim=range(breaks)
      sel=x>=xlim[1] & x<=xlim[2] & !is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)
    }else{
      sel=!is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)
    }
  }
  
  if(verbose & any(sel==FALSE)){
    if(plot){
      print(paste('Plotting ',length(which(sel)),' out of ',length(x),' (',round(100*length(which(sel))/length(x),2),'%) data points (',length(which(x<xlim[1])),' < xlo & ',length(which(x>xlim[2])),' > xhi)',sep=''))
    }else{
      print(paste('Selecting ',length(which(sel)),' out of ',length(x),' (',round(100*length(which(sel))/length(x),2),'%) data points (',length(which(x<xlim[1])),' < xlo & ',length(which(x>xlim[2])),' > xhi)',sep=''))
    }
  }
    x=x[sel]
  
  if (log[1] == "x" | log[1] == "xy" | log[1] == "yx"){
    x=log10(x)
  }
  
  out=hist(x=x, breaks=breaks, freq=freq, include.lowest=include.lowest, right=right, main='', xlim=xlim, ylim=ylim, xlab='', ylab='', plot=FALSE, warn.unused=FALSE)
  
  if (log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
    out$counts=log10(out$counts)
    out$density=log10(out$density)
    out$counts[is.infinite(out$counts)]=NA
    out$density[is.infinite(out$density)]=NA
  }
  
  if(missing(ylim)){
    ylim=c(0,max(out$counts,na.rm = TRUE))
  }else{
    if (log[1] == "y" | log[1] == "xy" | log[1] == "yx"){ylim=log10(ylim)}
  }
  
  if(plot){
    if(add==FALSE){
      plot(x=out$mids, y=out$counts, type='n', axes=FALSE, xlab='', ylab='', main='', xlim=xlim, ylim=ylim)
      magaxis(...)
      plot(out,density=density, angle=angle, col=col, border=border, add=TRUE)
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
    }else{
      if (log[1] == "x" | log[1] == "xy" | log[1] == "yx"){
        lims=par("usr")
        par(xlog=FALSE)
        par(usr=lims)
      }
      if (log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
        lims=par("usr")
        par(ylog=FALSE)
        par(usr=lims)
      }
      plot(out,density=density, angle=angle, col=col, border=border, add=TRUE)
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
    }
  }
  return=out
}