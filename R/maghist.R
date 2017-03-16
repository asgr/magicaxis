maghist=function(x, breaks = "Sturges", freq = TRUE, include.lowest = TRUE, right = TRUE,
                density = NULL, angle = 45, col = NULL, border = NULL, xlim = NULL,
                ylim = NULL, plot = TRUE, verbose=TRUE, add=FALSE, log='', ...){
  
  if(!class(x)=='histogram'){
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
  out=hist(x=x, breaks=breaks, include.lowest=include.lowest, right=right, plot=FALSE, warn.unused=FALSE)
  }else{
    out=x
    if (log[1] == "x" | log[1] == "xy" | log[1] == "yx"){
      out$breaks=log10(out$breaks)
      out$mids=log10(out$mids)
    }
  }
  
  if(missing(xlim)){
    xlim=range(out$breaks)
  }
  if (log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
    out$counts=log10(out$counts)
    out$density=log10(out$density)
    out$counts[is.infinite(out$counts)]=NA
    out$density[is.infinite(out$density)]=NA
  }
  
  #   if(missing(xlim)){
  #     xlim=range(out$breaks)
  #   }
  #   if (log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
  #     out$counts=log10(out$counts)
  #     out$density=log10(out$density)
  #     out$counts[is.infinite(out$counts)]=NA
  #     out$density[is.infinite(out$density)]=NA
  #   }
  # }else{
  #   if(missing(xlim)){
  #     xlim=range(x$breaks)
  #   }
  #   out=x
  #   if (log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
  #     out$counts[is.infinite(out$counts)]=NA
  #     out$density[is.infinite(out$density)]=NA
  #   }
  # }
  
  if(missing(ylim)){
    if(freq){
      ylim=c(0,max(out$counts,na.rm = TRUE))
    }else{
      if(log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
        ylim=c(min(out$density,na.rm = TRUE),max(out$density,na.rm = TRUE))
      }else{
        ylim=c(0,max(out$density,na.rm = TRUE))
      }
    }
  }else{
    if (log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
      ylim=log10(ylim)
    }
  }
  
  if(plot){
    if(add==FALSE){
      magplot(x=1, y=1, type='n', xlim=xlim, ylim=ylim, unlog=log, ...)
      if(freq==FALSE & (log[1] == "y" | log[1] == "xy" | log[1] == "yx")){
        lims=par()$usr
        lims[3:4]=lims[3:4]-min(ylim)
        par(usr=lims)
        out$density=out$density-min(ylim)
        plot(out, freq=freq, density=density, angle=angle, col=col, border=border, add=TRUE)
        lims[3:4]=lims[3:4]+min(ylim)
        par(usr=lims)
        out$density=out$density+min(ylim)
      }else{
        plot(out, freq=freq, density=density, angle=angle, col=col, border=border, add=TRUE)
      }
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
      if(freq==FALSE & (log[1] == "y" | log[1] == "xy" | log[1] == "yx")){
        lims=par()$usr
        lims[3:4]=lims[3:4]-min(ylim)
        par(usr=lims)
        out$density=out$density-min(ylim)
        plot(out, freq=freq, density=density, angle=angle, col=col, border=border, add=TRUE)
        lims[3:4]=lims[3:4]+min(ylim)
        par(usr=lims)
        out$density=out$density+min(ylim)
      }else{
        plot(out, freq=freq, density=density, angle=angle, col=col, border=border, add=TRUE)
      }
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
  if (log[1] == "x" | log[1] == "xy" | log[1] == "yx"){
    out$breaks=10^out$breaks
    out$mids=10^out$mids
  }
  if (log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
    out$counts=10^out$counts
    out$density=10^out$density
  }
  return=out
}