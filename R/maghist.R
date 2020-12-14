maghist=function(x, breaks = "Sturges", freq = TRUE, include.lowest = TRUE, right = TRUE,
                density = NULL, angle = 45, col = NULL, border = NULL, xlim = NULL,
                ylim = NULL, plot = TRUE, verbose=TRUE, add=FALSE, log='', unlog=log, scale=1, cumsum=FALSE,
                p.test=NULL, ...){
  
  if(!inherits(x,'histogram')){
    if(!is.null(xlim)){
      if(length(xlim)==1){
        sel= !is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)
        if (log[1] == "x" | log[1] == "xy" | log[1] == "yx") {
          sel= sel & x>0
        }
        xlim=magclip(x[sel], sigma=xlim)$range
      }
      sel=x>=min(xlim) & x<=max(xlim) & !is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)
    }else{
      if(is.numeric(breaks) & length(breaks)>1){
        xlim=range(breaks)
        sel=x>=min(xlim) & x<=max(xlim) & !is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)
        if (log[1] == "x" | log[1] == "xy" | log[1] == "yx") {
          breaks=log10(breaks)
        }
      }else{
        sel=!is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)
      }
    }
    
    if (log[1] == "x" | log[1] == "xy" | log[1] == "yx"){
      sel= sel & x>0
      xtemp=x[sel]
      xtemp=log10(xtemp)
    }else{
      xtemp=x[sel]
    }
    
    outsum=summary(xtemp)
    sdmad1q2q=c(as.numeric(sd(xtemp)), as.numeric(mad(xtemp)), as.numeric(diff(quantile(xtemp,pnorm(c(-1,1)),na.rm = TRUE)))/2, as.numeric(diff(quantile(xtemp,pnorm(c(-2,2)),na.rm = TRUE)))/2)
    
    if(verbose){
      print('Summary of used sample:')
      print(outsum)
      print('sd / MAD / 1-sig / 2-sig range:')
      print(sdmad1q2q)
      if(!is.null(xlim)){
        print(paste('Using ',length(which(sel)),' out of ',length(x),' (',round(100*length(which(sel))/length(x),2),'%) data points (',length(which(x<min(xlim))),' < xlo & ',length(which(x>max(xlim))),' > xhi)',sep=''))
      }else{
        print(paste('Using ',length(which(sel)),' out of ',length(x),sep=''))
      }
      if(!is.null(p.test)){
        print(p.test(x[sel]))
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
  
  out$counts[is.na(out$counts)]=0
  out$density[is.na(out$density)]=0
  
  out$counts=out$counts*scale
  
  if(cumsum){
    out$counts=cumsum(out$counts)
    out$density=cumsum(out$density)
  }
  
  if(is.null(xlim)){
    xlim=range(out$breaks)
  }else{
    if (log[1] == "x" | log[1] == "xy" | log[1] == "yx"){
      xlim=log10(xlim)
    }
  }
  if (log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
    out$counts=log10(out$counts)
    out$density=log10(out$density)
    out$counts[is.infinite(out$counts)]=NA
    out$density[is.infinite(out$density)]=NA
  }
  
  if(is.null(ylim)){
    if(freq){
      if(log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
        ylim=c(min(out$counts,na.rm = TRUE),max(out$counts,na.rm = TRUE))
      }else{
        ylim=c(0,max(out$counts,na.rm = TRUE))
      }
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
    if(log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
      if(max(ylim)-min(ylim)<1.5){
        ylim[which.min(ylim)]=ylim[which.max(ylim)]-1.5
      }
    }
    if(add==FALSE){
      magplot(x=1, y=1, type='n', xlim=xlim, ylim=ylim, unlog=unlog, ...)
      if(log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
        lims=par()$usr
        lims[3:4]=lims[3:4]-min(ylim)
        par(usr=lims)
        out$counts=out$counts-min(ylim)
        out$density=out$density-min(ylim)
        plot(out, freq=freq, density=density, angle=angle, col=col, border=border, add=TRUE)
        lims[3:4]=lims[3:4]+min(ylim)
        par(usr=lims)
        out$counts=out$counts+min(ylim)
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
      if(log[1] == "y" | log[1] == "xy" | log[1] == "yx"){
        lims=par()$usr
        lims[3:4]=lims[3:4]-min(ylim)
        par(usr=lims)
        out$counts=out$counts-min(ylim)
        out$density=out$density-min(ylim)
        plot(out, freq=freq, density=density, angle=angle, col=col, border=border, add=TRUE)
        lims[3:4]=lims[3:4]+min(ylim)
        par(usr=lims)
        out$counts=out$counts+min(ylim)
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
  if(!inherits(x,'histogram')){
    out=c(out, summary=list(outsum), ranges=list(sdmad1q2q))
    class(out)='histogram'
  }else{
    out=out
  }
  return=out
}