magplot <-
function(x, y, log='', main='', side = 1:2, majorn = 5, minorn = 'auto', tcl = 0.5, ratio = 0.5, labels = TRUE, unlog = "auto", mgp = c(2,0.5,0), mtline = 2, xlab = '', ylab = '', crunch = TRUE, logpretty = TRUE, prettybase = 10, powbase=10, hersh = FALSE, family = "sans", frame.plot=TRUE, usepar=FALSE, grid=FALSE, grid.col='grey', grid.lty=1, grid.lwd=1, axes=TRUE, xlim=NULL, ylim=NULL, ...){

dots=list(...)
dotargs=names(dots)
  
if(class(x)[1]=='histogram'){
  do.call('maghist',c(list(x=x, xlim=xlim, ylim=ylim, log=log, side = side, majorn = majorn, minorn = minorn, tcl = tcl, ratio = ratio, labels = labels, mgp = mgp, mtline = mtline, xlab = xlab, ylab = ylab, crunch = crunch, logpretty = logpretty, prettybase = prettybase, powbase=powbase, hersh = hersh, family = family, frame.plot = frame.plot, usepar = usepar, grid=grid, grid.col=grid.col, grid.lty=grid.lty, grid.lwd=grid.lwd), dots))
}else{
  
  if(missing(y)){
    if(is.data.frame(x) | is.matrix(x)){
      if(ncol(x)>2){
        x=as.data.frame(x)
        plot(x, ...)
      }else{
        if (length(xlim) == 1) {
          sel = !is.na(x[,1]) & !is.nan(x[,1]) & !is.null(x[,1]) & is.finite(x[,1])
          if (log[1] == "x" | log[1] == "xy" | log[1] == "yx") {
            sel = sel & x[,1] > 0
          }
          #xlim = as.numeric(quantile(x[sel,1], pnorm(c(-xlim, xlim)), na.rm = TRUE))
          xlim=magclip(x[sel,1], sigma=xlim)$range
          dots[['xlim']]=xlim
        }
        if (length(ylim) == 1) {
          sel = !is.na(x[,2]) & !is.nan(x[,2]) & !is.null(x[,2]) & is.finite(x[,2])
          if (log[1] == "y" | log[1] == "xy" | log[1] == "yx") {
            sel = sel & x[,2] > 0
          }
          #ylim = as.numeric(quantile(x[sel,2], pnorm(c(-ylim, ylim)), na.rm = TRUE))
          ylim=magclip(x[sel,2], sigma=ylim)$range
          dots[['ylim']]=ylim
        }
        plot(x=x, axes=FALSE, xlab='', ylab='', main=main, log=log, frame.plot=FALSE, panel.first = if(side[1] !=FALSE | axes==FALSE){magaxis(side = side, majorn = majorn, minorn = minorn, tcl = tcl, ratio = ratio, labels = labels, unlog = unlog, mgp = mgp, mtline = mtline, xlab = xlab, ylab = ylab, crunch = crunch, logpretty = logpretty, prettybase = prettybase, powbase=powbase, hersh = hersh, family = family, frame.plot = frame.plot, usepar = usepar, grid=grid, grid.col=grid.col, grid.lty=grid.lty, grid.lwd=grid.lwd, ...)}, xlim=xlim, ylim=ylim, ...)
      }
    }else{
      plot(x=x, axes=FALSE, xlab='', ylab='', main=main, log=log, frame.plot=FALSE, panel.first = if(side[1] !=FALSE | axes==FALSE){magaxis(side = side, majorn = majorn, minorn = minorn, tcl = tcl, ratio = ratio, labels = labels, unlog = unlog, mgp = mgp, mtline = mtline, xlab = xlab, ylab = ylab, crunch = crunch, logpretty = logpretty, prettybase = prettybase, powbase=powbase, hersh = hersh, family = family, frame.plot = frame.plot, usepar = usepar, grid=grid, grid.col=grid.col, grid.lty=grid.lty, grid.lwd=grid.lwd, ...)}, xlim=xlim, ylim=ylim, ...)
    }
  }else{
    if (length(xlim) == 1) {
      sel = !is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)
      if (log[1] == "x" | log[1] == "xy" | log[1] == "yx") {
        sel = sel & x > 0
      }
      #xlim = as.numeric(quantile(x[sel], pnorm(c(-xlim, xlim)), na.rm = TRUE))
      xlim=magclip(x[sel], sigma=xlim)$range
      dots[['xlim']]=xlim
    }
    if (length(ylim) == 1) {
      sel = !is.na(y) & !is.nan(y) & !is.null(y) & is.finite(y)
      if (log[1] == "y" | log[1] == "xy" | log[1] == "yx") {
        sel = sel & y > 0
      }
      #ylim = as.numeric(quantile(y[sel], pnorm(c(-ylim, ylim)), na.rm = TRUE))
      ylim=magclip(y[sel], sigma=ylim)$range
      dots[['ylim']]=ylim
    }
    plot(x=x, y=y, axes=FALSE, xlab='', ylab='', main=main, log=log, frame.plot=FALSE,  panel.first = if(side[1] !=FALSE | axes==FALSE){magaxis(side = side, majorn = majorn, minorn = minorn, tcl = tcl, ratio = ratio, labels = labels, unlog = unlog, mgp = mgp, mtline = mtline, xlab = xlab, ylab = ylab, crunch = crunch, logpretty = logpretty, prettybase = prettybase, powbase=powbase, hersh = hersh, family = family, frame.plot = frame.plot, usepar = usepar, grid=grid, grid.col=grid.col, grid.lty=grid.lty, grid.lwd=grid.lwd, ...)}, xlim=xlim, ylim=ylim, ...)
  }
}
}
