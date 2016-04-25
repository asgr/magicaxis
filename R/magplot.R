magplot <-
function(x, y, log='', main='', side = 1:2, majorn = 5, minorn = 'auto', tcl = 0.5, ratio = 0.5, labels = TRUE, unlog = "auto", mgp = c(2,0.5,0), mtline = 2, xlab = '', ylab = '', crunch = TRUE, logpretty = TRUE, prettybase = 10, powbase=10, hersh = FALSE, family = "sans", frame.plot=TRUE, usepar=FALSE, grid=FALSE, grid.col='grey', grid.lty=1, grid.lwd=1, axes=TRUE, ...){
  
if(missing(y)) plot(x, axes=FALSE, xlab='', ylab='', main=main, log=log, frame.plot=FALSE, panel.first = if(side[1] !=FALSE | axes==FALSE){magaxis(side = side, majorn = majorn, minorn = minorn, tcl = tcl, ratio = ratio, labels = labels, unlog = unlog, mgp = mgp, mtline = mtline, xlab = xlab, ylab = ylab, crunch = crunch, logpretty = logpretty, prettybase = prettybase, powbase=powbase, hersh = hersh, family = family, frame.plot = frame.plot, usepar = usepar, grid=grid, grid.col=grid.col, grid.lty=grid.lty, grid.lwd=grid.lwd, ...)}, ...)
else plot(x, y, axes=FALSE, xlab='', ylab='', main=main, log=log, frame.plot=FALSE,  panel.first = if(side[1] !=FALSE | axes==FALSE){magaxis(side = side, majorn = majorn, minorn = minorn, tcl = tcl, ratio = ratio, labels = labels, unlog = unlog, mgp = mgp, mtline = mtline, xlab = xlab, ylab = ylab, crunch = crunch, logpretty = logpretty, prettybase = prettybase, powbase=powbase, hersh = hersh, family = family, frame.plot = frame.plot, usepar = usepar, grid=grid, grid.col=grid.col, grid.lty=grid.lty, grid.lwd=grid.lwd, ...)}, ...)


}
