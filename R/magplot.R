magplot <-
function(x, y, log='', main='', side = 1:2, majorn = 5, minorn = 5, tcl = 0.5, ratio = 0.5, labels = TRUE, unlog = "Auto", tline = 0.5, mtline = 2, xlab = NULL, ylab = NULL, crunch = TRUE, logpretty = TRUE, prettybase = 10, hersh = FALSE, family = "sans", frame.plot=TRUE, ...){
if(missing(y)) plot(x, axes=F, xlab='', ylab='', main=main, log=log, frame.plot=FALSE, ...)
else plot(x, y, axes=F, xlab='', ylab='', main=main, log=log, frame.plot=FALSE, ...)

#x, y, log='', xlab=NULL, ylab=NULL, unlog='Auto', majorn=5, minorn=5, main='', labels=TRUE, crunch=TRUE, logpretty=TRUE, prettybase=10, hersh=FALSE, family='sans',side=1:2,frame.plot=TRUE,...
#side = 1:4, majorn = 5, minorn = 5, tcl = 0.5, ratio = 0.5, labels = TRUE, unlog = "Auto", tline = 0.5, mtline = 2, xlab = NULL, ylab = NULL, box = FALSE, crunch = TRUE, logpretty = TRUE, prettybase = 10, hersh = FALSE, family = "sans"
#currentbty=par()$bty
#if(all(side %in% c(1,2,3,4) & all(c(1,2,3,4) %in% side))){par(bty='o')}
#if(all(side %in% c(1,2) & all(c(1,2) %in% side))){par(bty='L')}
#if(all(side %in% c(3,4) & all(c(3,4) %in% side))){par(bty='7')}
#if(all(side %in% c(1,2,3) & all(c(1,2,3) %in% side))){par(bty='C')}
#if(all(side %in% c(1,2,4) & all(c(1,2,4) %in% side))){par(bty='U')}
#if(all(side %in% c(1,3,4) & all(c(1,3,4) %in% side))){par(bty=']')}

magaxis(side = side, majorn = majorn, minorn = minorn, tcl = tcl, ratio = ratio, labels = labels, unlog = unlog, tline = tline, mtline = mtline, xlab = xlab, ylab = ylab, crunch = crunch, logpretty = logpretty, prettybase = prettybase, hersh = hersh, family = family, frame.plot = frame.plot)
#par(bty=currentbty)
}
