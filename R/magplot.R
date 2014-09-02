magplot <-
function(x, y, log='', xlab=NULL, ylab=NULL, unlog='Auto', majorn=5, minorn=5, main='', labels=TRUE, crunch=TRUE, logpretty=TRUE, prettybase=10, hersh=FALSE, family='sans',side=1:2,frame.plot=TRUE,...){
if(missing(y)) plot(x,axes=F,xlab='',ylab='',main=main,log=log,frame.plot=frame.plot,...)
else plot(x,y,axes=F,xlab='',ylab='',main=main,log=log,frame.plot=frame.plot,...)

#currentbty=par()$bty
#if(all(side %in% c(1,2,3,4) & all(c(1,2,3,4) %in% side))){par(bty='o')}
#if(all(side %in% c(1,2) & all(c(1,2) %in% side))){par(bty='L')}
#if(all(side %in% c(3,4) & all(c(3,4) %in% side))){par(bty='7')}
#if(all(side %in% c(1,2,3) & all(c(1,2,3) %in% side))){par(bty='C')}
#if(all(side %in% c(1,2,4) & all(c(1,2,4) %in% side))){par(bty='U')}
#if(all(side %in% c(1,3,4) & all(c(1,3,4) %in% side))){par(bty=']')}

magaxis(side=side,majorn=majorn,minorn=minorn,xlab=xlab,ylab=ylab,labels=labels,unlog=unlog,box=F,crunch=crunch,logpretty=logpretty,prettybase=prettybase,hersh=hersh,family=family)
#par(bty=currentbty)
}
