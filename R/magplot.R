magplot <-
function(x, y, log='', xlab=NULL, ylab=NULL, unlog='Auto', majorn=5, minorn=5, main='', labels=TRUE, crunch=TRUE, logpretty=TRUE, prettybase=10, hersh=FALSE, family='sans',...){
if(missing(y)) plot(x,axes=F,xlab='',ylab='',main=main,log=log,...)
else plot(x,y,axes=F,xlab='',ylab='',main='',log=log,...)
magaxis(majorn=majorn,minorn=minorn,xlab=xlab,ylab=ylab,labels=labels,unlog=unlog,box=T,crunch=crunch,logpretty=logpretty,prettybase=prettybase,hersh=hersh,family=family)
}
