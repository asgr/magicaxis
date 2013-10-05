magcon=function(x,y,h,doim=TRUE,docon=TRUE,dobar=TRUE,n=100,add=FALSE,xlab='',ylab='',imcol=rev(rainbow(1000,start=0,end=2/3)),conlevels=c(0.5,pnorm(1)-pnorm(-1),0.95), barposition='topright', barorient='v',bartitle='Contained %',bartitleshift=0,...){
conlevels=1-conlevels
tempcon=kde2d(x,y,h=h,n=n)
temp=sort(tempcon$z)
tempsum=cumsum(temp)
convfunc=approxfun(tempsum,temp)
levelmap=approxfun(convfunc(seq(0,1,len=1000)*max(tempsum)),seq(0,1,len=1000))
tempcon$z=matrix(levelmap(tempcon$z),nrow=n)
tempcon$z[is.na(tempcon$z)]=min(tempcon$z,na.rm=TRUE)
if(doim){image(tempcon,col=imcol,axes=FALSE,add=add)}
if(doim & docon){contour(tempcon,levels=conlevels,add=TRUE,drawlabels=F,axes=FALSE,...)}
if(doim==FALSE & docon){contour(tempcon,levels=conlevels,add=add,drawlabels=F,axes=FALSE,...)}
if(add==FALSE){magaxis(xlab=xlab,ylab=ylab)}
if(doim & dobar){magbar(position=barposition,range=c(0,100),orient=barorient,col=rev(imcol),title=bartitle,titleshift=bartitleshift)}
return=tempcon
}

