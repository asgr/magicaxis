magcon <-
function(x,y,h,doim=TRUE,docon=TRUE,dobar=TRUE,ngrid=100,add=FALSE,xlab='',ylab='',imcol=c(NA,rev(rainbow(1000,start=0,end=2/3))),conlevels=c(0.5,pnorm(1)-pnorm(-1),0.95), barposition='topright', barorient='v',bartitle='Contained %',bartitleshift=0,xlim=NULL,ylim=NULL,weights=NULL,...){
  if(missing(y)){
    if(!is.null(dim(x))){
      if(dim(x)[2]>=2){y=x[,2];x=x[,1]}
    }
  }
  use=!is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x) & !is.na(y) & !is.nan(y) & !is.null(y) & is.finite(y)
  if(is.null(xlim)){xlim=range(x[use],na.rm=TRUE)}
  if(is.null(ylim)){ylim=range(y[use],na.rm=TRUE)}
  use= use & x>=min(xlim) & x<=max(xlim) & y>=min(ylim) & y<=max(ylim)
  if(is.null(weights)==FALSE){
    if(length(weights)==length(x)){
      weights=weights[use]
    }else{
      stop("weights must match lengths of x / y")
    }
  }
  x=x[use]
  y=y[use]
  conlevels=1-conlevels
  #if(is.null(weights)){
  #  tempcon=MASS::kde2d(x, y, h=h, n=ngrid,lims=c(xlim+c(-diff(xlim),diff(xlim)),ylim+c(-diff(ylim),diff(ylim))))
  #}else{
  
  tempcon=sm.density(cbind(x,y),h=h,weights=weights,display='none',ngrid=ngrid,xlim=xlim+c(-diff(xlim),diff(xlim)),ylim=ylim+c(-diff(ylim),diff(ylim)),verbose=FALSE)
  tempcon$x=tempcon$eval.points[,1]
  tempcon$y=tempcon$eval.points[,2]
  tempcon$z=tempcon$estimate
    
  #}
  temp=sort(tempcon$z)
  tempsum=cumsum(temp)
  convfunc=approxfun(tempsum,temp)
  levelmap=approxfun(convfunc(seq(0,1,len=1000)*max(tempsum)),seq(0,1,len=1000))
  tempcon$z=matrix(levelmap(tempcon$z),nrow=ngrid)
  tempcon$z[is.na(tempcon$z)]=min(tempcon$z,na.rm=TRUE)
  if(doim){
    if(add==FALSE){
      plot.new()
      plot.window(xlim=xlim,ylim=ylim)
      usrlims=par()$usr
      rect(usrlims[1],usrlims[3],usrlims[2],usrlims[4],col=imcol[1])
    }
    magimage(tempcon,col=imcol,axes=FALSE,add=TRUE,xlim=xlim,ylim=ylim,magmap=FALSE)
  }
  if(doim & docon){contour(tempcon,levels=conlevels,add=TRUE,drawlabels=F,axes=FALSE,...)}
  if(doim==FALSE & docon){contour(tempcon,levels=conlevels,add=add,drawlabels=F,axes=FALSE,xlim=xlim,ylim=ylim,...);box()}
  if(add==FALSE){magaxis(xlab=xlab,ylab=ylab)}
  if(doim & dobar){magbar(position=barposition,range=c(0,100),orient=barorient,col=rev(imcol),title=bartitle,titleshift=bartitleshift)}
  return(invisible(tempcon))
}
