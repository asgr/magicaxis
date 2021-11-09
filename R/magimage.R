magimage = function(x, y, z, zlim, xlim, ylim, col = grey((0:1e3)/1e3), add = FALSE,
                   useRaster=TRUE, asp=1, magmap=TRUE, locut=0.4, hicut=0.995, flip=FALSE,
                   range=c(0,1), type = "quan", stretch="asinh", stretchscale='auto',
                   bad=NA, clip="", axes=TRUE, frame.plot=TRUE, sparse='auto', qdiff=FALSE, rem_med=FALSE, ...){ 
  dots=list(...)
  dotskeepimage=c('xaxs', 'yaxs', 'breaks', 'oldstyle')
  if(length(dots)>0){
    dotsimage=dots[names(dots) %in% dotskeepimage]
  }else{
    dotsimage={}
  }
  if(!missing(x)){
    if(is.list(x)){
      if('y' %in% names(x)){y=x$y}
      if('z' %in% names(x)){z=x$z}
      if('x' %in% names(x)){x=x$x}
    }
    if(is.matrix(x)){
      z=x
      x=seq(0.5,dim(z)[1]-0.5)
      if(missing(y)){y=seq(0.5,dim(z)[2]-0.5)}
    }
  }
  
  if(!missing(z)){
    if(is.matrix(z)){
      if(missing(x)){x=seq(0.5,dim(z)[1]-0.5)}
      if(missing(y)){y=seq(0.5,dim(z)[2]-0.5)}
    }
  }
  
  if(is.vector(z) & !missing(x)){
    z=matrix(z, nrow=length(x))
    if(missing(y)){y=seq(0.5,dim(z)[2]-0.5)}
  }
  
  if(is.vector(z) & !missing(y)){
    z=matrix(z, ncol=length(y))
    if(missing(x)){x=seq(0.5,dim(z)[1]-0.5)}
  }
  
  if(rem_med){
    z = z - median(z, na.rm=TRUE)
  }
  
  if(missing(xlim) & length(x)==dim(z)[1]){
    xlim=range(x, na.rm=TRUE)+c(-0.5,0.5)*diff(range(x, na.rm=TRUE))/(dim(z)[1]-1)
  }
  if(missing(ylim) & length(y)==dim(z)[2]){
    ylim=range(y, na.rm=TRUE)+c(-0.5,0.5)*diff(range(y, na.rm=TRUE))/(dim(z)[2]-1)
  }
  if(missing(xlim) & length(x)==(dim(z)[1]+1)){
    xlim=range(x, na.rm=TRUE)
  }
  if(missing(ylim) & length(y)==(dim(z)[2]+1)){
    ylim=range(y, na.rm=TRUE)
  }
  if(x[1]>x[length(x)]){x=rev(x); xlim=rev(xlim)}
  if(y[1]>y[length(y)]){y=rev(y); ylim=rev(ylim)}
  if(sparse=='auto'){
    sparse=ceiling(max(dim(z)/1e3))
  }
  if(sparse>1){
    samplex=seq(sparse/2,length(x),by=sparse)
    sampley=seq(sparse/2,length(y),by=sparse)
    x=x[samplex]
    y=y[sampley]
    z=z[samplex,sampley]
  }
  if(qdiff){
    col = hcl.colors(100, 'RdYlBu', rev=TRUE)
    if(missing(hicut)){
      maximg=max(abs(z),na.rm=TRUE)
      locut=-maximg
      hicut=maximg
    }else{
      locut=-hicut
    }
    type='num'
    zlim=c(0,1)
  }
  if(magmap){
    if(type=='quan'){
      if(quantile(z,locut,na.rm=T) != quantile(z,hicut,na.rm=T)){
        z=magmap(data=z, locut=locut, hicut=hicut, flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale, bad=bad, clip=clip)$map
      }else{
        print('Too many same valued pixels: turning off magmap scaling!')
      }
    }else{
      z=magmap(data=z, locut=locut, hicut=hicut, flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale, bad=bad, clip=clip)$map
    }  
  }
  if(missing(zlim)){
    zlim=range(z, na.rm=TRUE)
  }
  do.call('image',c(list(x=x, y=y, z=z, zlim=zlim, xlim=xlim, ylim=ylim, col=col, add=add, useRaster=useRaster, axes=FALSE, asp=asp, xlab='', ylab='', main=''), dotsimage))
  if(add==FALSE){
    if(axes){
      magaxis(...)
      if(frame.plot){box()}
    }
  }
  return=list(x=x, y=y, z=z)
}
