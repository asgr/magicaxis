magimageRGB<-function(x, y, R, G, B, saturation=1, zlim, xlim, ylim, add = FALSE,
                      useRaster=TRUE, asp=1, magmap=TRUE, locut=0.4, hicut=0.995,
                      flip=FALSE, range=c(0,1), type = "quan", stretch="asinh",
                      stretchscale='auto', bad=range[1], clip="", axes=TRUE, frame.plot=TRUE,
                      sparse='auto', ...){
  dots=list(...)
  dotskeepimage=c('xaxs', 'yaxs', 'breaks', 'oldstyle')
  if(length(dots)>0){
    dotsimage=dots[names(dots) %in% dotskeepimage]
  }else{
    dotsimage={}
  }
  if(!missing(x)){
    if(is.list(x)){
      if('R' %in% names(x)){R=x$R}
      if('G' %in% names(x)){G=x$G}
      if('B' %in% names(x)){B=x$B}
      if('y' %in% names(x)){y=x$y}else{y=seq(0.5,dim(R)[2]-0.5)}
      if('x' %in% names(x)){x=x$x}else{x=seq(0.5,dim(R)[1]-0.5)}
    }
    if(is.array(x)){
      R=x[,,1]
      G=x[,,2]
      B=x[,,3]
      x=seq(0.5,dim(R)[1]-0.5)
      if(missing(y)){y=seq(0.5,dim(R)[2]-0.5)}
    }
  }
  if(!missing(R)){
    if(is.matrix(R)){
      if(missing(x)){x=seq(0.5,dim(R)[1]-0.5)}
      if(missing(y)){y=seq(0.5,dim(R)[2]-0.5)}
    }
  }
  if(missing(xlim) & length(x)==dim(R)[1]){
    xlim=range(x, na.rm=TRUE)+c(-0.5,0.5)*diff(range(x, na.rm=TRUE))/(dim(R)[1]-1)
  }
  if(missing(ylim) & length(y)==dim(R)[2]){
    ylim=range(y, na.rm=TRUE)+c(-0.5,0.5)*diff(range(y, na.rm=TRUE))/(dim(R)[2]-1)
  }
  if(missing(xlim) & length(x)==(dim(R)[1]+1)){
    xlim=range(x, na.rm=TRUE)
  }
  if(missing(ylim) & length(y)==(dim(R)[2]+1)){
    ylim=range(y, na.rm=TRUE)
  }
  if(is.numeric(x)){
    if(x[1]>x[length(x)]){x=rev(x); xlim=rev(xlim)}
  }
  if(is.numeric(y)){
    if(y[1]>y[length(y)]){y=rev(y); ylim=rev(ylim)}
  }
  if(sparse=='auto'){
    sparse=ceiling(max(dim(R)/1e3))
  }
  if(sparse>1){
    samplex=seq(sparse/2,length(x),by=sparse)
    sampley=seq(sparse/2,length(y),by=sparse)
    x=x[samplex]
    y=y[sampley]
    R=R[samplex,sampley]
    G=G[samplex,sampley]
    B=B[samplex,sampley]
  }
  if(missing(zlim)){
    zlim=c(0,length(R))
  }
  if(length(locut)<3){locut=rep(locut[1],3)}
  if(length(hicut)<3){hicut=rep(hicut[1],3)}
  if(length(stretchscale)<3){stretchscale=rep(stretchscale[1],3)}
  if(magmap){
    R=magmap(data=R, locut=locut[1], hicut=hicut[1], flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale[1], bad=bad, clip=clip)$map
    G=magmap(data=G, locut=locut[2], hicut=hicut[2], flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale[2], bad=bad, clip=clip)$map
    B=magmap(data=B, locut=locut[3], hicut=hicut[3], flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale[3], bad=bad, clip=clip)$map
  }
  
  if(saturation!=1){
    Y=(R+G+B)/3
    Ra=Y+saturation*(2*R-G-B)/3
    Ga=Y+saturation*(2*G-R-B)/3
    Ba=Y+saturation*(2*B-R-G)/3
    rm(Y)
    R=Ra
    rm(Ra)
    G=Ga
    rm(Ga)
    B=Ba
    rm(Ba)
  }
  
  # R=R-min(R)
  # R=R/max(R)
  # G=G-min(G)
  # G=G/max(G)
  # B=B-min(B)
  # B=B/max(B)
  
  do.call('image',c(list(x=x, y=y, z=matrix(1:length(R),dim(R)[1]), zlim=zlim, xlim=xlim, ylim=ylim, col=rgb(R,G,B), add=add, useRaster=useRaster, axes=FALSE, asp=asp, xlab='', ylab='', main=''), dotsimage))
  if(add==FALSE){
    if(axes){
      magaxis(...)
      if(frame.plot){box()}
    }
  }
  return=list(x=x, y=y, R=R, G=G, B=B)
}
