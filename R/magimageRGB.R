magimageRGB<-function(x, y, R, G, B, saturation=1, zlim, xlim, ylim, add = FALSE, useRaster=TRUE, asp=1, magmap=TRUE, locut=0.4, hicut=0.995, flip=FALSE, range=c(0,1), type = "quan", stretch="asinh", stretchscale='auto', bad=range[1], clip="", axes=TRUE, frame.plot=TRUE, sparse='auto', ...){
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
      if('R' %in% names(x)){R=x$R}
      if('G' %in% names(x)){R=x$G}
      if('B' %in% names(x)){R=x$B}
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
  if(x[1]>x[length(x)]){x=rev(x); xlim=rev(xlim)}
  if(y[1]>y[length(y)]){y=rev(y); ylim=rev(ylim)}
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
  if(length(locut)<3){locut=rep(locut,3)}
  if(length(hicut)<3){hicut=rep(hicut,3)}
  if(magmap){
    R=magmap(data=R, locut=locut[1], hicut=hicut[1], flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale, bad=bad, clip=clip)$map
    G=magmap(data=G, locut=locut[2], hicut=hicut[2], flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale, bad=bad, clip=clip)$map
    B=magmap(data=B, locut=locut[3], hicut=hicut[3], flip=flip, range=range, type=type, stretch=stretch, stretchscale=stretchscale, bad=bad, clip=clip)$map
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

magimageWCSRGB=function(R, G, B, header_out, Rheader, Gheader, Bheader, dowarp='auto', direction = "auto", boundary = "dirichlet", interpolation = "cubic", n, grid.col='grey', grid.lty=2, grid.lwd=0.5, lab.col='green', coord.type='sex', margin=TRUE, loc.diff=c(0,0), xlab='Right Ascension', ylab='Declination', mgp=c(2,0.5,0), mtline=2, position='topright', com.col="green", com.length=0.05, coord.axis='auto', pretty='auto', CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, CTYPE1='RA--TAN', CTYPE2='DEC--TAN', ...){
  
  if(missing(xlab)){
    if(coord.type=='sex'){
      xlab=paste(xlab,'/ H:M:S')
    }
    if(coord.type=='deg'){
      xlab=paste(xlab,'/ deg')
    }
  }
  if(missing(ylab)){
    if(coord.type=='sex'){
      ylab=paste(ylab,'/ D:M:S')
    }
    if(coord.type=='deg'){
      ylab=paste(ylab,'/ deg')
    }
  }
  
  if(!missing(R)){
    if(any(names(R)=='imDat')){
      if(missing(Rheader)){
        Rheader=R$hdr
      }
      R=R$imDat
    }
    if(any(names(R)=='dat')){
      if(missing(Rheader)){
        Rheader=R$hdr[[1]]
        Rheader=data.frame(key=Rheader[,1],value=Rheader[,2], stringsAsFactors = FALSE)
      }
      R=R$dat[[1]]
    }
    if(any(names(R)=='image')){
      if(missing(Rheader)){
        Rheader=R$header
      }
      R=R$image
    }
  }
  
  if(!missing(G)){
    if(any(names(G)=='imDat')){
      if(missing(Gheader)){
        Gheader=G$hdr
      }
      G=G$imDat
    }
    if(any(names(G)=='dat')){
      if(missing(Gheader)){
        Gheader=G$hdr[[1]]
        Gheader=data.frame(key=Gheader[,1],value=Gheader[,2], stringsAsFactors = FALSE)
      }
      G=G$dat[[1]]
    }
    if(any(names(G)=='image')){
      if(missing(Gheader)){
        Gheader=G$header
      }
      G=G$image
    }
  }
  
  if(!missing(B)){
    if(any(names(B)=='imDat')){
      if(missing(Bheader)){
        Bheader=B$hdr
      }
      B=B$imDat
    }
    if(any(names(B)=='dat')){
      if(missing(Bheader)){
        Bheader=B$hdr[[1]]
        Bheader=data.frame(key=Bheader[,1],value=Bheader[,2], stringsAsFactors = FALSE)
      }
      B=B$dat[[1]]
    }
    if(any(names(B)=='image')){
      if(missing(Bheader)){
        Bheader=B$header
      }
      B=B$image
    }
  }
  
  if(missing(header_out)){
    if(exists('Rheader')){
      header_out=Rheader
    }else if(exists('Gheader')){
      header_out=Gheader
    }else if(exists('Bheader')){
      header_out=Bheader
    }
  }
  
  if(dowarp=='auto'){
    dowarp=FALSE
    if(all(dim(R)==dim(G))==FALSE){dowarp=TRUE}
    if(all(dim(R)==dim(B))==FALSE){dowarp=TRUE}
    if(all(as.character(Rheader)==as.character(Gheader))==FALSE){dowarp=TRUE}
    if(all(as.character(Rheader)==as.character(Gheader))==FALSE){dowarp=TRUE}
  }
  
  if(dowarp){
    R=magwarp(image_in=R, header_out=header_out, header_in=Rheader, direction=direction, boundary=boundary, interpolation=interpolation)$image
    G=magwarp(image_in=G, header_out=header_out, header_in=Gheader, direction=direction, boundary=boundary, interpolation=interpolation)$image
    B=magwarp(image_in=B, header_out=header_out, header_in=Bheader, direction=direction, boundary=boundary, interpolation=interpolation)$image
  }
  
  output=magimageRGB(R=R, G=G, B=B, axes=FALSE, ...)
  box()
  
  magimageWCSGrid(header=header_out, n=n, grid.col=grid.col, grid.lty=grid.lty, grid.lwd=grid.lwd, coord.type=coord.type, loc.diff=loc.diff, pretty=pretty, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, CTYPE1=CTYPE1, CTYPE2=CTYPE2)
  magimageWCSLabels(header=header_out, n=n, lab.col=lab.col, coord.type=coord.type, margin=margin, loc.diff=loc.diff, xlab=xlab, ylab=ylab, mgp=mgp, mtline=mtline, pretty=pretty, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, CTYPE1=CTYPE1, CTYPE2=CTYPE2)
  magimageWCSCompass(header=header_out, position=position, com.col=com.col, com.length=com.length, loc.diff=loc.diff, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, CTYPE1=CTYPE1, CTYPE2=CTYPE2)
  
  return=output
}
