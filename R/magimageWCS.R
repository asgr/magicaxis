magimageWCS=function(image, header, n, grid.col='grey', grid.lty=2, grid.lwd=0.5, lab.col='green', type='sex', margin=TRUE, loc.diff=c(0,0), xlab='Right Ascension', ylab='Declination', mgp=c(2,0.5,0), mtline=2, position='topright', com.col="green", com.length=0.05, CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, ...){
  
  if(missing(xlab)){
    if(type=='sex'){
      xlab=paste(xlab,'/ H:M:S')
    }
    if(type=='deg'){
      xlab=paste(xlab,'/ deg')
    }
  }
  if(missing(ylab)){
    if(type=='sex'){
      ylab=paste(ylab,'/ D:M:S')
    }
    if(type=='deg'){
      ylab=paste(ylab,'/ deg')
    }
  }
  
  if(!missing(image)){
    if(any(names(image)=='imDat') & missing(header)){
      header=image$hdr
      header=data.frame(key=header[c(T,F)],value=header[c(F,T)], stringsAsFactors = FALSE)
      image=image$imDat
    }
    if(any(names(image)=='dat') & missing(header)){
      header=image$hdr[[1]]
      header=data.frame(key=header[,1],value=header[,2], stringsAsFactors = FALSE)
      image=image$dat[[1]]
    }
    if(any(names(image)=='image') & missing(header)){
      header=image$header
      image=image$image
    }
    magimage(image, axes=FALSE, ...)
    box()
  }
  magimageWCSGrid(header=header, n=n, grid.col=grid.col, grid.lty=grid.lty, grid.lwd=grid.lwd, type=type, loc.diff=loc.diff, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  magimageWCSLabels(header=header, n=n, lab.col=lab.col, type=type, margin=margin, loc.diff=loc.diff, xlab=xlab, ylab=ylab, mgp=mgp, mtline=mtline, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  magimageWCSCompass(header=header, position=position, com.col=com.col, com.length=com.length, loc.diff=loc.diff, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
}

magimageWCSGrid=function(header, n, grid.col='grey', grid.lty=1, grid.lwd=1, type='sex', loc.diff=c(0,0), CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, ...){
  
  xlo=min(par()$usr[1:2])+0.5+loc.diff[1]
  xhi=max(par()$usr[1:2])+0.5+loc.diff[1]
  ylo=min(par()$usr[3:4])+0.5+loc.diff[2]
  yhi=max(par()$usr[3:4])+0.5+loc.diff[2]
  
  coordlims=rbind(
    xy2radec(xlo, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    xy2radec(xlo, yhi, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    xy2radec(xhi, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    xy2radec(xhi, yhi, header = header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  )
  rarange=range(coordlims[,1])
  decrange=range(coordlims[,2])
  if(type=='sex'){
    ragrid=maglab(rarange, n=n, prettybase = 15/3600)
    decgrid=maglab(decrange, n=n, prettybase = 1/3600)
  }
  if(type=='deg'){
    ragrid=maglab(rarange, n=n)
    decgrid=maglab(decrange, n=n)
  }
  for(ra in ragrid$tickat){
    tempxy=radec2xy(cbind(ra, seq(min(decgrid$tickat), max(decgrid$tickat), len=100)), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)-0.5
    tempxy[,1]=tempxy[,1]-loc.diff[1]
    tempxy[,2]=tempxy[,2]-loc.diff[2]
    lines(tempxy, col=grid.col, lty=grid.lty, lwd=grid.lwd, ...)
  }
  for(dec in decgrid$tickat){
    tempxy=radec2xy(cbind(seq(min(ragrid$tickat), max(ragrid$tickat),len=100), dec), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)-0.5
    tempxy[,1]=tempxy[,1]-loc.diff[1]
    tempxy[,2]=tempxy[,2]-loc.diff[2]
    lines(tempxy, col=grid.col, lty=grid.lty, lwd=grid.lwd, ...)
  }
  
}

magimageWCSLabels=function(header, n, lab.col='green', type='sex', margin=TRUE, loc.diff=c(0,0), xlab='Right Ascension', ylab='Declination', mgp=c(2,0.5,0), mtline=2, CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, ...){
  
  if(missing(xlab)){
    if(type=='sex'){
      xlab=paste(xlab,'/ H:M:S')
    }
    if(type=='deg'){
      xlab=paste(xlab,'/ deg')
    }
  }
  if(missing(ylab)){
    if(type=='sex'){
      ylab=paste(ylab,'/ D:M:S')
    }
    if(type=='deg'){
      ylab=paste(ylab,'/ deg')
    }
  }
  
  xlo=min(par()$usr[1:2])+0.5+loc.diff[1]
  xhi=max(par()$usr[1:2])+0.5+loc.diff[1]
  ylo=min(par()$usr[3:4])+0.5+loc.diff[2]
  yhi=max(par()$usr[3:4])+0.5+loc.diff[2]
  
  coordlims=rbind(
    xy2radec(xlo, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    xy2radec(xlo, yhi, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    xy2radec(xhi, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    xy2radec(xhi, yhi, header = header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  )
  rarange=range(coordlims[,1])
  decrange=range(coordlims[,2])
  if(type=='sex'){
    ragrid=maglab(rarange, n=n, prettybase = 15/3600)
    decgrid=maglab(decrange, n=n, prettybase = 1/3600)
  }
  if(type=='deg'){
    ragrid=maglab(rarange, n=n)
    decgrid=maglab(decrange, n=n)
  }
  rapretty=ragrid$tickat
  rapretty=rapretty[rapretty>min(rarange) & rapretty<max(rarange)]
  decpretty=decgrid$tickat
  decpretty=decpretty[decpretty>min(decrange) & decpretty<max(decrange)]
  if(margin==FALSE){
    if(type=='sex'){
      tempxy=radec2xy(cbind(rapretty, decpretty[1]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)-0.5
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(1, at=tempxy[,1], labels = deg2hms(rapretty, type='cat', digits=1), mgp=-mgp-3, tick=FALSE, col.axis=lab.col, ...)
      #text(tempxy, labels=deg2hms(rapretty, type='cat', digits=1), col=lab.col, ...)
      
      tempxy=radec2xy(cbind(rapretty[length(rapretty)], decpretty[-1]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)-0.5
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(2, at=tempxy[,2], labels = deg2dms(decpretty[-1], type='cat', digits=0), mgp=-mgp-3, tick=FALSE, col.axis=lab.col, ...)
      #text(tempxy, labels=deg2dms(decpretty[-2:-1], type='cat', digits=0), srt=90, col=lab.col, ...)
    }
    if(type=='deg'){
      tempxy=radec2xy(cbind(rapretty, decpretty[1]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)-0.5
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(1, at=tempxy[,1], labels = rapretty, mgp=-mgp-3, tick=FALSE, col.axis=lab.col, ...)
      #text(tempxy, labels=rapretty, col=lab.col, ...)
      
      tempxy=radec2xy(cbind(rapretty[length(rapretty)], decpretty[-1]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)-0.5
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(2, at=tempxy[,2], labels = decpretty[-1], mgp=-mgp-3, tick=FALSE, col.axis=lab.col, ...)
      #text(tempxy, labels=decpretty[-2:-1], srt=90, col=lab.col, ...)
    }
    mtext(xlab, 1, line = -mtline, col=lab.col)
    mtext(ylab, 2, line = -mtline, col=lab.col)
  }else{
    if(type=='sex'){
      axis(1, radec2xy(cbind(rapretty, decpretty[1]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)[,1]-0.5-loc.diff[1], labels=deg2hms(rapretty, type='cat', digits=1), mgp=mgp, tick=FALSE, ...)
      axis(2, radec2xy(cbind(rapretty[1], decpretty), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)[,2]-0.5-loc.diff[2], labels=deg2dms(decpretty, type='cat', digits=0), mgp=mgp, tick=FALSE, ...)
    }
    if(type=='deg'){
      axis(1, radec2xy(cbind(rapretty, decpretty[1]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)[,1]-0.5-loc.diff[1], labels=rapretty, mgp=mgp, tick=FALSE, ...)
      axis(2, radec2xy(cbind(rapretty[1], decpretty), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)[,2]-0.5-loc.diff[2], labels=decpretty, mgp=mgp, tick=FALSE, ...)
    }
    mtext(xlab, 1, line = mtline)
    mtext(ylab, 2, line = mtline)
  }
}

magimageWCSCompass=function(header, position='topright', com.col='green', com.length=0.05, loc.diff=c(0,0), CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, ...){
  xlo=min(par()$usr[1:2])+0.5+loc.diff[1]
  xhi=max(par()$usr[1:2])+0.5+loc.diff[1]
  ylo=min(par()$usr[3:4])+0.5+loc.diff[2]
  yhi=max(par()$usr[3:4])+0.5+loc.diff[2]
  coordlims=rbind(
    xy2radec(xlo, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    xy2radec(xlo, yhi, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    xy2radec(xhi, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    xy2radec(xhi, yhi, header = header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  )
  rarange=range(coordlims[,1])
  decrange=range(coordlims[,2])
  
  startra=rarange[2]-(rarange[2]-rarange[1])*0.5
  startdec=decrange[2]-(decrange[2]-decrange[1])*0.5
  
  if (length(grep("bottom", position)) > 0) {
    startdec=decrange[2]-(decrange[2]-decrange[1])*0.85
  }
  if (length(grep("left", position)) > 0) {
    startra=rarange[2]-(rarange[2]-rarange[1])*0.15
  }
  if (length(grep("top", position)) > 0) {
    startdec=decrange[2]-(decrange[2]-decrange[1])*0.15
  }
  if (length(grep("right", position)) > 0) {
    startra=rarange[2]-(rarange[2]-rarange[1])*0.85
  }
  
  endra=startra+(rarange[2]-rarange[1])*0.05
  enddec=startdec+(decrange[2]-decrange[1])*0.05
  
  startxy=radec2xy(startra, startdec, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)-0.5-loc.diff
  endxyN=radec2xy(startra, enddec, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)-0.5-loc.diff
  endxyE=radec2xy(endra, startdec, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)-0.5-loc.diff
  
  arrows(startxy[1,1], startxy[1,2], endxyN[1,1], endxyN[1,2], length=com.length, col=com.col, ...)
  arrows(startxy[1,1], startxy[1,2], endxyE[1,1], endxyE[1,2], length=com.length, col=com.col, ...)
  
  text(endxyN[1,1], endxyN[1,2], labels='N', col=com.col, pos=3)
  text(endxyE[1,1], endxyE[1,2], labels='E', col=com.col, pos=2)
}

magcutoutWCS=function(image, header, loc, box = c(30, 30), plot = FALSE, CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, unit='deg', sep=':', loc.type='coord', ...){
  box=box/3600
  if(!missing(image)){
    if(any(names(image)=='imDat') & missing(header)){
      header=image$hdr
      header=data.frame(key=header[c(T,F)],value=header[c(F,T)], stringsAsFactors = FALSE)
      image=image$imDat
    }
    if(any(names(image)=='dat') & missing(header)){
      header=image$hdr[[1]]
      header=data.frame(key=header[,1],value=header[,2], stringsAsFactors = FALSE)
      image=image$dat[[1]]
    }
    if(any(names(image)=='image') & missing(header)){
      header=image$header
      image=image$image
    }
  }
  #Note below tempxy is FITS xy units, not R:
  if(missing(loc)){
    loc=xy2radec(dim(image)[1]/2+0.5, dim(image)[2]/2+0.5, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)[1,]
    tempxy=cbind(dim(image)[1]/2+0.5, dim(image)[2]/2+0.5)
  }else{
    if(loc.type=='coord'){
      if(unit=='sex'){loc[1]=hms2deg(loc[1],sep=sep); loc[2]=dms2deg(loc[2],sep=sep)}
      loc=as.numeric(loc)
      tempxy=radec2xy(loc[1], loc[2], header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
    }else if(loc.type=='image'){
      tempxy=rbind(loc+0.5)
      loc=xy2radec(loc[1]+0.5, loc[2]+0.5, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)[1,]
    }
  }
  xcen = tempxy[1,1]-0.5
  ycen = tempxy[1,2]-0.5
  tempxy=radec2xy(loc[1]-box[1]/2/cos(loc[2]*pi/180), loc[2], header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  xlo = tempxy[1,1]
  tempxy=radec2xy(loc[1]+box[1]/2/cos(loc[2]*pi/180), loc[2], header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  xhi = tempxy[1,1]
  tempxy=radec2xy(loc[1], loc[2]-box[2]/2, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  ylo = tempxy[1,2]
  tempxy=radec2xy(loc[1], loc[2]+box[2]/2, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  yhi = tempxy[1,2]
  xtemp=sort(c(xlo,xhi))
  xlo=floor(xtemp[1])
  xhi=floor(xtemp[2])
  ytemp=sort(c(ylo,yhi))
  ylo=floor(ytemp[1])
  yhi=floor(ytemp[2])
  if (xlo < 1) {
    xlo = 1
  }
  if (xhi > dim(image)[1]) {
    xhi = dim(image)[1]
  }
  if (ylo < 1) {
    ylo = 1
  }
  if (yhi > dim(image)[2]) {
    yhi = dim(image)[2]
  }
  image = image[xlo:xhi, ylo:yhi]
  xcen.new=xcen-xlo+1
  ycen.new=ycen-ylo+1
  xscale=abs(diff(xy2radec(c(xcen,xcen+1), c(ycen, ycen), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)))
  xscale=sqrt(sum(xscale^2))*cos(loc[2]*pi/180)
  yscale=abs(diff(xy2radec(c(xcen, xcen), c(ycen, ycen+1), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)))
  yscale=sqrt(sum(yscale^2))
  loc.diff = c(xlo - 1, ylo - 1)
  xlo=min(par()$usr[1:2])+0.5+loc.diff[1]
  xhi=max(par()$usr[1:2])+0.5+loc.diff[1]
  ylo=min(par()$usr[3:4])+0.5+loc.diff[2]
  yhi=max(par()$usr[3:4])+0.5+loc.diff[2]
  usr.WCS=rbind(
    xy2radec(xlo, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    xy2radec(xlo, yhi, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    xy2radec(xhi, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    xy2radec(xhi, yhi, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  )
  usr.WCS=cbind(x.cut=c(min(par()$usr[1:2]),min(par()$usr[1:2]),max(par()$usr[1:2]),max(par()$usr[1:2])),
                y.cut=c(min(par()$usr[3:4]),max(par()$usr[3:4]),min(par()$usr[3:4]),max(par()$usr[3:4])),
                x.orig=c(min(par()$usr[1:2]),min(par()$usr[1:2]),max(par()$usr[1:2]),max(par()$usr[1:2]))+loc.diff[1],
                y.orig=c(min(par()$usr[3:4]),max(par()$usr[3:4]),min(par()$usr[3:4]),max(par()$usr[3:4]))+loc.diff[2],
                usr.WCS
                )
  approx.map.RA=approxfun(seq(usr.WCS[1,'RA'],usr.WCS[4,'RA'],len=1e2),seq(usr.WCS[1,'x.cut'],usr.WCS[4,'x.cut'],len=1e2))
  approx.map.Dec=approxfun(seq(usr.WCS[1,'Dec'],usr.WCS[4,'Dec'],len=1e2),seq(usr.WCS[1,'y.cut'],usr.WCS[4,'y.cut'],len=1e2))
  approx.map=function(RA, Dec){
    if(length(dim(RA)) == 2){
      Dec = RA[, 2]
      RA = RA[, 1]
    }
    return=cbind(x=approx.map.RA(RA), y=approx.map.Dec(Dec))
  }
  if (plot) {
    magimageWCS(image=image, header=header, loc.diff=loc.diff, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, ...)
  }
  if(!missing(header)){
    if(length(dim(header))==2){
      CRPIX1=as.numeric(header[header[,1]=='CRPIX1',2])-loc.diff[1]
      CRPIX2=as.numeric(header[header[,1]=='CRPIX2',2])-loc.diff[2]
      header[header[,1]=='CRPIX1',2]=as.character(CRPIX1)
      header[header[,1]=='CRPIX2',2]=as.character(CRPIX2)
    }else{
      header=NULL
    }
  }else{
    header=NULL
  }
  output = list(image = image, loc = c(xcen.new, ycen.new), loc.orig = c(xcen, ycen), loc.diff = loc.diff, xsel = xlo:xhi, ysel = ylo:yhi, loc.WCS = loc, scale.WCS=c(xscale, yscale), usr.WCS=usr.WCS, approx.map=approx.map, header=header)
  return = output
}

magWCSradec2xy=function(RA, Dec, header, CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, loc.diff=c(0,0), unit='deg', sep=':'){
  if(length(dim(RA)) == 2){
    Dec = RA[, 2]
    RA = RA[, 1]
  }
  if(unit=='sex'){RA=hms2deg(RA,sep=sep); Dec=dms2deg(Dec,sep=sep)}
  RA = as.numeric(RA)
  Dec = as.numeric(Dec)
  
  tempxy=radec2xy(RA, Dec, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  tempxy[,1]=tempxy[,1]-0.5-loc.diff[1]
  tempxy[,2]=tempxy[,2]-0.5-loc.diff[2]
  return = tempxy
}

magWCSxy2radec=function(x, y, header, CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, loc.diff=c(0,0)){
  if(length(dim(x)) == 2){
    y = x[, 2]
    x = x[, 1]
  }
  x = as.numeric(x)
  y = as.numeric(y)
  
  tempradec=xy2radec(x+0.5+loc.diff[1], y+0.5+loc.diff[2], header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  tempradec[,1]=tempradec[,1]
  tempradec[,2]=tempradec[,2]
  return = tempradec
}
