magimageWCS=function(image, header, n, grid.col='grey', grid.lty=2, grid.lwd=0.5, lab.col='green', type='sex', margin=TRUE, loc.diff=c(0,0), xlab='Right Ascension', ylab='Declination', mgp=c(2,0.5,0), mtline=2, position='topright', com.col="green", com.length=0.05, coord.axis='auto', pretty='auto', CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, ...){
  
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
      #header=data.frame(key=header[c(T,F)],value=header[c(F,T)], stringsAsFactors = FALSE)
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
  magimageWCSGrid(header=header, n=n, grid.col=grid.col, grid.lty=grid.lty, grid.lwd=grid.lwd, type=type, loc.diff=loc.diff, pretty=pretty, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  magimageWCSLabels(header=header, n=n, lab.col=lab.col, type=type, margin=margin, loc.diff=loc.diff, xlab=xlab, ylab=ylab, mgp=mgp, mtline=mtline, pretty=pretty, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  magimageWCSCompass(header=header, position=position, com.col=com.col, com.length=com.length, loc.diff=loc.diff, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
}

magimageWCSGrid=function(header, n, grid.col='grey', grid.lty=1, grid.lwd=1, type='sex', loc.diff=c(0,0), pretty='auto', CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, ...){
  
  xlo=min(par()$usr[1:2])
  xhi=max(par()$usr[1:2])
  ylo=min(par()$usr[3:4])
  yhi=max(par()$usr[3:4])
  
  coordlims=rbind(
    magWCSxy2radec(xlo, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff),
    magWCSxy2radec(xlo, yhi, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff),
    magWCSxy2radec(xhi, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff),
    magWCSxy2radec(xhi, yhi, header = header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)
  )
  
  rarange=range(coordlims[,1])
  decrange=range(coordlims[,2])
  
  if(pretty=='auto'){
    if(diff(rarange)>0.1){pretty=1}
    if(diff(rarange)<0.1 & diff(rarange)>0.1/60){pretty=60}
    if(diff(rarange)<0.1/60){pretty=3600}
  }
  
  if(type=='sex'){
    ragrid=maglab(rarange, n=n, prettybase = 15/pretty)
    decgrid=maglab(decrange, n=n, prettybase = 1/pretty)
  }
  if(type=='deg'){
    ragrid=maglab(rarange, n=n)
    decgrid=maglab(decrange, n=n)
  }
  
  for(ra in ragrid$tickat){
    tempxy=magWCSradec2xy(cbind(ra, seq(min(decgrid$tickat), max(decgrid$tickat), len=100)), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
    tempxy[,1]=tempxy[,1]-loc.diff[1]
    tempxy[,2]=tempxy[,2]-loc.diff[2]
    lines(tempxy, col=grid.col, lty=grid.lty, lwd=grid.lwd, ...)
  }
  for(dec in decgrid$tickat){
    tempxy=magWCSradec2xy(cbind(seq(min(ragrid$tickat), max(ragrid$tickat),len=100), dec), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
    tempxy[,1]=tempxy[,1]-loc.diff[1]
    tempxy[,2]=tempxy[,2]-loc.diff[2]
    lines(tempxy, col=grid.col, lty=grid.lty, lwd=grid.lwd, ...)
  }
  
}

magimageWCSLabels=function(header, n, lab.col='green', type='sex', margin=TRUE, loc.diff=c(0,0), xlab='Right Ascension', ylab='Declination', mgp=c(2,0.5,0), mtline=2, coord.axis='auto', pretty='auto', CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, ...){
  
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
  
  xlo=min(par()$usr[1:2])
  xhi=max(par()$usr[1:2])
  ylo=min(par()$usr[3:4])
  yhi=max(par()$usr[3:4])
  
  coordlims=rbind(
    magWCSxy2radec(xlo, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff),
    magWCSxy2radec(xlo, yhi, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff),
    magWCSxy2radec(xhi, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff),
    magWCSxy2radec(xhi, yhi, header = header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)
  )
  
  if(coord.axis[1]=='auto'){
    if(abs(diff(coordlims[1:2,1]))<abs(diff(coordlims[c(1,3),1]))){
      raaxis=1
      decaxis=2
    }else{
      raaxis=2
      decaxis=1
    }
  }else{
    raaxis=coord.axis[1]
    decaxis=coord.axis[2]
  }
  
  rarange=range(coordlims[,1])
  decrange=range(coordlims[,2])
  
  if(pretty=='auto'){
    if(diff(rarange)>0.1){pretty=1}
    if(diff(rarange)<0.1 & diff(rarange)>0.1/60){pretty=60}
    if(diff(rarange)<0.1/60){pretty=3600}
  }
  
  if(type=='sex'){
    ragrid=maglab(rarange, n=n, prettybase = 15/pretty)
    decgrid=maglab(decrange, n=n, prettybase = 1/pretty)
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
      tempxy=magWCSradec2xy(cbind(rapretty, coordlims[1,2]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(raaxis, at=tempxy[,raaxis], labels = deg2hms(rapretty, type='cat', digits=1), mgp=-mgp-3, tick=FALSE, col.axis=lab.col, ...)

      tempxy=magWCSradec2xy(cbind(coordlims[1,1], decpretty[-1]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(decaxis, at=tempxy[,decaxis], labels = deg2dms(decpretty[-1], type='cat', digits=0), mgp=-mgp-3, tick=FALSE, col.axis=lab.col, ...)
    }
    if(type=='deg'){
      tempxy=magWCSradec2xy(cbind(rapretty, coordlims[1,2]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(raaxis, at=tempxy[,raaxis], labels = rapretty, mgp=-mgp-3, tick=FALSE, col.axis=lab.col, ...)

      tempxy=magWCSradec2xy(cbind(coordlims[1,1], decpretty[-1]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(decaxis, at=tempxy[,decaxis], labels = decpretty[-1], mgp=-mgp-3, tick=FALSE, col.axis=lab.col, ...)
    }
    mtext(xlab, raaxis, line = -mtline, col=lab.col)
    mtext(ylab, decaxis, line = -mtline, col=lab.col)
  }else{
    if(type=='sex'){
      tempxy=magWCSradec2xy(cbind(rapretty, coordlims[1,2]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(raaxis, tempxy[,raaxis], labels=deg2hms(rapretty, type='cat', digits=1), mgp=mgp, tick=FALSE, ...)
      
      tempxy=magWCSradec2xy(cbind(coordlims[1,1], decpretty), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(decaxis, tempxy[,decaxis], labels=deg2dms(decpretty, type='cat', digits=0), mgp=mgp, tick=FALSE, ...)
    }
    if(type=='deg'){
      tempxy=magWCSradec2xy(cbind(rapretty, coordlims[1,2]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(raaxis, tempxy[,raaxis], labels=rapretty, mgp=mgp, tick=FALSE, ...)
      
      tempxy=magWCSradec2xy(cbind(coordlims[1,1], decpretty), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(decaxis, tempxy[,decaxis], labels=decpretty, mgp=mgp, tick=FALSE, ...)
    }
    mtext(xlab, raaxis, line = mtline)
    mtext(ylab, decaxis, line = mtline)
  }
}

magimageWCSCompass=function(header, position='topright', com.col='green', com.length=0.05, loc.diff=c(0,0), CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, ...){
  xlo=min(par()$usr[1:2])
  xhi=max(par()$usr[1:2])
  ylo=min(par()$usr[3:4])
  yhi=max(par()$usr[3:4])
  
  xdiff=diff(c(xlo, xhi))
  ydiff=diff(c(ylo, yhi))
  
  if(position=='centre'){
    coord=magWCSxy2radec(xlo+xdiff*0.5, ylo+ydiff*0.5, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)[1,]
  }
  if(position=='bottom'){
    coord=magWCSxy2radec(xlo+xdiff*0.5, ylo+ydiff*0.15, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)[1,]
  }
  if(position=='bottomleft'){
    coord=magWCSxy2radec(xlo+xdiff*0.15, ylo+ydiff*0.15, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)[1,]
  }
  if(position=='left'){
    coord=magWCSxy2radec(xlo+xdiff*0.15, ylo+ydiff*0.5, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)[1,]
  }
  if(position=='topleft'){
    coord=magWCSxy2radec(xlo+xdiff*0.15, ylo+ydiff*0.85, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)[1,]
  }
  if(position=='top'){
    coord=magWCSxy2radec(xlo+xdiff*0.5, ylo+ydiff*0.85, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)[1,]
  }
  if(position=='topright'){
    coord=magWCSxy2radec(xlo+xdiff*0.85, ylo+ydiff*0.85, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)[1,]
  }
  if(position=='right'){
    coord=magWCSxy2radec(xlo+xdiff*0.85, ylo+ydiff*0.5, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)[1,]
  }
  if(position=='bottomright'){
    coord=magWCSxy2radec(xlo+xdiff*0.85, ylo+ydiff*0.15, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)[1,]
  }
  
  startra=coord[1]
  startdec=coord[2]
  
  coordlims=rbind(
    magWCSxy2radec(xlo, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff),
    magWCSxy2radec(xlo, yhi, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff),
    magWCSxy2radec(xhi, ylo, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff),
    magWCSxy2radec(xhi, yhi, header = header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)
  )
  rarange=range(coordlims[,1])
  decrange=range(coordlims[,2])
  
  endra=startra+abs(rarange[2]-rarange[1])*0.05
  enddec=startdec+abs(decrange[2]-decrange[1])*0.05
  
  startxy=magWCSradec2xy(startra, startdec, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)
  endxyN=magWCSradec2xy(startra, enddec, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)
  endxyE=magWCSradec2xy(endra, startdec, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)
  
  arrows(startxy[1,1], startxy[1,2], endxyN[1,1], endxyN[1,2], length=com.length, col=com.col, ...)
  arrows(startxy[1,1], startxy[1,2], endxyE[1,1], endxyE[1,2], length=com.length, col=com.col, ...)
  
  endra=startra+abs(rarange[2]-rarange[1])*0.06
  enddec=startdec+abs(decrange[2]-decrange[1])*0.06
  
  endxyN=magWCSradec2xy(startra, enddec, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)
  endxyE=magWCSradec2xy(endra, startdec, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)
  
  text(endxyN[1,1], endxyN[1,2], labels='N', col=com.col, adj=c(0.5,0))
  text(endxyE[1,1], endxyE[1,2], labels='E', col=com.col, adj=c(0,0.5))
}

magcutoutWCS=function(image, header, loc, box = c(30, 30), plot = FALSE, CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, unit='deg', sep=':', loc.type='coord', ...){
  box=box/3600
  if(!missing(image)){
    if(any(names(image)=='imDat') & missing(header)){
      imtype='FITSio'
      header=image$hdr
      image=image$imDat
    }
    if(any(names(image)=='dat') & missing(header)){
      imtype='astro'
      header=image$hdr[[1]]
      header=data.frame(key=header[,1],value=header[,2], stringsAsFactors = FALSE)
      image=image$dat[[1]]
    }
    if(any(names(image)=='image') & missing(header)){
      header=image$header
      image=image$image
      if(is.matrix(header)==TRUE){imtype='astro'}else{imtype='FITSio'}
    }
    if(!missing(header)){
      if(is.matrix(header)==TRUE){imtype='astro'}else{imtype='FITSio'}
    }
  }
  #Note below tempxy is R xy units, not FITS:
  if(missing(loc)){
    loc=magWCSxy2radec(dim(image)[1]/2, dim(image)[2]/2, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)[1,]
    tempxy=cbind(dim(image)[1]/2, dim(image)[2]/2)
  }else{
    if(loc.type=='coord'){
      if(unit=='sex'){loc[1]=hms2deg(loc[1],sep=sep); loc[2]=dms2deg(loc[2],sep=sep)}
      loc=as.numeric(loc)
      tempxy=magWCSradec2xy(loc[1], loc[2], header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
    }else if(loc.type=='image'){
      tempxy=rbind(loc)
      loc=magWCSxy2radec(loc[1], loc[2], header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)[1,]
    }
  }
  xcen = tempxy[1,1]
  ycen = tempxy[1,2]
  tempxy=magWCSradec2xy(loc[1]-box[1]/2/cos(loc[2]*pi/180), loc[2], header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  xlo = tempxy[1,1]
  tempxy=magWCSradec2xy(loc[1]+box[1]/2/cos(loc[2]*pi/180), loc[2], header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  xhi = tempxy[1,1]
  tempxy=radec2xy(loc[1], loc[2]-box[2]/2, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  ylo = tempxy[1,2]
  tempxy=magWCSradec2xy(loc[1], loc[2]+box[2]/2, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  yhi = tempxy[1,2]
  xtemp=sort(c(xlo,xhi))
  xlo=ceiling(xtemp[1])
  xhi=ceiling(xtemp[2])
  ytemp=sort(c(ylo,yhi))
  ylo=ceiling(ytemp[1])
  yhi=ceiling(ytemp[2])
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
  cut_image = image[xlo:xhi, ylo:yhi]
  xcen.new=xcen-xlo+1
  ycen.new=ycen-ylo+1
  xscale=abs(diff(magWCSxy2radec(c(xcen,xcen+1), c(ycen, ycen), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)))
  xscale=sqrt(sum(xscale^2))*cos(loc[2]*pi/180)
  yscale=abs(diff(magWCSxy2radec(c(xcen, xcen), c(ycen, ycen+1), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)))
  yscale=sqrt(sum(yscale^2))
  loc.diff = c(xlo - 1, ylo - 1)
  cut_xlo=1
  cut_xhi=dim(cut_image)[1]
  cut_ylo=1
  cut_yhi=dim(cut_image)[2]
  usr.WCS=rbind(
    magWCSxy2radec(xlo-1, ylo-1, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    magWCSxy2radec(xlo-1, yhi, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    magWCSxy2radec(xhi, ylo-1, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    magWCSxy2radec(xhi, yhi, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  )
  #Below we want to define the R image usr coordinate system, so if e.g. a matrix is 10x10 this would have elements 1:10 x 1:10 but a usr image range of 0->10 x 0->10, hence the minus 1s below. Even a single pixel has a finite image extent (0->1 x 0->1).
  usr.WCS=cbind(x.cut=c(cut_xlo-1, cut_xlo-1, cut_xhi, cut_xhi),
                y.cut=c(cut_ylo-1, cut_yhi, cut_ylo-1, cut_yhi),
                x.orig=c(xlo-1, xlo-1, xhi, xhi),
                y.orig=c(ylo-1, yhi, ylo-1, yhi),
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
    magimageWCS(image=cut_image, header=header, loc.diff=loc.diff, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, ...)
  }
  if(!missing(header)){
    if(imtype=='FITSio'){
      CRPIX1=as.numeric(header[which(header=='CRPIX1')+1])-loc.diff[1]
      CRPIX2=as.numeric(header[which(header=='CRPIX2')+1])-loc.diff[2]
      header[which(header=='CRPIX1')+1]=as.character(CRPIX1)
      header[which(header=='CRPIX2')+1]=as.character(CRPIX2)
    }else if(imtype=='astro'){
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
  output = list(image = cut_image, loc = c(xcen.new, ycen.new), loc.orig = c(xcen, ycen), loc.diff = loc.diff, xsel = xlo:xhi, ysel = ylo:yhi, loc.WCS = loc, scale.WCS=c(xscale, yscale), usr.WCS=usr.WCS, approx.map=approx.map, header=header)
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
