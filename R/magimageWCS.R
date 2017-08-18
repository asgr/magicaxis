magimageWCS=function(image, header, n, grid.col='grey', grid.lty=2, grid.lwd=0.5, lab.col='green', coord.type='sex', margin=TRUE, loc.diff=c(0,0), xlab='Right Ascension', ylab='Declination', mgp=c(2,0.5,0), mtline=2, position='topright', com.col="green", com.length=0.05, coord.axis='auto', pretty='auto', CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, ...){
  
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
  
  if(!missing(image)){
    if(any(names(image)=='imDat') & missing(header)){
      header=image$hdr
      image=image$imDat
    }else if(any(names(image)=='imDat') & !missing(header)){
      image=image$imDat
    }
    if(any(names(image)=='dat') & missing(header)){
      header=image$hdr[[1]]
      header=data.frame(key=header[,1],value=header[,2], stringsAsFactors = FALSE)
      image=image$dat[[1]]
    }else if(any(names(image)=='dat') & !missing(header)){
      image=image$dat[[1]]
    }
    if(any(names(image)=='image') & missing(header)){
      header=image$header
      image=image$image
    }else if(any(names(image)=='image') & !missing(header)){
      image=image$image
    }
    output=magimage(image, axes=FALSE, ...)
    box()
  }
  magimageWCSGrid(header=header, n=n, grid.col=grid.col, grid.lty=grid.lty, grid.lwd=grid.lwd, coord.type=coord.type, loc.diff=loc.diff, pretty=pretty, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  magimageWCSLabels(header=header, n=n, lab.col=lab.col, coord.type=coord.type, margin=margin, loc.diff=loc.diff, xlab=xlab, ylab=ylab, mgp=mgp, mtline=mtline, pretty=pretty, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  magimageWCSCompass(header=header, position=position, com.col=com.col, com.length=com.length, loc.diff=loc.diff, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  return=output
}

magimageWCSGrid=function(header, n, grid.col='grey', grid.lty=1, grid.lwd=1, coord.type='sex', loc.diff=c(0,0), pretty='auto', CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, ...){
  
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
    if(diff(rarange)>0.5){pretty=1}
    if(diff(rarange)<0.5 & diff(rarange)>0.5/60){pretty=6}
    if(diff(rarange)<0.5/6){pretty=3600}
  }
  
  if(coord.type=='sex'){
    ragrid=maglab(rarange, n=n, prettybase = 15/pretty)
    decgrid=maglab(decrange, n=n, prettybase = 1/pretty)
  }
  if(coord.type=='deg'){
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

magimageWCSLabels=function(header, n, lab.col='green', coord.type='sex', margin=TRUE, loc.diff=c(0,0), xlab='Right Ascension', ylab='Declination', mgp=c(2,0.5,0), mtline=2, coord.axis='auto', pretty='auto', CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, ...){
  
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
    if(diff(rarange)>0.5){pretty=1}
    if(diff(rarange)<0.5 & diff(rarange)>0.5/6){pretty=60}
    if(diff(rarange)<0.5/6){pretty=3600}
  }
  
  if(coord.type=='sex'){
    ragrid=maglab(rarange, n=n, prettybase = 15/pretty)
    decgrid=maglab(decrange, n=n, prettybase = 1/pretty)
  }
  if(coord.type=='deg'){
    ragrid=maglab(rarange, n=n)
    decgrid=maglab(decrange, n=n)
  }
  
  rapretty=ragrid$tickat
  rapretty=rapretty[rapretty>min(rarange) & rapretty<max(rarange)]
  decpretty=decgrid$tickat
  decpretty=decpretty[decpretty>min(decrange) & decpretty<max(decrange)]
  if(margin==FALSE){
    if(coord.type=='sex'){
      tempxy=magWCSradec2xy(cbind(rapretty, coordlims[1,2]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(raaxis, at=tempxy[,raaxis], labels = deg2hms(rapretty, type='cat', digits=1), mgp=-mgp-3, tick=FALSE, col.axis=lab.col, ...)

      tempxy=magWCSradec2xy(cbind(coordlims[1,1], decpretty[-1]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(decaxis, at=tempxy[,decaxis], labels = deg2dms(decpretty[-1], type='cat', digits=0), mgp=-mgp-3, tick=FALSE, col.axis=lab.col, ...)
    }
    if(coord.type=='deg'){
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
    if(coord.type=='sex'){
      tempxy=magWCSradec2xy(cbind(rapretty, coordlims[1,2]), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(raaxis, tempxy[,raaxis], labels=deg2hms(rapretty, type='cat', digits=1), mgp=mgp, tick=FALSE, ...)
      
      tempxy=magWCSradec2xy(cbind(coordlims[1,1], decpretty), header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
      tempxy[,1]=tempxy[,1]-loc.diff[1]
      tempxy[,2]=tempxy[,2]-loc.diff[2]
      axis(decaxis, tempxy[,decaxis], labels=deg2dms(decpretty, type='cat', digits=0), mgp=mgp, tick=FALSE, ...)
    }
    if(coord.type=='deg'){
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
  
  endra=startra+abs(rarange[2]-rarange[1])*0.065
  enddec=startdec+abs(decrange[2]-decrange[1])*0.065
  
  endxyN=magWCSradec2xy(startra, enddec, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)
  endxyE=magWCSradec2xy(endra, startdec, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, loc.diff=loc.diff)
  
  text(endxyN[1,1], endxyN[1,2], labels='N', col=com.col, adj=c(0.5,0.5))
  text(endxyE[1,1], endxyE[1,2], labels='E', col=com.col, adj=c(0.5,0.5))
}

magWCSradec2xy=function(RA, Dec, header, CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0, CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, loc.diff=c(0,0), coord.type='deg', sep=':'){
  if(length(dim(RA)) == 2){
    Dec = RA[, 2]
    RA = RA[, 1]
  }
  if(coord.type=='sex'){RA=hms2deg(RA,sep=sep); Dec=dms2deg(Dec,sep=sep)}
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
