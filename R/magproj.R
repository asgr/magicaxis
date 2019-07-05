magproj=function(long, lat, type='b', plottext, longlim=c(-180,180), latlim=c(-90,90), projection="aitoff", parameters=NULL, centre=c(0,0), add=FALSE, fliplong=FALSE, nlat=6, nlong=6, prettybase=30, labels=TRUE, grid=TRUE, grid.col='grey', grid.lty=2, auto=FALSE, upres=100, box=TRUE, labloc=c(90,-45), labeltype='deg', crunch=FALSE, ...){
  
  if(is.matrix(long) | is.data.frame(long)){
    lat = long[, 2]
    long = long[, 1]
  }
  
  if(length(long)>1 & length(lat)==1){lat=rep(lat,length(long))}
  if(length(lat)>1 & length(long)==1){long=rep(long,length(lat))}
  
  if(add==TRUE){
    projection=.Last.projection()$projection
    parameters=.Last.projection()$parameters
    orientation=.Last.projection()$orientation
    centre=.Last.projection()$centre
    labloc=.Last.projection()$labloc
    longlim=.Last.projection()$longlim
    latlim=.Last.projection()$latlim
  }else{
    orientation=c(90+centre[2], centre[1] %% 360, 0)
    if(longlim[1]<orientation[2]-180){
      longlim[2]=longlim[2]+orientation[2]-180-longlim[1]
      longlim[1]=orientation[2]-180
    }
  }
  
  long= (long+(180-orientation[2])) %% 360 - (180-orientation[2])
  lat= (lat+90) %% 180 - 90
  
  if(add==FALSE){
    if(auto==TRUE){
      longlim=range(long)
      longlim=longlim+diff(longlim)*c(-0.1,0.1)
      latlim=range(lat)
      latlim=latlim+diff(latlim)*c(-0.1,0.1)
      if(projection!='gnomonic'){
        orientation[1]=90-mean(latlim)
      }else{
        orientation[1]=mean(latlim)
      }
      orientation[2]=mean(longlim)
      labloc=c(longlim[1], latlim[1])
    }
      
    limgrid=expand.grid(seq(longlim[1],longlim[2],len=100),seq(latlim[1],latlim[2],len=100))
    templims=mapproject(limgrid[,1], limgrid[,2], projection=projection, parameters=parameters, orientation=orientation)
    plot.new()
    xlim=range(templims$x,na.rm = TRUE)
    ylim=range(templims$y, na.rm=TRUE)
    if(fliplong){xlim=rev(xlim)}
    plot.window(xlim=xlim, ylim=ylim, asp=1)
    if(grid==TRUE){
      longgrid=maglab(longlim, n=nlong, prettybase = prettybase)
      latgrid=maglab(latlim, n=nlat, prettybase = prettybase)
      longpretty=longgrid$tickat
      longpretty=longpretty[longpretty>longlim[1] & longpretty<longlim[2]]
      latpretty=latgrid$tickat
      latpretty=latpretty[latpretty>latlim[1] & latpretty<latlim[2]]
      for(i in 1:length(longpretty)){
        magproj(long=longpretty[i], lat=latlim+c(1e-9,-1e-9), type='l', add=TRUE, col=grid.col, lty=grid.lty)
      }
      for(i in 1:length(latpretty)){
        magproj(long=longlim+c(1e-9,-1e-9), lat=latpretty[i], type='l', add=TRUE, col=grid.col, lty=grid.lty)
      }
      #map.grid(c(longlim,latlim), nx=nlong, ny=nlat, labels=FALSE, col=grid.col, lty=grid.lty)
    }
    if(labels==TRUE){
      longgrid=maglab(longlim, n=nlong, prettybase = prettybase)
      latgrid=maglab(latlim, n=nlat, prettybase = prettybase)
      longpretty=longgrid$tickat
      longpretty=longpretty[longpretty>longlim[1] & longpretty<longlim[2]]
      latpretty=latgrid$tickat
      latpretty=latpretty[latpretty>latlim[1] & latpretty<latlim[2]]
      temp=mapproject(longpretty, rep(labloc[2],length(longpretty)))
      if(labeltype=='deg'){text(temp,labels = longpretty %% 360)}
      if(labeltype=='sex'){
        if(crunch==FALSE){text(temp,labels = deg2hms(longpretty %% 360,type='cat'))}
        if(crunch==TRUE){text(temp,labels = paste(deg2hms(longpretty %% 360,type='mat')[,1],'h',sep=''))}
      }
      temp=mapproject(rep(labloc[1],length(latpretty)), latpretty)
      if(labeltype=='deg'){text(temp,labels = latpretty)}
      if(labeltype=='sex'){
        if(crunch==FALSE){text(temp,labels = deg2dms(latpretty,type='cat'))}
        if(crunch==TRUE){text(temp,labels = paste(deg2dms(latpretty,type='mat')[,1],'\u00B0',sep=''))}
      }
    }
  }else{
    xlim=par()$usr[1:2]
    ylim=par()$usr[3:4]
  }
  
  if(type=='p'){
    temp=mapproject(long, lat)
    points(temp, ...)
  }
  
  if(type=='l'){
    long=approxfun(seq(0,1,len=length(long)), long)(seq(0,1,len=length(long)*upres))
    lat=approxfun(seq(0,1,len=length(lat)), lat)(seq(0,1,len=length(lat)*upres))
    temp=mapproject(long, lat)
    lines(temp, ...)
  }
  
  if(type=='pl'){
    long=ifelse(long < longlim[1],longlim[1],long)
    long=ifelse(long > longlim[2],longlim[2],long)
    lat=ifelse(lat < latlim[1],latlim[1],lat)
    lat=ifelse(lat > latlim[2],latlim[2],lat)
    checklims=diff(long)
    if(any(abs(checklims)>180)){
      lolong=which(abs(long-longlim[1])<=180)
      hilong=which(abs(long-longlim[2])<180)
        if(1 %in% lolong){
          templolonglong=long[lolong]
          templolonglat=lat[lolong]
          templolonglong=c(longlim[1]+1e-9,templolonglong,longlim[1]+1e-9,longlim[1]+1e-9)
          templolonglat=c(lat[max(hilong)],templolonglat,lat[min(hilong)],lat[max(hilong)])
          templolonglong=approxfun(seq(0,1,len=length(templolonglong)), templolonglong)(seq(0,1,len=length(templolonglong)*upres))
          templolonglat=approxfun(seq(0,1,len=length(templolonglat)), templolonglat)(seq(0,1,len=length(templolonglat)*upres))
          temp=mapproject(templolonglong, templolonglat)
          temp$x=ifelse(temp$x < min(xlim), min(xlim), temp$x)
          temp$x=ifelse(temp$x > max(xlim), max(xlim), temp$x)
          temp$y=ifelse(temp$y < min(ylim), min(ylim), temp$y)
          temp$y=ifelse(temp$y > max(ylim), max(ylim), temp$y)
          if(length(unique(temp$x))>1 | length(unique(temp$y))>1){
            polygon(temp, ...)
          }
          temphilonglong=long[hilong]
          temphilonglat=lat[hilong]
          temphilonglong=c(temphilonglong,longlim[2]-1e-9,longlim[2]-1e-9,temphilonglong[1])
          temphilonglat=c(temphilonglat,lat[max(hilong)],lat[min(hilong)],temphilonglat[1])
          temphilonglong=approxfun(seq(0,1,len=length(temphilonglong)), temphilonglong)(seq(0,1,len=length(temphilonglong)*upres))
          temphilonglat=approxfun(seq(0,1,len=length(temphilonglat)), temphilonglat)(seq(0,1,len=length(temphilonglat)*upres))
          temp=mapproject(temphilonglong, temphilonglat)
          temp$x=ifelse(temp$x < min(xlim), min(xlim), temp$x)
          temp$x=ifelse(temp$x > max(xlim), max(xlim), temp$x)
          temp$y=ifelse(temp$y < min(ylim), min(ylim), temp$y)
          temp$y=ifelse(temp$y > max(ylim), max(ylim), temp$y)
          if(length(unique(temp$x))>1 | length(unique(temp$y))>1){
            polygon(temp, ...)
          }
        }
        if(1 %in% hilong){
          templolonglong=long[lolong]
          templolonglat=lat[lolong]
          templolonglong=c(longlim[1]+1e-9,templolonglong,longlim[1]+1e-9,longlim[1]+1e-9)
          templolonglat=c(lat[min(lolong)],templolonglat,lat[max(lolong)],lat[min(lolong)])
          templolonglong=approxfun(seq(0,1,len=length(templolonglong)), templolonglong)(seq(0,1,len=length(templolonglong)*upres))
          templolonglat=approxfun(seq(0,1,len=length(templolonglat)), templolonglat)(seq(0,1,len=length(templolonglat)*upres))
          temp=mapproject(templolonglong, templolonglat)
          temp$x=ifelse(temp$x < min(xlim), min(xlim), temp$x)
          temp$x=ifelse(temp$x > max(xlim), max(xlim), temp$x)
          temp$y=ifelse(temp$y < min(ylim), min(ylim), temp$y)
          temp$y=ifelse(temp$y > max(ylim), max(ylim), temp$y)
          if(length(unique(temp$x))>1 | length(unique(temp$y))>1){
            polygon(temp, ...)
          }
          temphilonglong=long[hilong]
          temphilonglat=lat[hilong]
          temphilonglong=c(temphilonglong,longlim[2]-1e-9,longlim[2]-1e-9,temphilonglong[1])
          temphilonglat=c(temphilonglat,lat[min(lolong)],lat[max(lolong)],temphilonglat[1])
          temphilonglong=approxfun(seq(0,1,len=length(temphilonglong)), temphilonglong)(seq(0,1,len=length(temphilonglong)*upres))
          temphilonglat=approxfun(seq(0,1,len=length(temphilonglat)), temphilonglat)(seq(0,1,len=length(temphilonglat)*upres))
          temp=mapproject(temphilonglong, temphilonglat)
          temp$x=ifelse(temp$x < min(xlim), min(xlim), temp$x)
          temp$x=ifelse(temp$x > max(xlim), max(xlim), temp$x)
          temp$y=ifelse(temp$y < min(ylim), min(ylim), temp$y)
          temp$y=ifelse(temp$y > max(ylim), max(ylim), temp$y)
          if(length(unique(temp$x))>1 | length(unique(temp$y))>1){
            polygon(temp, ...)
          }
        }
    }else{
    long=approxfun(seq(0,1,len=length(long)), long)(seq(0,1,len=length(long)*upres))
    lat=approxfun(seq(0,1,len=length(lat)), lat)(seq(0,1,len=length(lat)*upres))
    temp=mapproject(long, lat)
    temp$x=ifelse(temp$x < min(xlim), min(xlim), temp$x)
    temp$x=ifelse(temp$x > max(xlim), max(xlim), temp$x)
    temp$y=ifelse(temp$y < min(ylim), min(ylim), temp$y)
    temp$y=ifelse(temp$y > max(ylim), max(ylim), temp$y)
    if(length(unique(temp$x))>1 | length(unique(temp$y))>1){
      polygon(temp, ...)
    }
    }
  }
  
  if(type=='t'){
    temp=mapproject(long, lat)
    text(temp, labels=plottext, ...)
  }
  
  if(type=='b' & length(long)==2 & length(lat)==2){
    lolong=long[1]; hilong=long[2]
    lolat=lat[1]; hilat=lat[2]
    if(lolong<hilong){
      long=c(lolong, hilong, hilong, lolong, lolong)
      lat=c(lolat, lolat, hilat, hilat, lolat)
      long=approxfun(seq(0,1,len=length(long)), long)(seq(0,1,len=length(long)*(ceiling(upres/4)*4)))
      lat=approxfun(seq(0,1,len=length(lat)), lat)(seq(0,1,len=length(lat)*(ceiling(upres/4)*4)))
      temp=mapproject(long, lat)
      polygon(temp, ...)
    }else{
      long=c(lolong, longlim[2]-1e-9, longlim[2]-1e-9, lolong, lolong)
      lat=c(lolat, lolat, hilat, hilat, lolat)
      long=approxfun(seq(0,1,len=length(long)), long)(seq(0,1,len=length(long)*(ceiling(upres/4)*4)))
      lat=approxfun(seq(0,1,len=length(lat)), lat)(seq(0,1,len=length(lat)*(ceiling(upres/4)*4)))
      temp=mapproject(long, lat)
      polygon(temp, ...)
      
      long=c(longlim[1]+1e-9, hilong, hilong, longlim[1]+1e-9, longlim[1]+1e-9)
      lat=c(lolat, lolat, hilat, hilat, lolat)
      long=approxfun(seq(0,1,len=length(long)), long)(seq(0,1,len=length(long)*(ceiling(upres/4)*4)))
      lat=approxfun(seq(0,1,len=length(lat)), lat)(seq(0,1,len=length(lat)*(ceiling(upres/4)*4)))
      temp=mapproject(long, lat)
      polygon(temp, ...)
    }
  }
  
  if(add==FALSE & box==TRUE){
    magproj(long=longlim+c(1e-9,-1e-9), lat=latlim+c(1e-9,-1e-9), add=TRUE, border='black', lwd=2)
  }
  .Last.projection(list(projection=.Last.projection()$projection, parameters=.Last.projection()$parameters, orientation=.Last.projection()$orientation, centre=centre, longlim=longlim, latlim=latlim))
}

magprojgrid=function(nlat=6, nlong=6, prettybase=30, box=TRUE, ...){
  centre=.Last.projection()$centre
  longlim=.Last.projection()$longlim
  latlim=.Last.projection()$latlim
  longgrid=maglab(longlim, n=nlong, prettybase = prettybase)
  latgrid=maglab(latlim, n=nlat, prettybase = prettybase)
  longpretty=longgrid$tickat
  longpretty=longpretty[longpretty>longlim[1] & longpretty<longlim[2]]
  latpretty=latgrid$tickat
  latpretty=latpretty[latpretty>latlim[1] & latpretty<latlim[2]]
  for(i in 1:length(longpretty)){
    magproj(long=longpretty[i], lat=latlim+c(1e-9,-1e-9), type='l', add=TRUE, ...)
  }
  for(i in 1:length(latpretty)){
    magproj(long=longlim+c(1e-9,-1e-9), lat=latpretty[i], type='l', add=TRUE, ...)
  }
  if(box){
    magproj(long=longlim+c(1e-9,-1e-9), lat=latlim+c(1e-9,-1e-9), add=TRUE, border='black', lwd=2)
  }
  .Last.projection(list(projection=.Last.projection()$projection, parameters=.Last.projection()$parameters, orientation=.Last.projection()$orientation, centre=centre, longlim=longlim, latlim=latlim))
}

magprojlabels=function(nlat=6, nlong=6, prettybase=30, labloc = c(90, -45), labeltype='deg', crunch=FALSE, ...){
  centre=.Last.projection()$centre
  longlim=.Last.projection()$longlim
  latlim=.Last.projection()$latlim
  longgrid=maglab(longlim, n=nlong, prettybase = prettybase)
  latgrid=maglab(latlim, n=nlat, prettybase = prettybase)
  longpretty=longgrid$tickat
  longpretty=longpretty[longpretty>longlim[1] & longpretty<longlim[2]]
  latpretty=latgrid$tickat
  latpretty=latpretty[latpretty>latlim[1] & latpretty<latlim[2]]
  
  temp=mapproject(longpretty, rep(labloc[2],length(longpretty)))
  if(labeltype=='deg'){text(temp,labels = longpretty %% 360, ...)}
  if(labeltype=='sex'){
    if(crunch==FALSE){text(temp,labels = deg2hms(longpretty %% 360,type='cat'), ...)}
    if(crunch==TRUE){text(temp,labels = paste(deg2hms(longpretty %% 360,type='mat')[,1],'h',sep=''), ...)}
  }
  temp=mapproject(rep(labloc[1],length(latpretty)), latpretty)
  if(labeltype=='deg'){text(temp,labels = latpretty, ...)}
  if(labeltype=='sex'){
    if(crunch==FALSE){text(temp,labels = deg2dms(latpretty,type='cat'), ...)}
    if(crunch==TRUE){text(temp,labels = paste(deg2dms(latpretty,type='mat')[,1],'\u00B0',sep=''), ...)}
  }
  .Last.projection(list(projection=.Last.projection()$projection, parameters=.Last.projection()$parameters, orientation=.Last.projection()$orientation, centre=centre, longlim=longlim, latlim=latlim))
}
