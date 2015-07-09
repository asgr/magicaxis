magproj=function(long, lat, type='b', plottext, longlim=c(-180,180), latlim=c(-70,70), projection="mollweide", parameters=NULL, centre=c(0,0), add=FALSE, nx=6, ny=6, labels=TRUE, grid=TRUE, grid.col='grey', grid.lty=2, auto=FALSE, upres=100, box=TRUE, labloc=c(-75,-50), labeltype='deg', ...){
  
  if(is.matrix(long) | is.data.frame(long)){
    lat = long[, 2]
    long = long[, 1]
  }
  
  if(add==TRUE){
    orientation=.Last.projection()$orientation
    projection=.Last.projection()$projection
  }else{
    orientation=c(90+centre[2], centre[1] %% 360, 0)
    #orientation[1]=(orientation[1]+90) %% 180 -90
    #orientation[2]=(orientation[2]+180) %% 360 -180
  }
  long= (long+(180-orientation[2])) %% 360 - (180-orientation[2])
  lat= (lat+90) %% 180 - 90
  if(longlim[1]<orientation[2]-180){
    longlim[2]=longlim[2]+orientation[2]-180-longlim[1]
    longlim[1]=orientation[2]-180
  }
  
  if(add==FALSE){
    if(auto==TRUE){
      longlim=range(long)
      longlim=longlim+diff(longlim)*c(-0.1,0.1)
      latlim=range(lat)
      latlim=latlim+diff(latlim)*c(-0.1,0.1)
      orientation[1]=90-mean(latlim)
      orientation[2]=mean(longlim)
      labloc=c(longlim[1], latlim[1])
    }
      
    limgrid=expand.grid(seq(longlim[1],longlim[2],len=100),seq(latlim[1],latlim[2],len=100))
    templims=mapproject(limgrid[,1], limgrid[,2], projection=projection, parameters=parameters, orientation=orientation)
    plot.new()
    plot.window(xlim=range(templims$x,na.rm = TRUE), ylim=range(templims$y, na.rm=TRUE), asp=1)
    if(grid==TRUE){
      map.grid(c(longlim,latlim), nx=nx, ny=ny, labels=FALSE, col=grid.col, lty=grid.lty)
    }
    if(labels==TRUE){
      longpretty=pretty(longlim,nx)
      latpretty=pretty(latlim,ny)
      longpretty=longpretty[longpretty>longlim[1] & longpretty<longlim[2]]
      latpretty=latpretty[latpretty>latlim[1] & latpretty<latlim[2]]
      temp=mapproject(longpretty, rep(labloc[2],length(longpretty)))
      #longpretty=longpretty[order(temp$x)]
      #temp$y=temp$y[order(temp$x)]
      #temp$x=temp$x[order(temp$x)]
      if(labeltype=='deg'){text(temp,labels = longpretty %% 360)}
      if(labeltype=='sex'){text(temp,labels = deg2hms(longpretty %% 360,type='cat'))}
      temp=mapproject(rep(labloc[1],length(latpretty)), latpretty)
      #latpretty=latpretty[order(temp$y)]
      #temp$y=temp$y[order(temp$y)]
      #temp$x=temp$x[order(temp$y)]
      if(labeltype=='deg'){text(temp,labels = latpretty)}
      if(labeltype=='sex'){text(temp,labels = deg2dms(latpretty,type='cat'))}
    }
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
      long=approxfun(seq(0,1,len=length(long)), long)(seq(0,1,len=length(long)*upres))
      lat=approxfun(seq(0,1,len=length(lat)), lat)(seq(0,1,len=length(lat)*upres))
      temp=mapproject(long, lat)
      polygon(temp, ...)
    }else{
      long=c(lolong, longlim[2]-1e-9, longlim[2]-1e-9, lolong, lolong)
      lat=c(lolat, lolat, hilat, hilat, lolat)
      long=approxfun(seq(0,1,len=length(long)), long)(seq(0,1,len=length(long)*upres))
      lat=approxfun(seq(0,1,len=length(lat)), lat)(seq(0,1,len=length(lat)*upres))
      temp=mapproject(long, lat)
      polygon(temp, ...)
      
      long=c(longlim[1]+1e-9, hilong, hilong, longlim[1]+1e-9, longlim[1]+1e-9)
      lat=c(lolat, lolat, hilat, hilat, lolat)
      long=approxfun(seq(0,1,len=length(long)), long)(seq(0,1,len=length(long)*upres))
      lat=approxfun(seq(0,1,len=length(lat)), lat)(seq(0,1,len=length(lat)*upres))
      temp=mapproject(long, lat)
      polygon(temp, ...)
    }
  }
  
  if(add==FALSE & box==TRUE){
    lolong=min(longlim)+1e-9; hilong=max(longlim)-1e-9
    lolat=min(latlim)+1e-9; hilat=max(latlim)-1e-9
    longbox=c(lolong, hilong, hilong, lolong, lolong)
    latbox=c(lolat, lolat, hilat, hilat, lolat)
    longbox=approxfun(seq(0,1,len=length(longbox)), longbox)(seq(0,1,len=length(longbox)*upres))
    latbox=approxfun(seq(0,1,len=length(latbox)), latbox)(seq(0,1,len=length(latbox)*upres))
    temp=mapproject(longbox, latbox)
    lines(temp, col='black')
  }
}