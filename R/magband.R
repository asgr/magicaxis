.car2sph=function (x, y, z, deg = TRUE) 
{
    if (is.matrix(x) || is.data.frame(x)) {
        if (ncol(x) == 1) {
            x = x[, 1]
        }
        else if (ncol(x) == 2) {
            y = x[, 2]
            x = x[, 1]
        }
        else if (ncol(x) == 3) {
            z = x[, 3]
            y = x[, 2]
            x = x[, 1]
        }
    }
    if (missing(x) | missing(y) | missing(z)) {
        stop("Missing full cartesian 3D input data.")
    }
    radius = sqrt(x^2 + y^2 + z^2)
    long = atan2(y, x)
    lat = asin(z/radius)
    if (deg) {
        long = long * 180/pi
        lat = lat * 180/pi
    }
    lat[radius == 0] = 0
    return = cbind(long = long, lat = lat, radius = radius)
}

.rotdata3d=function(x,y,z,theta,dim='z'){
  out=.makerotmat3d(theta,dim=dim) %*% rbind(x,y,z)
  return=cbind(out[1,],out[2,],out[3,])
}

.makerotmat3d=function(theta,dim='z'){
  theta=theta*pi/180
  sintheta=sin(theta)
  costheta=cos(theta)
  
  if(dim=='x' | dim==1){
    out=matrix(c(
      1           , 0           , 0          ,
      0           , costheta    , -sintheta  ,
      0           , sintheta    , costheta
    ),ncol=3,byrow=T)
  }
  
  if(dim=='y' | dim==2){
    out=matrix(c(
      costheta    , 0           , -sintheta  ,
      0           , 1           , 0          ,
      sintheta    , 0           , costheta
    ),ncol=3,byrow=T)
  }
  
  if(dim=='z' | dim==3){
    out=matrix(c(
      costheta    , -sintheta   , 0          ,
      sintheta    , costheta    , 0          ,
      0           , 0           , 1
    ),ncol=3,byrow=T)
  }
  
  return=out
}

.ring=function(crosseq = 0, peaklat = 0, longlo=-180, offset=0, res=1000){
  temp=cbind(cos(seq(-pi, pi, len = res))*cos(offset*pi/180), -sin(offset*pi/180), sin(seq(-pi, pi, len = res))*cos(offset*pi/180))
  temp = .rotdata3d(temp[,1],temp[,2],temp[,3], theta=90-peaklat, dim='x')
  temp = .rotdata3d(temp[,1],temp[,2],temp[,3], theta=180-crosseq, dim='z')
  temp=.car2sph(temp)[,1:2]
  temp[,1]= (temp[,1]-longlo) %% 360 + longlo
  temp = temp[order(temp[,1]),]
  return = temp
}

".Last.magroj"<-
  local({
    val <- list(
      projection = "", 
      parameters = NULL, 
      orientation = NULL,
      centre = NULL,
      longlim = NULL,
      latlim = NULL
    )
    function(new) if(!missing(new)) val <<- new else val
  })

magband=function(crosseq = 0, peaklat = 0, width=10, res=1000, ...){
  longlo=.Last.magproj()$longlim[1]
  loring = .ring(crosseq = crosseq, peaklat = peaklat, offset=-width/2, longlo=longlo, res=res)
  hiring = .ring(crosseq = crosseq, peaklat = peaklat, offset=width/2, longlo=longlo, res=res)
  temp=rbind(loring, hiring[order(hiring[,1],decreasing = T),],loring[1,])
  magproj(temp, type='pl', add=TRUE, upres=10, ...)
}

magring=function(crosseq = 0, peaklat = 0, offset=0, res=1000, ...){
  longlo=.Last.magproj()$longlim[1]
  temp = .ring(crosseq = crosseq, peaklat = peaklat, offset=offset, longlo=longlo, res=res)
  magproj(temp, type='l', add=TRUE, upres=10, ...)
}

magecliptic=function(width=10, ...){
  longlo=.Last.magproj()$longlim[1]
  if(width>0){magband(0, 23.4, width=width, ...)}
  if(width==0){magring(0,23.4, ...)}
}

magMWplane=function(width=10, ...){
  longlo=.Last.magproj()$longlim[1]
  if(width>0){magband(76.75, 62.6, width=width, ...)}
  if(width==0){magring(76.75, 62.6, ...)}
}

magsun=function(Ydate='get', anti=FALSE, ...){
  if (Ydate[1] == "get") {
    Ydate = c(format(Sys.Date(), "%m"), format(Sys.Date(),"%d"))
    Ydate = as.numeric(Ydate)
  }
  year = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  yearfrac = sum(year[1:Ydate[1]]) - Ydate[1] + Ydate[2]
  yearfrac = yearfrac - 108
  if (yearfrac < 0) {
    yearfrac = yearfrac + 365
  }
  yearfrac = (yearfrac * 2 * pi/365) * 180/pi
  if(anti){
    sunloc = c((yearfrac+180) %% 360, -sin(yearfrac * pi/180) * 23.4)
  }else{
    sunloc = c(yearfrac, sin(yearfrac * pi/180) * 23.4)
  }
  magproj(sunloc[1], sunloc[2], type='p', add=TRUE, ...)
}

magMW=function(...){
  magproj(266.42, -29, type='p', add=TRUE, ...)
}