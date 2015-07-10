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

.ring=function(crosseq = 0, peaklat = 0, offset=0, longlo=-180, res=1000, ...){
  temp=cbind(cos(seq(-pi, pi, len = res))*cos(offset*pi/180), -sin(offset*pi/180), sin(seq(-pi, pi, len = res))*cos(offset*pi/180))
  temp = .rotdata3d(temp[,1],temp[,2],temp[,3], theta=90-peaklat, dim='x')
  temp = .rotdata3d(temp[,1],temp[,2],temp[,3], theta=180-crosseq, dim='z')
  temp=.car2sph(temp)[,1:2]
  temp[,1]= (temp[,1]-longlo) %% 360 + longlo
  temp = temp[order(temp[,1]),]
  return = temp
}

magband=function(crosseq = 0, peaklat = 0, longlo=-180, width=0, res=1000, ...){
  loring = .ring(crosseq = crosseq, peaklat = peaklat, offset=-width, longlo=longlo, res=res)
  hiring = .ring(crosseq = crosseq, peaklat = peaklat, offset=width, longlo=longlo, res=res)
  temp=rbind(loring, hiring[order(hiring[,1],decreasing = T),],loring[1,])
  magproj(temp, type='pl', add=TRUE, upres=10,...)
}
