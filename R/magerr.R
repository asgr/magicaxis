magerr=function(x, y, xlo=FALSE, ylo=FALSE, xhi=xlo, yhi=ylo, corxy, log='', length=0.02, col='black',fill=FALSE,...){
if(missing(corxy)){
  xhi=x+abs(xhi)
  xlo=x-abs(xlo)
  yhi=y+abs(yhi)
  ylo=y-abs(ylo)
  if(log=='x'){
  	sel=which(xlo<=0)
  	xlo[sel]=1e-300
  }
  if(log=='y'){
  	sel=which(ylo<=0)
  	ylo[sel]=1e-300
  	}
  if(log=='xy' | log=='yx'){
  	sel=which(xlo<=0)
  	xlo[sel]=1e-300
  	sel=which(ylo<=0)
  	ylo[sel]=1e-300
  }
  	
  	if(any(xlo != FALSE)){
  	arrows(x,y,xhi,y,angle=90,length=length,col=col,...)
  	arrows(x,y,xlo,y,angle=90,length=length,col=col,...)
  	}
  	if(any(ylo != FALSE)){
  	arrows(x,y,x,yhi,angle=90,length=length,col=col,...)
  	arrows(x,y,x,ylo,angle=90,length=length,col=col,...)
  	}
}else{
  if(any(xlo==FALSE) | any(ylo==FALSE)){stop('For error ellipses all xlo and ylo values must have real positive values.')}
  if(any(xlo!=xhi) | any(ylo!=yhi)){stop('xlo/ylo must equal xhi/yhi (i.e. the errors must be symmetric)')}
  n = length(x)
  a = rep(0,n)
  b = rep(0,n)
  angle = rep(0,n)
  for(i in 1:n){
    Cov = matrix(c(xlo[i]^2,xlo[i]*ylo[i]*corxy[i],xlo[i]*ylo[i]*corxy[i],ylo[i]^2),2)
    E = eigen(Cov)
    a[i] = sqrt(E$values[1])
    b[i] = sqrt(E$values[2])
    angle[i] = atan2(E$vector[2,1],E$vector[1,1])*180/pi
  }
  if(fill){draw.ellipse(x,y,a=a,b=b,angle=angle,border=col,col=col,...)}else{draw.ellipse(x,y,a=a,b=b,angle=angle,border=col,col=NULL,...)}
}
}
