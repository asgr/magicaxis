magerr=function(x, y, xlo, ylo, xhi=xlo, yhi=ylo, corxy, length=0.02, col='black',fill=FALSE,...){
if(missing(corxy)){
  if(!missing(xlo)){xlodraw=x-abs(xlo);doxlo=TRUE}else{xlodraw=0;doxlo=FALSE}
  if(!missing(xlo) | !missing(xhi)){xhidraw=x+abs(xhi);doxhi=TRUE}else{xhidraw=0;doxhi=FALSE}
  if(!missing(ylo)){ylodraw=y-abs(ylo);doylo=TRUE}else{ylodraw=0;doylo=FALSE}
  if(!missing(ylo) | !missing(yhi)){yhidraw=y+abs(yhi);doyhi=TRUE}else{yhidraw=0;doyhi=FALSE}
  
  if(doxlo & par()$xlog){
  	sel=which(xlodraw<=0)
  	xlodraw[sel]=1e-300
  }
  if(doxhi & par()$xlog){
    sel=which(xhidraw>0 & x<0)
    x[sel]=1e-300
  }
  if(doylo & par()$ylog){
  	sel=which(ylodraw<=0)
  	ylodraw[sel]=1e-300
  }
  if(doyhi & par()$ylog){
    sel=which(yhidraw>0 & y<0)
    y[sel]=1e-300
  }

  if(doxlo & any(is.finite(xlodraw))){arrows(x,y,xlodraw,y,angle=90,length=length,col=col,...)}
  if(doxhi & any(is.finite(xhidraw))){arrows(x,y,xhidraw,y,angle=90,length=length,col=col,...)}
  if(doylo & any(is.finite(ylodraw))){arrows(x,y,x,ylodraw,angle=90,length=length,col=col,...)}
  if(doyhi & any(is.finite(yhidraw))){arrows(x,y,x,yhidraw,angle=90,length=length,col=col,...)}
  
}else{
  if(missing(xlo) | missing(ylo)){stop('For error ellipses xlo and ylo must be specified explicitly.')}
  if(any(is.finite(xlo)==FALSE) | any(is.finite(ylo)==FALSE)){stop('For error ellipses all xlo and ylo values must have real values.')}
  if(any(xlo<0) | any(ylo<0)){stop('For error ellipses all xlo and ylo values must have real values.')}
  if(any(xlo!=xhi) | any(ylo!=yhi)){stop('xlo/ylo must equal xhi/yhi (i.e. the errors must be symmetric)')}
  n = length(x)
  a = rep(0,n)
  b = rep(0,n)
  angle = rep(0,n)
  for(i in 1:n){
    if(xlo[i]>0 & ylo[i]>0){
      Cov = matrix(c(xlo[i]^2,xlo[i]*ylo[i]*corxy[i],xlo[i]*ylo[i]*corxy[i],ylo[i]^2),2)
      E = eigen(Cov)
      a[i] = sqrt(E$values[1])
      b[i] = sqrt(E$values[2])
      angle[i] = atan2(E$vector[2,1],E$vector[1,1])*180/pi
    }
  }
  if(fill){draw.ellipse(x,y,a=a,b=b,angle=angle,border=col,col=col,...)}else{draw.ellipse(x,y,a=a,b=b,angle=angle,border=col,col=NULL,...)}
}
}
