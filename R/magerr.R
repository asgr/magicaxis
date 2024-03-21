magerr = function(x, y, xlo, ylo, xhi=xlo, yhi=ylo, corxy, length=0.02, col='black', fill=FALSE, poly=FALSE,...){
  if(missing(x)){stop('Need x!')}
  
  if (missing(y)) {
    if (is.data.frame(x) | is.matrix(x)) {
      if (ncol(x) == 2) {
        y = x[, 2]
        x = x[, 1]
      }else{
        stop('x can only be two columns if using to provide x and y data!')
      }
    }
  }
  
  if(missing(y)){stop('Need y!')}
  
  x[is.infinite(x)]=1e300*sign(x[is.infinite(x)])
  y[is.infinite(y)]=1e300*sign(y[is.infinite(y)])
  x[!is.finite(x)]=NA
  y[!is.finite(y)]=NA
  
  if(length(col)==1){col=rep(col,length(x))}
  if(!missing(xlo)){
    if(length(xlo)==1){xlo=rep(xlo,length(x))}
    xlo[!is.finite(xlo)]=1e300*sign(xlo[!is.finite(xlo)])
  }
  if(!missing(xhi)){
    if(length(xhi)==1){xhi=rep(xhi,length(x))}
    xhi[!is.finite(xhi)]=1e300*sign(xhi[!is.finite(xhi)])
  }
  if(!missing(ylo)){
    if(length(ylo)==1){ylo=rep(ylo,length(x))}
    ylo[!is.finite(ylo)]=1e300*sign(ylo[!is.finite(ylo)])
  }
  if(!missing(yhi)){
    if(length(yhi)==1){yhi=rep(yhi,length(x))}
    yhi[!is.finite(yhi)]=1e300*sign(yhi[!is.finite(yhi)])
  }
  if(!missing(corxy)){
    errbarsel=which(xlo==0 | ylo==0)
  }else{
    errbarsel=1:length(x)
  }
  if(length(errbarsel)>0){
    if(!missing(xlo)){xlodraw=x-abs(xlo);doxlo=TRUE}else{xlodraw=0;doxlo=FALSE}
    if(!missing(xlo) | !missing(xhi)){xhidraw=x+abs(xhi);doxhi=TRUE}else{xhidraw=0;doxhi=FALSE}
    if(!missing(ylo)){ylodraw=y-abs(ylo);doylo=TRUE}else{ylodraw=0;doylo=FALSE}
    if(!missing(ylo) | !missing(yhi)){yhidraw=y+abs(yhi);doyhi=TRUE}else{yhidraw=0;doyhi=FALSE}
    
    if(doxlo & par()$xlog){
    	sel=which(xlodraw<=0)
    	xlodraw[sel]=1e-300
    }
    if(doxhi & par()$xlog){
      sel=which(xhidraw>0 & x<=0)
      x[sel]=1e-300
    }
    if(doylo & par()$ylog){
    	sel=which(ylodraw<=0)
    	ylodraw[sel]=1e-300
    }
    if(doyhi & par()$ylog){
      sel=which(yhidraw>0 & y<=0)
      y[sel]=1e-300
    }
    
    xarrow=x[errbarsel]
    yarrow=y[errbarsel]
    xlodraw=xlodraw[errbarsel]
    xhidraw=xhidraw[errbarsel]
    ylodraw=ylodraw[errbarsel]
    yhidraw=yhidraw[errbarsel]
    colarrow=col[errbarsel]
    
    xlodraw[xlodraw == -Inf]=-1e300
    xhidraw[xhidraw == Inf]=1e300
    ylodraw[ylodraw == -Inf]=-1e300
    yhidraw[yhidraw == Inf]=1e300
    
    if(poly==FALSE){
      if(doxlo & any(is.finite(xlodraw))){
        finalsel=xarrow>xlodraw
        arrows(xarrow[finalsel],yarrow[finalsel],xlodraw[finalsel],yarrow[finalsel],angle=90,length=length,col=colarrow[finalsel],...)
      }
      if(doxhi & any(is.finite(xhidraw))){
        finalsel=xarrow<xhidraw
        arrows(xarrow[finalsel],yarrow[finalsel],xhidraw[finalsel],yarrow[finalsel],angle=90,length=length,col=colarrow[finalsel],...)
      }
      if(doylo & any(is.finite(ylodraw))){
        finalsel=yarrow>ylodraw
        arrows(xarrow[finalsel],yarrow[finalsel],xarrow[finalsel],ylodraw[finalsel],angle=90,length=length,col=colarrow[finalsel],...)
      }
      if(doyhi & any(is.finite(yhidraw))){
        finalsel=yarrow<yhidraw
        arrows(xarrow[finalsel],yarrow[finalsel],xarrow[finalsel],yhidraw[finalsel],angle=90,length=length,col=colarrow[finalsel],...)
      }
    }
  }
  if(poly==FALSE){
    if(!missing(corxy)){
      n = length(x)
      if(length(corxy)==1){corxy=rep(corxy,n)}
      if(missing(xlo) | missing(ylo)){stop('For error ellipses xlo and ylo must be specified explicitly.')}
      if(any(is.finite(xlo)==FALSE) | any(is.finite(ylo)==FALSE)){stop('For error ellipses all xlo and ylo values must have real values.')}
      if(any(xlo!=xhi) | any(ylo!=yhi)){stop('xlo/ylo must equal xhi/yhi (i.e. the errors must be symmetric)')}
      for(i in 1:n){
        if(xlo[i]>0 & ylo[i]>0){
          Cov = matrix(c(xlo[i]^2,xlo[i]*ylo[i]*corxy[i],xlo[i]*ylo[i]*corxy[i],ylo[i]^2),2)
          E = eigen(Cov)
          a = sqrt(E$values[1])
          b = sqrt(E$values[2])
          angle = atan2(E$vector[2,1],E$vector[1,1])*180/pi
          if(fill){draw.ellipse(x[i],y[i],a=a,b=b,angle=angle,border=col[i],col=col[i],...)}else{draw.ellipse(x[i],y[i],a=a,b=b,angle=angle,border=col[i],col=NULL,...)}
        }
        if(xlo[i]>0 & ylo[i]==0){
          #Nothing yet
        }
        if(xlo[i]==0 & ylo[i]>0){
          #Nothing yet
        }
      }
    }
  }
  if(poly){
    polygon(c(xarrow,rev(xarrow)),c(ylodraw,rev(yhidraw)),col=col,...)
  }
}
