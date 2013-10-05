magerr=function(x, y, xlo=FALSE, ylo=FALSE, xhi=xlo, yhi=ylo, log='', length=0.02,...){
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
	arrows(x,y,xhi,y,angle=90,length=length,...)
	arrows(x,y,xlo,y,angle=90,length=length,...)
	}
	if(any(ylo != FALSE)){
	arrows(x,y,x,yhi,angle=90,length=length,...)
	arrows(x,y,x,ylo,angle=90,length=length,...)
	}
}
