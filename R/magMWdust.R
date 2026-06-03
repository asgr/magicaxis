#=========================================
#
# File Name : magMWdust.R
# Created By : awright
# Creation Date : 03-06-2026
# Last Modified : Wed Jun  3 10:25:44 2026
#
#=========================================


magMWdust<-function(type='p',pch=16,pt.cex=0.5,
                    dlon=1,dlat=1,
                    opacity.range=c(0,0.5),
                    whiteblack.percentile=c(0.5,0.95),stretch='lin',
                    min.opacity.plot=0.01,...) { 

  if (!type%in%c('pl','p')) stop("magMWdust function expects type of 'p' (for points) or 'pl' (for polygons) only")

  sky<-expand.grid(seq(0+dlon,360-dlon,by=dlon),seq(-90+dlat,90-dlat,by=dlat))
  gal<-helpRfuncs::eq2gal(sky[,1],sky[,2],eq="B1950")
  dust_all<-cbind(ra=sky[,1],dec=sky[,2],dust_getval(gal[,1],gal[,2],map = "Ebv", interp = TRUE))
  dust_all$map<-magicaxis::magmap(dust_all$ebv,range=opacity.range,hicut=whiteblack.percentile[2],locut=whiteblack.percentile[1],stretch=stretch)$map
  dust<-dust_all[which(dust_all$map>min.opacity.plot),]

  if (type=='pl'){ 
    pb<-txtProgressBar(style=3,min=1,max=nrow(dust))
    for (i in 1:nrow(dust)) { 
      magicaxis::magproj(c(dust$ra[i] -dlon/2,dust$ra[i] -dlon/2,dust$ra[i] +dlon/2,dust$ra[i] +dlon/2),
                         c(dust$dec[i]-dlat/2,dust$dec[i]+dlat/2,dust$dec[i]+dlat/2,dust$dec[i]-dlat/2),
                         type=type, add=TRUE,col=hsv(v=0,alpha=dust$map[i]),...)
      setTxtProgressBar(pb,i)
    }
    close(pb)
  } else { 
    magicaxis::magproj(dust$ra,dust$dec,
                       type='p', add=TRUE, border=NA,pch=pch,cex=pt.cex,col=hsv(v=0,alpha=dust$map),...)
  }
  return(invisible(NULL))
}
