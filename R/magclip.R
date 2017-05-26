magclip=function(x, sigma='auto', clipiters=5, sigmasel=1, estimate='both'){
  sel = !is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)
  if(clipiters>0){
    newlen=length(x)
    clipx=x[sel]
    for(i in 1:clipiters){
      oldlen=newlen
      roughmed=median(clipx, na.rm=TRUE)
      if(sigma=='auto'){
        clipsigma=qnorm(1-2/max(newlen,2,na.rm=TRUE))
      }else{
        clipsigma=sigma
      }
      if(estimate=='both'){
        vallims=clipsigma*diff(quantile(clipx,pnorm(c(-sigmasel,sigmasel)), na.rm=TRUE))/2/sigmasel
      }
      if(estimate=='lo'){
        vallims=clipsigma*diff(quantile(clipx,pnorm(c(-sigmasel,0)), na.rm=TRUE))/sigmasel
      }
      if(estimate=='hi'){
        vallims=clipsigma*diff(quantile(clipx,pnorm(c(0,sigmasel)), na.rm=TRUE))/sigmasel
      }
      cliplogic=x>=(roughmed-vallims) & x<=(roughmed+vallims) & sel
      clipx=x[which(cliplogic)]
      newlen=length(clipx)
      if(oldlen==newlen | newlen<=1){break}
    }
  }else{
    cliplogic=rep(TRUE,length(clipx))
  }
  return=list(x=clipx, clip=cliplogic, range=range(clipx, na.rm = TRUE))
}