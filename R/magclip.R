magclip=function(x, sigma='auto', clipiters=5, sigmasel=1, estimate='both', extra=TRUE){
  if(extra){
    xord=order(x)
    sel = !is.na(x[xord]) & !is.nan(x[xord]) & !is.null(x[xord]) & is.finite(x[xord])
    clipx=x[xord][sel]
  }else{
    sel = !is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)
    clipx=sort(x[sel])
  }
  
  if(clipiters>0 & length(x)>0){
    newlen=length(clipx)
    sigcut=pnorm(sigmasel)
    
    for(i in 1:clipiters){
      if(newlen<=1){break}
      oldlen=newlen
      roughmed=clipx[newlen/2]
      if(sigma=='auto'){
        clipsigma=qnorm(1-2/max(newlen,2,na.rm=TRUE))
      }else{
        clipsigma=sigma
      }
      if(estimate=='both'){
        #vallims=clipsigma*diff(quantile(clipx,c(1-sigcut,sigcut)))/2/sigmasel
        vallims=clipsigma*(clipx[sigcut*newlen]-clipx[(1-sigcut)*newlen])/2/sigmasel
      }
      if(estimate=='lo'){
        #vallims=clipsigma*(roughmed-quantile(clipx,1-sigcut))/sigmasel
        vallims=clipsigma*(roughmed-clipx[(1-sigcut)*newlen])/sigmasel
      }
      if(estimate=='hi'){
        #vallims=clipsigma*(quantile(clipx,sigcut)-roughmed)/sigmasel
        vallims=clipsigma*(clipx[sigcut*newlen]-roughmed)/sigmasel
      }
      if(extra){
        cliplogic=x[xord]>=(roughmed-vallims) & x[xord]<=(roughmed+vallims) & sel
        clipx=x[xord][which(cliplogic)]
        newlen=length(clipx)
      }else{
        clipx=clipx[clipx>=(roughmed-vallims) & clipx<=(roughmed+vallims)]
        newlen=length(clipx)
      }
      if(oldlen==newlen){break}
    }
  }else{
    clipx=x
    if(extra){
      cliplogic=TRUE
    }
  }
  if(extra & length(clipx)>0){
    cliplogic[xord]=cliplogic
    range=range(clipx, na.rm = TRUE)
  }else{
    cliplogic=NA
    range=NA
  }
  invisible(list(x=clipx, clip=cliplogic, range=range, clipiters=i))
}