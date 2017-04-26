magclip=function(x, sigma='auto', clipiters=5, sigmasel=1, estimate='both'){
  if(clipiters>0){
    newlen=length(x)
    clipx=x
    for(i in 1:clipiters){
      oldlen=newlen
      roughmed=median(clipx, na.rm=TRUE)
      if(sigma=='auto'){
        clipsigma=qnorm(1-2/newlen)
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
      cliplogic=x>(roughmed-vallims) & x<(roughmed+vallims)
      clipx=x[cliplogic]
      newlen=length(clipx)
      if(oldlen==newlen){break}
    }
  }
  return=list(x=clipx, clip=cliplogic, range=range(clipx))
}