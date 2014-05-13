magmap<-
function(data, lo=0, hi=1, flip=FALSE, range=c(0,2/3), type='quan', log=FALSE, bad=NA, clip=''){
if(log){
	good=is.na(data)==FALSE & is.nan(data)==FALSE & is.infinite(data)==FALSE & is.null(data)==FALSE & data>0
	}else{
	good=is.na(data)==FALSE & is.nan(data)==FALSE & is.infinite(data)==FALSE & is.null(data)==FALSE
	}
if(type=='quan'){
	lo=quantile(data[good],lo)
	hi=quantile(data[good],hi)
}
if(type=='num'){
	lo=lo
	hi=hi
}
if(type=='sig'){
	lo=quantile(data[good],pnorm(lo))
	hi=quantile(data[good],pnorm(hi))
}
if(type=='rank'){
	lo=1
	hi=length(data[good])
	data[order(data[good])]=lo:hi
}
if(log & lo==0){stop('lo value is 0 and scale is step to log- this is not allowed!')}
if(log & hi==0){stop('hi value is 0 and scale is step to log- this is not allowed!')}
if(lo>hi){stop('lo>hi is not allowed')}
if(lo==hi){data=rep((range[2]+range[1])/2,length(data))}
if(lo<hi){
	if(log){data=suppressWarnings(log10(data));lo=log10(lo);hi=log10(hi)}
  losel=data<lo; hisel=data>hi
	data[losel]=lo; data[hisel]=hi
	data=data-lo
	data=range[1]+(data*(range[2]-range[1])/(hi-lo))
	if(flip){data=range[2]-data+range[1]}
  if(clip=='NA'){data[losel]=NA;data[hisel]=NA}
	if(log){lo=10^lo;hi=10^hi}
}
data[! good]=bad
return(list(map=data,datalim=c(lo,hi),maplim=range,loclip=length(which(data[good]==range[1]))/length(data[good]),hiclip=length(which(data[good]==range[2]))/length(data[good])))
}

