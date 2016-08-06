magmap<-
function(data, lo=0, hi=1, flip=FALSE, range=c(0,2/3), type='quan', stretch='lin', stretchscale=1, bad=NA, clip=''){
if(stretchscale=='auto'){
  good=is.na(data)==FALSE & is.nan(data)==FALSE & is.infinite(data)==FALSE & is.null(data)==FALSE
	if(length(which(good))==0){stop('There is no numeric data!')}
  data=data-quantile(data[good],lo,na.rm=TRUE)
  stretchscale=1/median(abs(data[good & data>0]),na.rm=TRUE)
}
  
if(stretch=='log' | stretch=='sqrt'){
	good=is.na(data)==FALSE & is.nan(data)==FALSE & is.infinite(data)==FALSE & is.null(data)==FALSE & data>0
	if(length(which(good))==0){stop('There is no numeric data with a value greater than 0!')}
}else{
	good=is.na(data)==FALSE & is.nan(data)==FALSE & is.infinite(data)==FALSE & is.null(data)==FALSE
	if(length(which(good))==0){stop('There is no numeric data!')}
}
  
if(type=='quan'){
	lo=quantile(data[good],lo)
	hi=quantile(data[good],hi)
}else if(type=='num'){
	lo=lo
	hi=hi
}else if(type=='sig'){
	lo=quantile(data[good],pnorm(lo))
	hi=quantile(data[good],pnorm(hi))
}else if(type=='rank'){
	lo=1
	hi=length(data[good])
	data[order(data[good])]=lo:hi
}else{
  stop(type,'is not a valid type option!')
}
loreturn=lo
hireturn=hi
if(stretch=='log' & lo<=0){stop('lo <= 0 and stretch=\'log\'- this is not allowed!')}
if(stretch=='log' & hi<=0){stop('hi <=0 and stretch=\'log\'- this is not allowed!')}
if(lo>hi){stop('lo>hi is not allowed')}
if(lo==hi){data[1:length(data)]=(range[2]+range[1])/2}
if(lo<hi){
  if(stretch=='lin'){
  #Nothing to see here...
  }else if(stretch=='log'){
	  lo=log10(lo)
	  hi=log10(hi)
    data=suppressWarnings(log10(data))
	}else if(stretch=='atan'){
    lo=atan(lo*stretchscale)
    hi=atan(hi*stretchscale)
    data=atan(data*stretchscale)
  }else if(stretch=='asinh'){
	  lo=asinh(lo*stretchscale)
	  hi=asinh(hi*stretchscale)
    data=asinh(data*stretchscale)
	}else if(stretch=='sqrt'){
	  lo=sqrt(lo)
	  hi=sqrt(hi)
    data=suppressWarnings(sqrt(data))
	}else{
	  stop(paste(stretch,'is not a valid stretch option!'))
	}
  losel=data<lo; hisel=data>hi
	data[losel]=lo; data[hisel]=hi
	data=data-lo
	data=range[1]+(data*(range[2]-range[1])/(hi-lo))
	if(flip){data=range[2]-data+range[1]}
  if(clip=='NA'){data[losel]=NA;data[hisel]=NA}
}
data[! good]=bad
return(list(map=data,datalim=c(loreturn,hireturn),maplim=range,loclip=length(which(data[good]==range[1]))/length(data[good]),hiclip=length(which(data[good]==range[2]))/length(data[good])))
}

