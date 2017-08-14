magmap<-
function(data, locut=0, hicut=1, flip=FALSE, range=c(0,2/3), type='quan', stretch='lin', stretchscale=1, bad=NA, clip=''){
if(stretchscale=='auto'){
  good=is.na(data)==FALSE & is.nan(data)==FALSE & is.infinite(data)==FALSE & is.null(data)==FALSE
	if(length(which(good))==0){stop('There is no numeric data!')}
  data=data-median(data[good],na.rm=TRUE)
  absdata=abs(data[good])
  stretchscale=1/median(absdata[absdata>0],na.rm=TRUE)
  if(!is.finite(stretchscale)){stretchscale=1}
}
  
if(stretch=='log' | stretch=='sqrt'){
	good=is.na(data)==FALSE & is.nan(data)==FALSE & is.infinite(data)==FALSE & is.null(data)==FALSE & data>0
	if(length(which(good))==0){stop('There is no numeric data with a value greater than 0!')}
}else{
	good=is.na(data)==FALSE & is.nan(data)==FALSE & is.infinite(data)==FALSE & is.null(data)==FALSE
	if(length(which(good))==0){stop('There is no numeric data!')}
}
  
if(type=='quan'){
	locut=quantile(data[good],locut)
	hicut=quantile(data[good],hicut)
}else if(type=='num'){
	locut=locut
	hicut=hicut
}else if(type=='sig'){
	locut=quantile(data[good],pnorm(locut))
	hicut=quantile(data[good],pnorm(hicut))
}else if(type=='rank'){
	locut=1
	hicut=length(data[good])
	data[good][order(data[good])]=locut:hicut
}else{
  stop(type,'is not a valid type option!')
}
loreturn=locut
hireturn=hicut
if(stretch=='log' & locut<=0){stop('locut <= 0 and stretch=\'log\'- this is not allowed!')}
if(stretch=='log' & hicut<=0){stop('hicut <=0 and stretch=\'log\'- this is not allowed!')}
if(locut>hicut){stop('locut>hicut is not allowed')}
if(locut==hicut){data[good][1:length(data)]=(range[2]+range[1])/2}
if(locut<hicut){
  if(stretch=='lin'){
  #Nothing to see here...
  }else if(stretch=='log'){
	  locut=log10(locut)
	  hicut=log10(hicut)
    data=suppressWarnings(log10(data))
	}else if(stretch=='atan'){
    locut=atan(locut*stretchscale)
    hicut=atan(hicut*stretchscale)
    data=atan(data*stretchscale)
  }else if(stretch=='asinh'){
	  locut=asinh(locut*stretchscale)
	  hicut=asinh(hicut*stretchscale)
    data=asinh(data*stretchscale)
	}else if(stretch=='sqrt'){
	  locut=sqrt(locut)
	  hicut=sqrt(hicut)
    data=suppressWarnings(sqrt(data))
	}else if(stretch=='cdf'){
    cdf=ecdf(data[good])
    locut=cdf(locut)
    hicut=cdf(hicut)
    data[good]=cdf(data[good])
  }else{
	  stop(paste(stretch,'is not a valid stretch option!'))
	}
  losel=data<locut & good; hisel=data>hicut & good
	data[losel]=locut; data[hisel]=hicut
	data[good]=data[good]-locut
	data[good]=range[1]+(data[good]*(range[2]-range[1])/(hicut-locut))
	if(flip){data[good]=range[2]-data[good]+range[1]}
  if(clip=='NA'){data[losel]=NA;data[hisel]=NA}
}
data[! good]=bad
return(list(map=data,datalim=c(loreturn,hireturn),maplim=range,loclip=length(which(data[good]==range[1]))/length(data[good]),hiclip=length(which(data[good]==range[2]))/length(data[good])))
}

