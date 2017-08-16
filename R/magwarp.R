magwarp=function(image_in, header_out=NULL, header_in=NULL, dim_out, direction = "auto", boundary = "dirichlet", interpolation = "cubic", CRVAL1_in = 0, CRVAL2_in = 0, CRPIX1_in = 0, CRPIX2_in = 0, CD1_1_in = 1, CD1_2_in = 0, CD2_1_in = 0, CD2_2_in = 1, CRVAL1_out = 0, CRVAL2_out = 0, CRPIX1_out = 0, CRPIX2_out = 0, CD1_1_out = 1, CD1_2_out = 0, CD2_1_out = 0, CD2_2_out = 1, plot=FALSE, ...){
  
  if(!requireNamespace("imager", quietly = TRUE)){
    stop('The imager package is needed for this function to work. Please install it from CRAN.', call. = FALSE)
  }
  
  if(!missing(image_in)){
    if(any(names(image_in)=='imDat') & missing(header_in)){
      header_in=image_in$hdr
      image_in=image_in$imDat
    }else if(any(names(image_in)=='imDat') & !missing(header_in)){
      image_in=image_in$imDat
    }
    if(any(names(image_in)=='dat') & missing(header_in)){
      header_in=image_in$hdr[[1]]
      header_in=data.frame(key=header_in[,1],value=header_in[,2], stringsAsFactors = FALSE)
      image_in=image_in$dat[[1]]
    }else if(any(names(image_in)=='dat') & !missing(header_in)){
      image_in=image_in$dat[[1]]
    }
    if(any(names(image_in)=='image') & missing(header_in)){
      header_in=image_in$header_in
      image_in=image_in$image
    }else if(any(names(image_in)=='image') & !missing(header_in)){
      image_in=image_in$image
    }
  }
  
  if(is.null(header_out)==FALSE & missing(dim_out)){
    if(is.data.frame(header_out) | is.matrix(header_out)){
    locs=match(c('NAXIS1','NAXIS2'),header_out[,1])
    locs=locs[is.na(locs)==FALSE]
    headerWCS=data.frame(header_out[locs,1],as.numeric(header_out[locs,2]))
      if('NAXIS1' %in% headerWCS[,1]){NAXIS1=headerWCS[headerWCS[,1]=='NAXIS1',2]}else{message('Missing NAXIS1')}
      if('NAXIS2' %in% headerWCS[,1]){NAXIS2=headerWCS[headerWCS[,1]=='NAXIS2',2]}else{message('Missing NAXIS2')}
    }else{
      if('NAXIS1' %in% header_out){NAXIS1=as.numeric(header_out[which(header_out=='NAXIS1')+1])}else{message('Missing NAXIS1')}
      if('NAXIS2' %in% header_out){NAXIS2=as.numeric(header_out[which(header_out=='NAXIS2')+1])}else{message('Missing NAXIS2')}
    }
    dim_out=c(NAXIS1, NAXIS2)
  }
  
  .warpfunc_in2out=function(x, y){
  radectemp=xy2radec(x, y, header=header_in, CRVAL1 = CRVAL1_in, CRVAL2 = CRVAL2_in, CRPIX1 = CRPIX1_in, CRPIX2 = CRPIX2_in, CD1_1 = CD1_1_in, CD1_2 = CD1_2_in, CD2_1 = CD2_1_in, CD2_2 = CD2_2_in)
  xy_out=radec2xy(radectemp, header=header_out, CRVAL1 = CRVAL1_out, CRVAL2 = CRVAL2_out, CRPIX1 = CRPIX1_out, CRPIX2 = CRPIX2_out, CD1_1 = CD1_1_out, CD1_2 = CD1_2_out, CD2_1 = CD2_1_out, CD2_2 = CD2_2_out)
  return=list(x=xy_out[,1], y=xy_out[,2])
}

.warpfunc_out2in=function(x, y){
  radectemp=xy2radec(x, y, header=header_out, CRVAL1 = CRVAL1_out, CRVAL2 = CRVAL2_out, CRPIX1 = CRPIX1_out, CRPIX2 = CRPIX2_out, CD1_1 = CD1_1_out, CD1_2 = CD1_2_out, CD2_1 = CD2_1_out, CD2_2 = CD2_2_out)
  xy_out=radec2xy(radectemp, header=header_in, CRVAL1 = CRVAL1_in, CRVAL2 = CRVAL2_in, CRPIX1 = CRPIX1_in, CRPIX2 = CRPIX2_in, CD1_1 = CD1_1_in, CD1_2 = CD1_2_in, CD2_1 = CD2_1_in, CD2_2 = CD2_2_in)
  return=list(x=xy_out[,1], y=xy_out[,2])
}

  image_out=matrix(0, max(dim(image_in)[1],dim_out[1]), max(dim(image_in)[2],dim_out[2]))
  image_out[1:dim(image_in)[1],1:dim(image_in)[2]]=image_in
  
  pixscale_in=getpixscale(header_in, CD1_1 = CD1_1_in, CD1_2 = CD1_2_in, CD2_1 = CD2_1_in, CD2_2 = CD2_2_in)
  pixscale_out=getpixscale(header_out, CD1_1 = CD1_1_out, CD1_2 = CD1_2_out, CD2_1 = CD2_1_out, CD2_2 = CD2_2_out)
  scale=pixscale_out^2/pixscale_in^2
  
  if(direction=='auto'){
    if(pixscale_in<pixscale_out){direction='forward'}
    if(pixscale_in>=pixscale_out){direction='backward'}
  }
  
  if(direction=='forward'){
    out=imager::imwarp(im=imager::as.cimg(image_out), map=.warpfunc_in2out, direction = direction, coordinates = "absolute",
  boundary = boundary, interpolation = interpolation)
  }
  
  if(direction=='backward'){
    out=imager::imwarp(im=imager::as.cimg(image_out), map=.warpfunc_out2in, direction = direction, coordinates = "absolute",
  boundary = boundary, interpolation = interpolation)
  }
  
  output=list(image=as.matrix(out)[1:dim_out[1],1:dim_out[2]]*scale, header=header_out)
  
  if(plot){
    magimageWCS(output, ...)
  }
  
  return=output
}