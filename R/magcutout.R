magcutout=function(image, loc = dim(image)/2, box = c(100, 100), shiftloc=FALSE, paddim=TRUE,
                   padval=NA, plot = FALSE, ...){
  
  if(length(image)==1){
    if(is.character(image) & 'fst' %in% .packages()){
      image = fst::fst(image)
    }else{
      stop('Target image must be fst file!')
    }
  }
  
  loc = as.numeric(loc)
  xcen = loc[1]
  ycen = loc[2]
  loc = ceiling(loc)
  box = ceiling(box)
  if(length(box)==1){box=rep(box,2)}
  xlo = ceiling(loc[1] - (box[1]/2 - 0.5))
  xhi = ceiling(loc[1] + (box[1]/2 - 0.5))
  ylo = ceiling(loc[2] - (box[2]/2 - 0.5))
  yhi = ceiling(loc[2] + (box[2]/2 - 0.5))
  
  loc.diff = c(x=xlo-1, y=ylo-1)
  
  expand = paddim && shiftloc
  diffxlo = xlo - 1
  if (diffxlo < 0) {
    xlo = 1
    if(expand) xhi = xlo + (box[1] - 1)
  }
  diffxhi = xhi - dim(image)[1]
  if (diffxhi > 0) {
    xhi = dim(image)[1]
    if(expand) {
	    xlo = xlo - diffxhi
	    if(xlo < 1) xlo = 1
    }
  }
  diffylo = ylo - 1
  if (diffylo < 0) {
    ylo = 1
    if(expand) yhi = ylo + (box[2] - 1)
  }
  diffyhi = yhi - dim(image)[2]
  if (diffyhi > 0) {
    yhi = dim(image)[2]
    if(expand) {
	    ylo = ylo - diffyhi
	    if(ylo < 1) ylo = 1
    }
  }
  if(!paddim && !shiftloc) {
  	if(diffxlo < 0 && (-diffxlo > diffxhi)) xhi = xhi - max(diffxhi,0) + diffxlo
  	if(diffxhi > 0 && (-diffxlo < diffxhi)) xlo = xlo + diffxhi - min(diffxlo,0)
  	if(diffylo < 0 && (-diffylo > diffyhi)) yhi = yhi - max(diffyhi,0) + diffylo
  	if(diffyhi > 0 && (-diffylo < diffyhi)) ylo = ylo + diffyhi - min(diffylo,0)
  }
  xsel = as.integer(xlo:xhi)
  ysel = as.integer(ylo:yhi)
  xsel=xsel[xsel>0]
  ysel=ysel[ysel>0]
  
  if(length(xsel)==0 | length(ysel)==0){
    image=matrix(padval,box[1],box[2])
  }else{
    image = as.matrix(image[xsel, ysel])
    if(paddim && !shiftloc && any(c(diffxlo,-diffxhi,diffylo,-diffyhi) < 0)) {
    	padded = matrix(padval,box[1],box[2])
    	padded[xsel-diffxlo,ysel-diffylo] = image
    	image = padded
    }
  }
  
  if(paddim & shiftloc==FALSE){
    loc=c(x=xcen-diffxlo,y=ycen-diffylo)
  }else{
    loc=c(x=xcen-xlo+1, y=ycen-ylo+1)
  }
  loc.orig=c(x=xcen, y=ycen)
  loc.diff=c(x=loc.orig[1]-loc[1], y=loc.orig[2]-loc[2])
  
  output = list(image = image, loc=loc, loc.orig=loc.orig, loc.diff=loc.diff, xsel=xsel, ysel=ysel)
  if (plot) {
    if(all(is.na(image))){
      image[]=0
      magimage(image, ...)
    }else{
      magimage(image, ...)
    }
  }
  invisible(output)
}

magcutoutWCS=function(image, header, loc, box = c(100, 100), shiftloc=FALSE, paddim=TRUE,
                      padval=NA, plot = FALSE, CRVAL1=0, CRVAL2=0, CRPIX1=0, CRPIX2=0,
                      CD1_1=1, CD1_2=0, CD2_1=0, CD2_2=1, coord.type='deg', sep=':',
                      loc.type=c('coord','coord'), approx.map=FALSE, ...){
  if(length(loc.type)==1){loc.type=rep(loc.type,2)}
  if(length(box)==1){box=rep(box,2)}
  if(!missing(image)){
    if(any(names(image)=='imDat') & missing(header)){
      imtype='FITSio'
      header=image$hdr
      image=image$imDat
    }
    if(any(names(image)=='dat') & missing(header)){
      imtype='astro'
      header=image$hdr[[1]]
      header=data.frame(key=header[,1],value=header[,2], stringsAsFactors = FALSE)
      image=image$dat[[1]]
    }
    if(any(names(image)=='image') & missing(header)){
      header=image$header
      image=image$image
      if(is.matrix(header) | is.data.frame(header)){imtype='astro'}else{imtype='FITSio'}
    }
    if(!missing(header)){
      if(is.matrix(header) | is.data.frame(header)){imtype='astro'}else{imtype='FITSio'}
    }
  }
  #Note below tempxy is R xy units, not FITS:
  if(missing(loc)){
    loc=magWCSxy2radec(dim(image)[1]/2, dim(image)[2]/2, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)[1,]
    tempxy=cbind(dim(image)[1]/2, dim(image)[2]/2)
  }else{
    if(loc.type[1]=='coord'){
      if(coord.type=='sex'){loc[1]=hms2deg(loc[1],sep=sep); loc[2]=dms2deg(loc[2],sep=sep)}
      loc=as.numeric(loc)
      tempxy=magWCSradec2xy(loc[1], loc[2], header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
    }else if(loc.type[1]=='image'){
      tempxy=rbind(loc)
      loc=magWCSxy2radec(loc[1], loc[2], header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)[1,]
    }
  }
  xcen = tempxy[1,1]
  ycen = tempxy[1,2]
  if(loc.type[2]=='coord'){
    box=box/3600
    tempxy=magWCSradec2xy(loc[1]-box[1]/2/cos(loc[2]*pi/180), loc[2], header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
    xlo = xcen - sqrt((tempxy[1,1]-xcen)^2+(tempxy[1,2]-ycen)^2)
    tempxy=magWCSradec2xy(loc[1]+box[1]/2/cos(loc[2]*pi/180), loc[2], header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
    xhi = xcen + sqrt((tempxy[1,1]-xcen)^2+(tempxy[1,2]-ycen)^2)
    tempxy=magWCSradec2xy(loc[1], loc[2]-box[2]/2, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
    ylo = ycen - sqrt((tempxy[1,1]-xcen)^2+(tempxy[1,2]-ycen)^2)
    tempxy=magWCSradec2xy(loc[1], loc[2]+box[2]/2, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
    yhi = ycen + sqrt((tempxy[1,1]-xcen)^2+(tempxy[1,2]-ycen)^2)
    xtemp=sort(c(xlo,xhi))
    xlo=ceiling(xtemp[1])
    xhi=ceiling(xtemp[2])
    ytemp=sort(c(ylo,yhi))
    ylo=ceiling(ytemp[1])
    yhi=ceiling(ytemp[2])
    box=c(xhi-xlo+1,yhi-ylo+1)
  }else{
    # Do nothing!
  }
  cutout = magcutout(image, loc = c(xcen,ycen), box = box, shiftloc=shiftloc, paddim=paddim, padval=padval, plot = FALSE)
  cut_image = cutout$image
  xlo = cutout$loc.diff[1]+1
  xhi = xlo+dim(cut_image)[1]-1
  ylo = cutout$loc.diff[2]+1
  yhi = ylo+dim(cut_image)[2]-1
  xcen.new=xcen-xlo+1
  ycen.new=ycen-ylo+1
  
  pixscale=getpixscale(header=header, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  loc.diff = c(xlo - 1, ylo - 1)
  cut_xlo=1
  cut_xhi=dim(cut_image)[1]
  cut_ylo=1
  cut_yhi=dim(cut_image)[2]
  usr.WCS=rbind(
    magWCSxy2radec(xlo-1, ylo-1, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    magWCSxy2radec(xlo-1, yhi, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    magWCSxy2radec(xhi, ylo-1, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2),
    magWCSxy2radec(xhi, yhi, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2)
  )
  #Below we want to define the R image usr coordinate system, so if e.g. a matrix is 10x10 this would have elements 1:10 x 1:10 but a usr image range of 0->10 x 0->10, hence the minus 1s below. Even a single pixel has a finite image extent (0->1 x 0->1).
  usr.WCS=cbind(x.cut=c(cut_xlo-1, cut_xlo-1, cut_xhi, cut_xhi),
                y.cut=c(cut_ylo-1, cut_yhi, cut_ylo-1, cut_yhi),
                x.orig=c(xlo-1, xlo-1, xhi, xhi),
                y.orig=c(ylo-1, yhi, ylo-1, yhi),
                usr.WCS
                )
  if(approx.map){
    approx.map.RA=approxfun(seq(usr.WCS[1,'RA'],usr.WCS[4,'RA'],len=1e2),seq(usr.WCS[1,'x.cut'],usr.WCS[4,'x.cut'],len=1e2))
    approx.map.Dec=approxfun(seq(usr.WCS[1,'Dec'],usr.WCS[4,'Dec'],len=1e2),seq(usr.WCS[1,'y.cut'],usr.WCS[4,'y.cut'],len=1e2))
    approx.map=function(RA, Dec){
      if(length(dim(RA)) == 2){
        Dec = RA[, 2]
        RA = RA[, 1]
      }
      invisible(cbind(x=approx.map.RA(RA), y=approx.map.Dec(Dec)))
    }
  }else{
    approx.map=NULL
  }
  
  if(!missing(header)){
  	dimdiff = dim(cut_image)-dim(image)
  	hdradd = list(CRPIX1 = -loc.diff[1], CRPIX2 = -loc.diff[2],
			NAXIS1=dimdiff[1], NAXIS2=dimdiff[2])
    if(imtype=='FITSio'){
    	for(hdrname in names(hdradd)){
    		if(hdradd[[hdrname]] != 0){
	    		hdrrow = which(header==hdrname)+1
	    		header[hdrrow] = as.character(as.numeric(header[hdrrow]) + hdradd[[hdrname]])
    		}
    	}
    }else if(imtype=='astro'){
    	for(hdrname in names(hdradd)){
    		if(hdradd[[hdrname]] != 0){
	    		hdrrow = which(header[,"key"]==hdrname)
	    		header[hdrrow,"value"] = as.character(as.numeric(header[hdrrow,"value"]) + hdradd[[hdrname]])
    		}
    	}
    }else{
      header=NULL
    }
  }else{
    header=NULL
  }
  
  output = list(image = cut_image, loc = c(x=as.numeric(xcen.new), y=as.numeric(ycen.new)), loc.orig = c(x=as.numeric(xcen), y=as.numeric(ycen)), loc.diff = c(as.numeric(loc.diff[1]),as.numeric(loc.diff[2])), xsel = xlo:xhi, ysel = ylo:yhi, loc.WCS = loc, scale.WCS=pixscale, usr.WCS=usr.WCS, approx.map=approx.map, header=header)
  
  if (plot) {
    if(all(is.na(cut_image))){
      cut_image[]=0
      magimageWCS(image=cut_image, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, ...)
      cut_image[]=NA
    }else{
      magimageWCS(image=cut_image, header=header, CRVAL1=CRVAL1, CRVAL2=CRVAL2, CRPIX1=CRPIX1, CRPIX2=CRPIX2, CD1_1=CD1_1, CD1_2=CD1_2, CD2_1=CD2_1, CD2_2=CD2_2, ...)
    }
  }
  
  invisible(output)
}
