.hexgrid = function(xlim=c(0,100), ylim=c(0,100), step=c(1,1), direction='h'){
  if(direction=='h'){
    xvec1 = seq(xlim[1], xlim[2]+step[1], by=step[1])
    xvec2 = seq(xlim[1]-step[1]/2, xlim[2]+step[1]*3/2, by=step[1])
    
    yvec1 = seq(ylim[1], ylim[2]+step[2], by=step[2]*sqrt(3))
    yvec2 = seq(ylim[1]-step[2]*0.8660254, ylim[2]+step[2]*sqrt(3)*3/2, by=step[2]*sqrt(3))
  }
  if(direction=='v'){
    xvec1 = seq(xlim[1], xlim[2]+step[1], by=step[1]*sqrt(3))
    xvec2 = seq(xlim[1]-step[1]*0.8660254, xlim[2]+step[1]*sqrt(3)*3/2, by=step[1]*sqrt(3))
    
    yvec1 = seq(ylim[1], ylim[2]+step[2], by=step[2])
    yvec2 = seq(ylim[1]-step[2]/2, ylim[2]+step[2]*3/2, by=step[2])
  }
  grid1 = expand.grid(x=xvec1, y=yvec1)
  grid2 = expand.grid(x=xvec2, y=yvec2)
  return(rbind(grid1, grid2))
}

.sqgrid = function(xlim=c(0,100), ylim=c(0,100), step=c(1,1), direction='h', offset=0, jitterseed=666){
  
  xvec = seq(xlim[1], xlim[2]+step[1], by=step[1])
  yvec = seq(ylim[1], ylim[2]+step[2], by=step[2])
  
  if(direction=='h'){
    if(offset[1]=='jitter'){
      set.seed(jitterseed)
      offset = runif(length(yvec))
      offset = rep(offset, each=length(xvec))
    }else{
      yoff = rep(c(0,offset), length(yvec)/2)
      if(length(yoff) < length(yvec)){yoff = c(yoff,0)}
      offset = rep(yoff, each=length(xvec))
    }
    grid = expand.grid(x=xvec, y=yvec)
    grid[,1] = grid[,1] + offset * step[1]
  }
  if(direction=='v'){
    if(offset[1]=='jitter'){
      set.seed(jitterseed)
      offset = runif(length(xvec))
      offset = rep(offset, length(yvec))
    }else{
      xoff = rep(c(0,offset), length(xvec)/2)
      if(length(xoff) < length(xvec)){xoff = c(xoff,0)}
      offset = rep(xoff, length(yvec))
    }
    grid = expand.grid(x=xvec, y=yvec)
    grid[,2] = grid[,2] + offset * step[2]
  }
  return(grid)
}

.trigrid = function(xlim=c(0,100), ylim=c(0,100), step=c(1,1), direction='h', offset=0){
  if(direction=='h'){
    xvec1 = seq(xlim[1], xlim[2]+step[1], by=step[1])
    xvec2 = seq(xlim[1]-step[1]/2, xlim[2]+step[1]/2, by=step[1])
    
    yvec1 = seq(ylim[1], ylim[2]+step[2]*0.8660254, by=step[2]*0.8660254)
    yvec2 = seq(ylim[1]+step[2]*0.2886751-step[2]*0.8660254, ylim[2]-step[2]*0.2886751+step[2]*0.8660254, by=step[2]*0.8660254)
  }
  if(direction=='v'){
    xvec1 = seq(xlim[1], xlim[2]+step[1]*0.8660254, by=step[1]*0.8660254)
    xvec2 = seq(xlim[1]+step[1]*0.2886751-step[1]*0.8660254, xlim[2]-step[1]*0.2886751+step[1]*0.8660254, by=step[1]*0.8660254)
    
    yvec1 = seq(ylim[1], ylim[2]+step[2], by=step[2])
    yvec2 = seq(ylim[1]-step[2]/2, ylim[2]+step[2]/2, by=step[2])
  }
  
  if(direction=='h'){
    yoff1 = rep(c(0,offset), length(yvec1)/2)
    if(length(yoff1) < length(yvec1)){yoff1 = c(yoff1,0)}
    offset1 = rep(yoff1, each=length(xvec1))
    
    yoff2 = rep(c(offset,0), length(yvec2)/2)
    if(length(yoff2) < length(yvec2)){yoff2 = c(yoff2,offset)}
    offset2 = rep(yoff2, each=length(xvec2))

    grid1 = expand.grid(x=xvec1, y=yvec1)
    grid2 = expand.grid(x=xvec2, y=yvec2)
    grid1[,1] = grid1[,1] + offset1 * step[1]
    grid2[,1] = grid2[,1] + offset2 * step[1]
  }
  if(direction=='v'){
    xoff1 = rep(c(0,offset), length(xvec1)/2)
    if(length(xoff1) < length(xvec1)){xoff1= c(xoff1,0)}
    offset1 = rep(xoff1, length(yvec1))
    
    xoff2 = rep(c(offset,0), length(xvec2)/2)
    if(length(xoff2) < length(xvec2)){xoff2= c(xoff2,offset)}
    offset2 = rep(xoff2, length(yvec2))
  
    grid1 = expand.grid(x=xvec1, y=yvec1)
    grid2 = expand.grid(x=xvec2, y=yvec2)
    grid1[,2] = grid1[,2] + offset1 * step[2]
    grid2[,2] = grid2[,2] + offset2 * step[2]
  }
  
  return(rbind(cbind(grid1, type=1), cbind(grid2,type=2)))
}

.trihexgrid = function(xlim=c(0,100), ylim=c(0,100), step=c(1,1), direction='h'){
  # if(direction=='h'){
  #   xvec1 = seq(xlim[1], xlim[2]+step[1], by=step[1])
  #   xvec2 = seq(xlim[1], xlim[2]+step[1]/2, by=step[1])
  #   
  #   yvec1 = seq(ylim[1], ylim[2]+step[2]*0.8660254, by=2*step[2]*0.8660254)
  #   yvec2 = seq(ylim[1]+step[2]*0.2886751-step[2]*0.8660254, ylim[2]-step[2]*0.2886751+step[2]*0.8660254, by=2*step[2]*0.8660254)
  #   
  #   grid1 = expand.grid(x=xvec1, y=yvec1)
  #   grid2 = expand.grid(x=xvec2, y=yvec2)
  #   grid3 = expand.grid(x=xvec1-step[1]/2, y=yvec1-step[2]*0.8660254)
  #   grid4 = expand.grid(x=xvec2-step[1]/2, y=yvec2-step[2]*0.8660254)
  # }
  # if(direction=='v'){
  #   xvec1 = seq(xlim[1], xlim[2]+step[1]*0.8660254, by=2*step[1]*0.8660254)
  #   xvec2 = seq(xlim[1]+step[1]*0.2886751-step[1]*0.8660254, xlim[2]-step[1]*0.2886751+step[1]*0.8660254, by=2*step[1]*0.8660254)
  #   
  #   yvec1 = seq(ylim[1], ylim[2]+step[2], by=step[2])
  #   yvec2 = seq(ylim[1], ylim[2]+step[2]/2, by=step[2])
  #   
  #   grid1 = expand.grid(x=xvec1, y=yvec1)
  #   grid2 = expand.grid(x=xvec2, y=yvec2)
  #   grid3 = expand.grid(x=xvec1-step[1]*0.8660254, y=yvec1-step[2]/2)
  #   grid4 = expand.grid(x=xvec2-step[1]*0.8660254, y=yvec2-step[2]/2)
  # }
  # return(rbind(cbind(grid1, type=1), cbind(grid2,type=2), cbind(grid3, type=1), cbind(grid4, type=2)))
  .trigrid(xlim=xlim, ylim=ylim, step=step, direction=direction, offset=0.5)
}

.drawhex = function (x, y, unitcell = c(1,1), col = NA, border = "black", direction='h'){
  if(direction=='h'){
    xvec = c(0, 1, 1, 1, 0, -1, -1, -1, 0)*unitcell[1]/2 + x
    yvec = c(1.154701, 0.5773502, 0, -0.5773502, -1.154701, -0.5773502, 0, 0.5773502, 1.154701)*unitcell[2]/2 + y
  }
  if(direction=='v'){
    xvec = c(1.154701, 0.5773502, 0, -0.5773502, -1.154701, -0.5773502, 0, 0.5773502, 1.154701)*unitcell[1]/2 + x
    yvec = c(0, 1, 1, 1, 0, -1, -1, -1, 0)*unitcell[2]/2 + y
  }
  polygon(
    x = xvec,
    y = yvec,
    col = col,
    border = border
  )
}

.drawsquare = function (x, y, unitcell = c(1,1), col = NA, border = "black"){
  polygon(
    x = c(1,1,-1,-1,1)*unitcell[1]/2 + x,
    y = c(1,-1,-1,1,1)*unitcell[2]/2 + y,
    col = col,
    border = border
  )
}

.drawtriangle = function (x, y, unitcell = c(1,1), col = NA, border = "black", type = 1, direction='h'){
  if(type==1){
    if(direction=='h'){
      xvec = c(0,1,-1,0)*unitcell[1]/2 + x
      yvec = c(1.154701,-0.5773503,-0.5773503,1.154701)*unitcell[2]/2 + y
    }
    if(direction=='v'){
      xvec = c(1.154701,-0.5773503,-0.5773503,1.154701)*unitcell[1]/2 + x
      yvec = c(0,1,-1,0)*unitcell[2]/2 + y
    }
    polygon(
      x = xvec,
      y = yvec,
      col = col,
      border = border
    )
  }
  if(type==2){
    if(direction=='h'){
      xvec = c(0,1,-1,0)*unitcell[1]/2 + x
      yvec = c(-1.154701,0.5773503,0.5773503,-1.154701)*unitcell[2]/2 + y
    }
    if(direction=='v'){
      xvec = c(-1.154701,0.5773503,0.5773503,-1.154701)*unitcell[1]/2 + x
      yvec = c(0,1,-1,0)*unitcell[2]/2 + y
    }
    polygon(
      x = xvec,
      y = yvec,
      col = col,
      border = border
    )
  }
}

.magbincount = function(x, y, z=NULL, xlim=NULL, ylim=NULL, zlim=NULL, Nbin=50, step=NULL,
                        clustering=10, dustlim=NA, shape='hex', funstat=function(x) median(x, na.rm=TRUE),
                        direction='h', offset=0, jitterseed=666){
  if(is.null(z)){
    if(!is.null(dim(x))){
      if(dim(x)[2]==3){z=unlist(x[,3])}
    }
  }
  if(missing(y)){
    if(!is.null(dim(x))){
      if(dim(x)[2]>=2){y=unlist(x[,2]); x=unlist(x[,1])}
    }
  }
  use = !is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x) & !is.na(y) & !is.nan(y) & !is.null(y) & is.finite(y)
  if(is.null(xlim)){xlim=range(x[use],na.rm=TRUE)}
  if(is.null(ylim)){ylim=range(y[use],na.rm=TRUE)}
  if(is.null(zlim) & !is.null(z)){zlim=range(z[use],na.rm=TRUE)}
  use = use & x >= xlim[1]
  use = use & x <= xlim[2]
  use = use & y >= ylim[1]
  use = use & y <= ylim[2]
  if(!is.null(z)){
    use = use & z >= zlim[1]
    use = use & z <= zlim[2]
  }
  use = which(use)
  if(is.null(step)){
    step = c(diff(xlim), diff(ylim))/Nbin
  }
  if(length(step)==1){
    step = rep(step,2)
  }
  if(shape=='hex' | shape=='hexagon'){grid = .hexgrid(xlim=xlim, ylim=ylim, step=step, direction=direction)}
  if(shape=='sq' | shape=='square'){grid = .sqgrid(xlim=xlim, ylim=ylim, step=step, direction=direction, offset=offset, jitterseed=jitterseed)}
  if(shape=='tri' | shape=='triangle'){grid = .trigrid(xlim=xlim, ylim=ylim, step=step, direction=direction, offset=offset)}
  if(shape=='trihex'){grid = .trihexgrid(xlim=xlim, ylim=ylim, step=step, direction=direction)}
  #if(exactcount){
  if(shape=='hex' | shape=='hexagon'){searchrad = 1.154701*step[1]/2}
  if(shape=='sq' | shape=='square'){searchrad = 1.414214*step[1]/2}
  if(shape=='tri' | shape=='triangle' | shape=='trihex'){searchrad = 1.154701*step[1]/2}
  # }else{
  #   if(shape=='hex' | shape=='hexagon'){searchrad = 1.050075*step[1]/2} #slightly smaller radius so on average we do not over-count statistically
  #   if(shape=='sq' | shape=='square'){searchrad = 1.128379*step[1]/2}
  #   if(shape=='tri' | shape=='triangle'){searchrad = 0.7425152*step[1]/2}
  # }
  ktry = ceiling(clustering * length(x) / dim(grid)[1])
  output = nn2(cbind(x[use],y[use]*step[1]/step[2]), cbind(grid$x,grid$y*step[1]/step[2]), searchtype='radius', radius=searchrad, k=ktry)
  while(sum(output$nn.idx[,dim(output$nn.idx)[2]]) > 0){
    ktry = ktry * 2
    output = nn2(cbind(x[use],y[use]*step[1]/step[2]), cbind(grid$x,grid$y*step[1]/step[2]), searchtype='radius', radius=searchrad, k=ktry)
  }
  output$nn.dists[output$nn.dists>1.34e154]=NA
  output$nn.idx[output$nn.idx==0] = NA
  
  distorder = order(output$nn.dists)
  tempambig = output$nn.idx[distorder]
  remdupe = duplicated(tempambig, incomparables=NA)
  output$nn.idx[distorder][remdupe] = NA
  output$output$nn.dists[distorder][remdupe] = NA
  
  # if(exactcount){
  #   if(shape=='hex' | shape=='hexagon'){
  #     overlap = output$nn.idx[output$nn.dists>0.845299*step[1]/2 & output$nn.idx>0]
  #   }
  #   if(shape=='sq' | shape=='square'){
  #     overlap = output$nn.idx[output$nn.dists>0.72676*step[1]/2 & output$nn.idx>0]
  #   }
  #   if(shape=='tri' | shape=='triangle'){
  #     overlap = output$nn.idx[output$nn.dists>0 & output$nn.idx>0]
  #   }
  #   findclash = duplicated(overlap, incomparables=0)
  #   resolve = unique(overlap[findclash])
  #   allclashIDs = which(output$nn.idx %in% resolve)
  #   for(i in resolve){
  #     sel = which(output$nn.idx[allclashIDs]==i)
  #     minsel = which.min(output$nn.dists[allclashIDs][sel])
  #     output$nn.idx[allclashIDs][sel[-minsel]] = NA
  #   }
  # }
  
  bincount = ktry - rowSums(is.na(output$nn.idx))
  
  if(!is.na(dustlim)){
    if(dustlim > 0 & dustlim < 1){
      dustlim = ceiling(quantile(bincount[bincount>0],dustlim))
    }
    if(dustlim > 0){
      dustsel = use[unique(output$nn.idx[bincount <= dustlim,])]
      dustsel = dustsel[-which(is.na(dustsel))]
      if(is.null(z)){
        dust = data.frame(x=x[dustsel], y=y[dustsel])
      }else{
        dust = data.frame(x=x[dustsel], y=y[dustsel], z=z[dustsel])
      }
    }else{
      dust = NULL
    }
  }else{
    dust = NULL
  }
  
  refs = rep.int(1:dim(output$nn.idx)[1],times=ktry)
  groups = rep(NA,length(x))
  replace = which(!is.na(output$nn.idx))
  groups[use][output$nn.idx[replace]] = refs[replace]
  
  binzstat = rep(NA, length(bincount))
  
  if(!is.null(z)){
    tempagg = aggregate(z, by=list(groups), FUN=funstat)
    binzstat[tempagg[,1]] = tempagg[,2]
  }
  
  output = list(bins=data.frame(grid, count=bincount, zstat=binzstat), dust=dust, groups=groups,
                xlim=xlim, ylim=ylim, step=step, dustlim=dustlim, shape=shape, direction=direction)
  class(output) = 'magbin'
  return(output)
}

plot.magbin = function(x, colramp=hcl.colors(21), colstretch='lin', sizestretch='lin',
                       colref='count', sizeref='none', add=FALSE, dobar=TRUE, title=colref,
                       colnorm=FALSE, projden=FALSE, projsig=FALSE, xdata=NULL, ydata=NULL, pch.dust='.', cex.dust=1, ...){
  dots=list(...)
  dotskeepmap=c("locut", "hicut", "flip", "type", "stretchscale", "clip" )
  dotskeepbar=c("position", "orient", "scale", "inset", "labN", "titleshift", "centrealign")
  
  if(length(dots)>0){
    dotsmap = dots[names(dots) %in% dotskeepmap]
    dots = dots[!names(dots) %in% dotskeepmap]
    dotsbar = dots[names(dots) %in% dotskeepbar]
    dots = dots[!names(dots) %in% dotskeepbar]
  }else{
    dotsmap={}
    dotsbar={}
  }
  if(!is.na(x$dustlim)){
    x$bins = x$bins[x$bins[,'count']>x$dustlim,]
  }
  colmap = do.call("magmap", c(list(data=x$bins[,colref], stretch=colstretch, range=c(1,length(colramp)), bad=NA), dotsmap))
  if(sizeref=='none'){
    sizemap = rep(1,dim(x$bins)[1])
  }else{
    sizemap = do.call("magmap", c(list(data=x$bins[,sizeref], stretch=sizestretch, range=c(0,1), bad=NA)))$map
  }
  #colmap = magmap(x$bins[,3], stretch=stretch, bad=NA, range=c(1,length(colramp)))
  if('xlim' %in% names(dots)){
    xlim = dots$xlim
    dots = dots[!names(dots) %in% 'xlim']
  }else{
    xlim=x$xlim
  }
  if('ylim' %in% names(dots)){
    ylim = dots$ylim
    dots = dots[!names(dots) %in% 'ylim']
  }else{
    ylim=x$ylim
  }
  
  if(add==FALSE){
    if(projden & !is.null(xdata) & !is.null(ydata)){
      layout(matrix(c(2,4,1,3),2), widths = c(0.9,0.1), heights = c(0.1,0.9))
      par(oma=c(3.1,3.1,0.6,0.6))
      #1 (topright)
      par(mar=c(0,0,0,0))
      plot.new()
      #2 (topleft)
      tempden = density(xdata, from=x$xlim[1], to=x$xlim[2], na.rm=TRUE)
      par(mar=c(0,0,0,0))
      magplot(tempden$x, tempden$y, xlim=x$xlim, ylim=c(0,max(tempden$y)*1.04), type='l', ylab='',
              majorn=c(5,2), side=FALSE)
      if(projsig){
        abline(v=quantile(xdata, pnorm(c(-1,1)), na.rm=TRUE), lty=3, col='darkgrey')
      }
      
      #3 (bottomleft)
      tempden = density(ydata, from=x$ylim[1], to=x$ylim[2], na.rm=TRUE)
      par(mar=c(0,0,0,0))
      magplot(tempden$y, tempden$x, xlim=c(0,max(tempden$y)*1.04), ylim=x$ylim, type='l', xlab='', 
              majorn=c(2,5), side=FALSE)
      if(projsig){
        abline(h=quantile(ydata, pnorm(c(-1,1)), na.rm=TRUE), lty=3, col='darkgrey')
      }
      
      #4 (bottomright)
      par(mar=c(0,0,0,0))
      do.call("magplot", c(list(NA, NA, xlim=xlim, ylim=ylim, side=c(1,2,3,4), labels=c(T,T,F,F)), dots))
    }else{
      do.call("magplot", c(list(NA, NA, xlim=xlim, ylim=ylim), dots))
    }
  }
  #magplot(NA, NA, xlim=x$xlim, ylim=x$ylim, ...)
  for(i in 1:dim(x$bins)[1]){
    if(!is.na(colmap$map[i])){
      if(is.na(x$dustlim)){
        if(x$shape=='hex' | x$shape=='hexagon'){
          .drawhex(x$bins[i,'x'], x$bins[i,'y'], unitcell=x$step*sizemap[i], col=colramp[colmap$map[i]], border=NA, direction=x$direction)
        }
        if(x$shape=='sq' | x$shape=='square'){
          .drawsquare(x$bins[i,'x'], x$bins[i,'y'], unitcell=x$step*sizemap[i], col=colramp[colmap$map[i]], border=NA)
        }
        if(x$shape=='tri' | x$shape=='triangle' | x$shape=='trihex'){
          .drawtriangle(x$bins[i,'x'], x$bins[i,'y'], unitcell=x$step*sizemap[i], col=colramp[colmap$map[i]], border=NA, type=x$bins[i,'type'], direction=x$direction)
        }
      }else if(x$bins[i,'count'] > x$dustlim){
        if(x$shape=='hex' | x$shape=='hexagon'){
          .drawhex(x$bins[i,'x'], x$bins[i,'y'], unitcell=x$step*sizemap[i], col=colramp[colmap$map[i]], border=NA, direction=x$direction)
        }
        if(x$shape=='sq' | x$shape=='square'){
          .drawsquare(x$bins[i,'x'], x$bins[i,'y'], unitcell=x$step*sizemap[i], col=colramp[colmap$map[i]], border=NA)
        }
        if(x$shape=='tri' | x$shape=='triangle' | x$shape=='trihex'){
          .drawtriangle(x$bins[i,'x'], x$bins[i,'y'], unitcell=x$step*sizemap[i], col=colramp[colmap$map[i]], border=NA, type=x$bins[i,'type'], direction=x$direction)
        }
      }
    }
  }
  if(!is.null(x$dust)){
    if(is.null(x$dust$z)){
      points(x$dust$x, x$dust$y, pch=pch.dust, col=colramp[1], cex=cex.dust)
    }else{
      if(colref=='zstat'){
        colmapdust = do.call("magmap", c(list(data=x$dust$z, locut=colmap$datalim[1],
                    hicut=colmap$datalim[2], type='num', stretch=colstretch,
                    range=c(1,length(colramp)), bad=NA)))$map
        points(x$dust$x, x$dust$y, pch=pch.dust, col=colramp[colmapdust], cex=cex.dust)
      }else{
        points(x$dust$x, x$dust$y, pch=pch.dust, col=colramp[1], cex=cex.dust)
      }
    }
  }
  
  if(dobar){
    if(colnorm){
      colmap$datalim = colmap$datalim/max(colmap$datalim, na.rm=TRUE)
      if(missing(title)){
        title = "norm"
      }
    }
    do.call("magbar", c(list(range=colmap$datalim, log=colstretch=='log', col=colramp, title=title), dotsbar))
  }
}

magbin = function(x, y, z=NULL, xlim=NULL, ylim=NULL, zlim=NULL, Nbin=50, step=NULL, log='', unlog=log, clustering=10,
                  dustlim=0.1, shape='hex', plot=TRUE, colramp=hcl.colors(21),
                  colstretch='lin', sizestretch='lin', colref='count', sizeref='none',
                  funstat=function(x) median(x, na.rm=TRUE), direction='h',
                  offset=0, jitterseed=666, projden=FALSE, projsig=FALSE, ...){
  if(is.null(z)){
    if(!is.null(dim(x))){
      if(dim(x)[2]==3){z=unlist(x[,3])}
    }
  }
  if(missing(y)){
    if(!is.null(dim(x))){
      if(dim(x)[2]>=2){y=unlist(x[,2]); x=unlist(x[,1])}
    }
  }
  
  logsplit = strsplit(log,'')[[1]]
  if ('x' %in% logsplit) {
    x = log10(x)
    if(length(xlim) == 2){
      xlim = log10(xlim)
    }
  }
  if ('y' %in% logsplit) {
    y = log10(y)
    if(length(ylim) == 2){
      ylim = log10(ylim)
    }
  }
  if ('z' %in% logsplit & !is.null(z)) {
    z = log10(z)
    if(length(zlim) == 2){
      zlim = log10(zlim)
    }
    if(missing(unlog)){
      unlog = paste(logsplit[-which(logsplit == 'z')], collapse='')
    }
    if(missing(colstretch)){
      colstretch = 'log'
    }
  }
  
  if (length(xlim) == 1) {
    sel = !is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)
    xlim=magclip(x[sel], sigma=xlim)$range
  }
  if (length(ylim) == 1) {
    sel = !is.na(y) & !is.nan(y) & !is.null(y) & is.finite(y)
    ylim=magclip(y[sel], sigma=ylim)$range
  }
  if (length(zlim) == 1) {
    sel = !is.na(z) & !is.nan(z) & !is.null(z) & is.finite(z)
    zlim=magclip(z[sel], sigma=zlim)$range
  }
  
  bincount = .magbincount(x=x, y=y, z=z, xlim=xlim, ylim=ylim, zlim=zlim, Nbin=Nbin, step=step,
                    clustering=clustering, dustlim=dustlim, shape=shape,
                    funstat=funstat, direction=direction, offset=offset, jitterseed=jitterseed)
  
  if(plot){
    plot(bincount, colramp=colramp, colstretch=colstretch, sizestretch=sizestretch,
         colref=colref, sizeref=sizeref, unlog=unlog, projden=projden, projsig=projsig, xdata=x,
         ydata=y, ...)
  }
  return(invisible(bincount))
}


