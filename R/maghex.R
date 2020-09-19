.hexgrid = function(xlim=c(0,100), ylim=c(0,100), step=1){
  xvec1 = seq(xlim[1], xlim[2], by=step)
  xvec2 = seq(xlim[1]+step/2, xlim[2]-step/2, by=step)
  
  yvec1 = seq(ylim[1], ylim[2], by=step*sqrt(3))
  yvec2 = seq(ylim[1]+step*sqrt(3)/2, ylim[2]-step*sqrt(3)/2, by=step*sqrt(3))
  
  grid1 = expand.grid(x=xvec1, y=yvec1)
  grid2 = expand.grid(x=xvec2, y=yvec2)
  return(rbind(grid1, grid2))
}

.squaregrid = function(xlim=c(0,100), ylim=c(0,100), step=1){
  xvec = seq(xlim[1], xlim[2], by=step)
  yvec = seq(ylim[1], ylim[2], by=step)
  grid = expand.grid(x=xvec, y=yvec)
  return(grid)
}

.magbincount = function(x, y, z=NULL, xlim=NULL, ylim=NULL, step=diff(xlim)/50, clustering=10, dustlim=NA, shape='hex', exactcount=TRUE, funstat=median){
  if(missing(y)){
    if(!is.null(dim(x))){
      if(dim(x)[2]>=2){y=x[,2];x=x[,1]}
    }
  }
  use = which(!is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x) & !is.na(y) & !is.nan(y) & !is.null(y) & is.finite(y))
  if(is.null(xlim)){xlim=range(x[use],na.rm=TRUE)}
  if(is.null(ylim)){ylim=range(y[use],na.rm=TRUE)}
  if(shape=='hex'){grid = .hexgrid(xlim, ylim, step)}
  if(shape=='square'){grid = .squaregrid(xlim, ylim, step)}
  if(exactcount){
    if(shape=='hex'){searchrad = 1.154701*step/2}
    if(shape=='square'){searchrad = 1.414214*step/2}
  }else{
    if(shape=='hex'){searchrad = 1.050075*step/2} #slightly smaller radius so on average we do not overcount statistically
    if(shape=='square'){searchrad = 1.27324*step/2}
  }
  ktry = ceiling(clustering * length(x) / dim(grid)[1])
  output = nn2(cbind(x[use],y[use]), grid, searchtype='radius', radius=searchrad, k=ktry)
  while(sum(output$nn.idx[,dim(output$nn.idx)[2]]) > 0){
    ktry = ktry * 2
    output = nn2(cbind(x[use],y[use]), grid, searchtype='radius', radius=searchrad, k=ktry)
  }
  output$nn.dists[output$nn.dists>1.34e154]=NA
  output$nn.idx[output$nn.idx==0] = NA
  if(exactcount){
    if(shape=='hex'){
      overlap = output$nn.idx[output$nn.dists>0.845299*step/2 & output$nn.idx>0]
    }
    if(shape=='square'){
      overlap = output$nn.idx[output$nn.dists>0.72676*step/2 & output$nn.idx>0]
    }
    findclash = duplicated(overlap, incomparables=0)
    resolve = unique(overlap[findclash])
    allclashIDs = which(output$nn.idx %in% resolve)
    for(i in resolve){
      sel = which(output$nn.idx[allclashIDs]==i)
      minsel = which.min(output$nn.dists[allclashIDs][sel])
      output$nn.idx[allclashIDs][sel[-minsel]] = NA
    }
  }
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
  
  output = list(bins=data.frame(grid, count=bincount, zstat=binzstat), dust=dust,
                xlim=xlim, ylim=ylim, step=step, dustlim=dustlim, groups=groups, shape=shape)
  class(output) = 'magbin'
  return(output)
}

plot.magbin = function(x, colramp=terrain.colors(1e4), colstretch='log', sizestretch='log', colref='count', sizeref='none', ...){
  dots=list(...)
  dotskeepmap=c("locut", "hicut", "flip", "type", "stretchscale", "clip" )
  if(length(dots)>0){
    dotsmap = dots[names(dots) %in% dotskeepmap]
    dots = dots[!names(dots) %in% dotskeepmap]
  }else{
    dotsmap={}
  }
  if(!is.na(x$dustlim)){
    x$bins = x$bins[x$bins[,'count']>x$dustlim,]
  }
  colmap = do.call("magmap", c(list(data=x$bins[,colref], stretch=colstretch, range=c(1,length(colramp)), bad=NA), dotsmap))
  if(sizeref=='none'){
    sizemap = rep(1,dim(x$bins)[1])
  }else{
    sizemap = do.call("magmap", c(list(data=x$bins[,sizeref], stretch=sizestretch, range=c(0,1), bad=NA), dotsmap))$map
  }
  #colmap = magmap(x$bins[,3], stretch=stretch, bad=NA, range=c(1,length(colramp)))
  do.call("magplot", c(list(NA, NA, xlim=x$xlim, ylim=x$ylim), dots))
  #magplot(NA, NA, xlim=x$xlim, ylim=x$ylim, ...)
  for(i in 1:dim(x$bins)[1]){
    if(!is.na(colmap$map[i])){
      if(is.na(x$dustlim)){
        if(x$shape=='hex'){
          .drawhex(x$bins[i,'x'], x$bins[i,'y'], unitcell=x$step*sizemap[i], col=colramp[colmap$map[i]], border=NA)
        }
        if(x$shape=='square'){
          .drawsquare(x$bins[i,'x'], x$bins[i,'y'], unitcell=x$step*sizemap[i], col=colramp[colmap$map[i]], border=NA)
        }
      }else if(x$bins[i,'count'] > x$dustlim){
        if(x$shape=='hex'){
          .drawhex(x$bins[i,'x'], x$bins[i,'y'], unitcell=x$step*sizemap[i], col=colramp[colmap$map[i]], border=NA)
        }
        if(x$shape=='square'){
          .drawsquare(x$bins[i,'x'], x$bins[i,'y'], unitcell=x$step*sizemap[i], col=colramp[colmap$map[i]], border=NA)
        }
      }
    }
  }
  if(!is.null(x$dust)){
    if(is.null(x$dust$z)){
      points(x$dust$x, x$dust$y, pch='.', col=colramp[1])
    }else{
      if(colref=='zstat'){
        colmapdust = do.call("magmap", c(list(data=x$dust$z, locut=colmap$datalim[1],hicut=colmap$datalim[2], type='num', stretch=colstretch, range=c(1,length(colramp)), bad=NA), dotsmap))$map
        points(x$dust$x, x$dust$y, pch='.', col=colramp[colmapdust])
      }else{
        points(x$dust$x, x$dust$y, pch='.', col=colramp[1])
      }
    }
  }
  magbar('topleft', range=colmap$datalim, log=colstretch=='log', col=colramp)
}

maghex = function(x, y, z=NULL, xlim=NULL, ylim=NULL, step=diff(xlim)/50, clustering=10,
                  dustlim=0.1, shape='hex', exactcount=FALSE, plot=TRUE, colramp=terrain.colors(1e4), colstretch='log', sizestretch='log', colref='count', sizeref='none', funstat=median, ...){
  
  if(missing(y)){
    if(!is.null(dim(x))){
      if(dim(x)[2]>=2){y=unlist(x[,2]); x=unlist(x[,1])}
    }
  }
  use=!is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x) & !is.na(y) & !is.nan(y) & !is.null(y) & is.finite(y)
  if(is.null(xlim)){xlim=range(x[use],na.rm=TRUE)}
  if(is.null(ylim)){ylim=range(y[use],na.rm=TRUE)}
  bincount = .magbincount(x=x, y=y, z=z, xlim=xlim, ylim=ylim, step=step,
                    clustering=clustering, dustlim=dustlim, shape=shape,
                    exactcount=exactcount, funstat=funstat)
  if(plot){
    plot(bincount, colramp=colramp, colstretch=colstretch, sizestretch=sizestretch, colref=colref, sizeref=sizeref, ...)
  }
  return(invisible(bincount))
}

.drawhex = function (x, y, unitcell = 1, col = NA, border = "black"){
  polygon(
    x = c(0, 1, 1, 1, 0, -1, -1, -1, 0)*unitcell/2 + x,
    y = c(1.154701, 0.5773502, 0, -0.5773502, -1.154701, -0.5773502, 0, 0.5773502, 1.154701)*unitcell/2 + y,
    col = col,
    border = border
  )
}

.drawsquare = function (x, y, unitcell = 1, col = NA, border = "black"){
  polygon(
    x = c(1,1,-1,-1,1)*unitcell/2 + x,
    y = c(1,-1,-1,1,1)*unitcell/2 + y,
    col = col,
    border = border
  )
}

