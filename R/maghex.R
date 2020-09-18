hexgrid = function(xlim=c(0,100), ylim=c(0,100), step=1){
  xvec1 = seq(xlim[1], xlim[2], by=step)
  xvec2 = seq(xlim[1]+step/2, xlim[2]-step/2, by=step)
  
  yvec1 = seq(ylim[1], ylim[2], by=step*sqrt(3))
  yvec2 = seq(ylim[1]+step*sqrt(3)/2, ylim[2]-step*sqrt(3)/2, by=step*sqrt(3))
  
  grid1 = expand.grid(x=xvec1, y=yvec1)
  grid2 = expand.grid(x=xvec2, y=yvec2)
  return(rbind(grid1, grid2))
}

hexcount = function(x, y, z=NULL, xlim=NULL, ylim=NULL, step=diff(xlim)/50, clustering=10, dustlim=NA, exacthex=TRUE, funstat=median){
  if(missing(y)){
    if(!is.null(dim(x))){
      if(dim(x)[2]>=2){y=x[,2];x=x[,1]}
    }
  }
  use = which(!is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x) & !is.na(y) & !is.nan(y) & !is.null(y) & is.finite(y))
  if(is.null(xlim)){xlim=range(x[use],na.rm=TRUE)}
  if(is.null(ylim)){ylim=range(y[use],na.rm=TRUE)}
  grid = hexgrid(xlim, ylim, step)
  if(exacthex){
    searchrad = 1.154701*step/2
  }else{
    searchrad = 1.050075*step/2 #slightly smaller radius so on average we do not overcount statistically
  }
  ktry = ceiling(clustering * length(x) / dim(grid)[1])
  output = nn2(cbind(x[use],y[use]), grid, searchtype='radius', radius=searchrad, k=ktry)
  while(sum(output$nn.idx[,dim(output$nn.idx)[2]]) > 0){
    ktry = ktry * 2
    output = nn2(cbind(x[use],y[use]), grid, searchtype='radius', radius=searchrad, k=ktry)
  }
  output$nn.dists[output$nn.dists>1.34e154]=NA
  output$nn.idx[output$nn.idx==0] = NA
  if(exacthex){
    overlap = output$nn.idx[output$nn.dists>0.845299*step/2 & output$nn.idx>0]
    findclash = duplicated(overlap, incomparables=0)
    resolve = unique(overlap[findclash])
    allclashIDs = which(output$nn.idx %in% resolve)
    for(i in resolve){
      sel = which(output$nn.idx[allclashIDs]==i)
      minsel = which.min(output$nn.dists[allclashIDs][sel])
      output$nn.idx[allclashIDs][sel[-minsel]] = NA
    }
  }
  hexcounts = ktry - rowSums(is.na(output$nn.idx))
  
  if(!is.na(dustlim)){
    if(dustlim > 0 & dustlim < 1 & is.null(z)){
      dustlim = ceiling(quantile(hexcounts[hexcounts>0],dustlim))
    }
    if(dustlim > 0 & is.null(z)){
      dustsel = use[unique(output$nn.idx[hexcounts <= dustlim,])]
      dustsel = dustsel[-which(is.na(dustsel))]
      if(is.null(z)){
        dust = data.frame(x=x[dustsel], y=y[dustsel])
      }else{
        dust = data.frame(x=x[dustsel], y=y[dustsel], z=z[distsel])
      }
    }else{
      dust = NULL
    }
  }else{
    dust = NULL
  }
  
  hexrefs = rep.int(1:dim(output$nn.idx)[1],times=ktry)
  groups = rep(NA,length(x))
  replace = which(!is.na(output$nn.idx))
  groups[use][output$nn.idx[replace]] = hexrefs[replace]
  
  if(!is.null(z)){
    tempagg = aggregate(z, by=list(groups), FUN=funstat)
    hexcounts[] = NA
    hexcounts[tempagg[,1]] = tempagg[,2]
  }
  
  output = list(hexbins=cbind(grid, hexcounts), dust=dust,
                xlim=xlim, ylim=ylim, step=step, dustlim=dustlim, groups=groups)
  class(output) = 'hexcount'
  return(output)
}

plot.hexcount = function(x, colramp=terrain.colors(1e4), stretch='log', ...){
  dots=list(...)
  dotskeepmap=c("locut", "hicut", "flip", "type", "stretchscale", "clip" )
  if(length(dots)>0){
    dotsmap = dots[names(dots) %in% dotskeepmap]
    dots = dots[!names(dots) %in% dotskeepmap]
  }else{
    dotsmap={}
  }
  if(!is.na(x$dustlim)){
    x$hexbins = x$hexbins[x$hexbins[,3]>x$dustlim,]
  }
  colmap = do.call("magmap", c(list(data=x$hexbins[,3], stretch=stretch, range=c(1,length(colramp)), bad=NA), dotsmap))
  #colmap = magmap(x$hexbins[,3], stretch=stretch, bad=NA, range=c(1,length(colramp)))
  do.call("magplot", c(list(NA, NA, xlim=x$xlim, ylim=x$ylim), dots))
  #magplot(NA, NA, xlim=x$xlim, ylim=x$ylim, ...)
  for(i in 1:dim(x$hexbins)[1]){
    if(!is.na(colmap$map[i])){
      if(is.na(x$dustlim)){
        drawhex(x$hexbins[i,1], x$hexbins[i,2], unitcell=x$step, col=colramp[colmap$map[i]], border=NA)
      }else if(x$hexbins[i,3] > x$dustlim){
        drawhex(x$hexbins[i,1], x$hexbins[i,2], unitcell=x$step, col=colramp[colmap$map[i]], border=NA)
      }
    }
  }
  if(!is.null(x$dust)){
    if(is.null(x$dust$z)){
      points(x$dust$x, x$dust$y, pch='.')
    }else{
      colmap = do.call("magmap", c(list(data=x$dust$z, stretch=stretch, range=c(1,length(colramp)), bad=NA), dotsmap))
      points(x$dust$x, x$dust$y, pch='.', col=colramp[colmap$map])
    }
  }
  magbar('topleft', range=colmap$datalim, log=stretch=='log', col=colramp)
}

maghex = function(x, y, z=NULL, xlim=NULL, ylim=NULL, step=diff(xlim)/50, clustering=10,
                  dustlim=0.1, exacthex=FALSE, colramp=terrain.colors(1e4), stretch='log', funstat=median, ...){
  
  if(missing(y)){
    if(!is.null(dim(x))){
      if(dim(x)[2]>=2){y=unlist(x[,2]); x=unlist(x[,1])}
    }
  }
  use=!is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x) & !is.na(y) & !is.nan(y) & !is.null(y) & is.finite(y)
  if(is.null(xlim)){xlim=range(x[use],na.rm=TRUE)}
  if(is.null(ylim)){ylim=range(y[use],na.rm=TRUE)}
  hexout = hexcount(x=x, y=y, z=z, xlim=xlim, ylim=ylim, step=step,
                    clustering=clustering, dustlim=dustlim, exacthex=exacthex,
                    funstat=funstat)
  plot(hexout, colramp=colramp, stretch=stretch, ...)
  return(invisible(hexout))
}

drawhex = function (x, y, unitcell = 1, col = NA, border = "black"){
  polygon(
    x = c(0, 1, 1, 1, 0, -1, -1, -1, 0)*unitcell/2 + x,
    y = c(1.154701, 0.5773502, 0, -0.5773502, -1.154701, -0.5773502, 0, 0.5773502, 1.154701)*unitcell/2 + y,
    col = col,
    border = border
  )
}

passdots=function(func, dots){
  funcargs = formalArgs(func)
  
}
