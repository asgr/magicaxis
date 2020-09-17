hexgrid = function(xlim=c(0,100), ylim=c(0,100), step=1){
  xvec1 = seq(xlim[1], xlim[2], by=step)
  xvec2 = seq(xlim[1]+step/2, xlim[2]-step/2, by=step)
  
  yvec1 = seq(ylim[1], ylim[2], by=step*sqrt(3))
  yvec2 = seq(ylim[1]+step*sqrt(3)/2, xlim[2]-step*sqrt(3)/2, by=step*sqrt(3))
  
  grid1 = expand.grid(x=xvec1, y=yvec1)
  grid2 = expand.grid(x=xvec2, y=yvec2)
  return(rbind(grid1, grid2))
}

hexcount = function(x, y, xlim=NULL, ylim=NULL, step=diff(xlim)/50, k=1e3, dustlim=1, exacthex=TRUE){
  if(missing(y)){
    if(!is.null(dim(x))){
      if(dim(x)[2]>=2){y=x[,2];x=x[,1]}
    }
  }
  use=!is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x) & !is.na(y) & !is.nan(y) & !is.null(y) & is.finite(y)
  if(is.null(xlim)){xlim=range(x[use],na.rm=TRUE)}
  if(is.null(ylim)){ylim=range(y[use],na.rm=TRUE)}
  grid = hexgrid(xlim, ylim, step)
  if(exacthex){
    output = nn2(cbind(x,y), grid, searchtype='radius', radius=1.154701*step/2, k=k)
  }else{
    output = nn2(cbind(x,y), grid, searchtype='radius', radius=1.050075*step/2, k=k) #slightly smaller radius so on average we do not overcount
  }
  output$nn.dists[output$nn.dists>1.34e154]=NA
  if(exacthex){
    overlap = output$nn.idx[output$nn.dists>0.845299*step/2 & output$nn.idx>0]
    findclash = duplicated(overlap, incomparables=0)
    resolve = unique(overlap[findclash])
    allclashIDs = which(output$nn.idx %in% resolve)
    for(i in resolve){
      #print(i)
      sel = which(output$nn.idx[allclashIDs]==i)
      minsel = which.min(output$nn.dists[allclashIDs][sel])
      output$nn.idx[allclashIDs][sel[-minsel]] = 0
      #output$nn.dists[sel[-minsel]] = NA
    }
  }
  hexcounts = k - rowSums(output$nn.idx==0)
  if(dustlim>0){
    dust = unique(output$nn.idx[hexcounts <= dustlim,])
    dust = dust[-which(dust==0)]
  }else{
    dust=NULL
  }
  output = list(hexbins=cbind(grid, hexcounts), dust=data.frame(x=x[dust], y=y[dust]), xlim=xlim, ylim=ylim, step=step, dustlim=dustlim)
  class(output) = 'hexcount'
  return(output)
}

plot.hexcount = function(x, colramp=terrain.colors(1e4), ...){
  x$hexbins = x$hexbins[x$hexbins[,3]>x$dustlim,]
  colmap = magmap(x$hexbins[,3], stretch='log', bad=NA, range=c(1,length(colramp)))$map
  magplot(NA, NA, xlim=x$xlim, ylim=x$ylim, asp=1, grid=T)
  for(i in 1:dim(x$hexbins)[1]){
    if(!is.na(colmap[i])){
      if(x$hexbins[i,3] > x$dustlim){
        drawhex(x$hexbins[i,1], x$hexbins[i,2], unitcell=x$step, col=colramp[colmap[i]], border=NA)
      }
    }
  }
  if(!is.null(x$dust)){
    points(x$dust$x, x$dust$y, pch='.')
  }
}

maghex = function(x, y, xlim=NULL, ylim=NULL, step=diff(xlim)/50, k=1e3, dustlim=1, exacthex=FALSE, colramp=terrain.colors(1e4)){
  if(missing(y)){
    if(!is.null(dim(x))){
      if(dim(x)[2]>=2){y=x[,2];x=x[,1]}
    }
  }
  use=!is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x) & !is.na(y) & !is.nan(y) & !is.null(y) & is.finite(y)
  if(is.null(xlim)){xlim=range(x[use],na.rm=TRUE)}
  if(is.null(ylim)){ylim=range(y[use],na.rm=TRUE)}
  hexout = hexcount(x=x, y=y, xlim=xlim, ylim=ylim, step=step, k=k, dustlim=dustlim, exacthex=exacthex)
  plot(hexout, colramp=colramp)
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
