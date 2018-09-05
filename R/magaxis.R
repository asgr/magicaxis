magaxis <-
function(side=1:2, majorn=5, minorn='auto', tcl=0.5, ratio=0.5, labels=TRUE, unlog='auto', mgp=c(2,0.5,0), mtline=2, xlab=NULL, ylab=NULL, crunch=TRUE, logpretty=TRUE, prettybase=10, powbase=10, hersh=FALSE, family='sans', frame.plot=FALSE, usepar=FALSE, grid=FALSE, grid.col='grey', grid.lty=1, grid.lwd=1, ...){
dots=list(...)
dotskeepaxis=c('cex.axis', 'col.axis', 'font.axis', 'xaxp', 'yaxp', 'tck', 'las', 'fg', 'xpd', 'xaxt', 'yaxt', 'col.ticks', 'tick')
dotskeepmtext=c('cex.lab', 'col.lab', 'font.lab')
if(length(dots)>0){
  dotsaxis=dots[names(dots) %in% dotskeepaxis]
  dotsmtext=dots[names(dots) %in% dotskeepmtext]
}else{
  dotsaxis={}
  dotsmtext={}
}
if(length(mtline)==1){mtline=rep(mtline,2)}
majornlist=majorn
minornlist=minorn
labelslist=labels
unloglist=unlog
crunchlist=crunch
logprettylist=logpretty
prettybaselist=prettybase
powbaselist=powbase
gridlist=grid
if(length(majorn)==1 & length(side)>1){majornlist=rep(majorn,length(side))}
if(length(minorn)==1 & length(side)>1){minornlist=rep(minorn,length(side))}
if(length(labels)==1 & length(side)>1){labelslist=rep(labels,length(side))}
if(length(unlog)==1 & length(side)>1 & (unlog[1]==T | unlog[1]==F | unlog[1]=='auto')){unloglist=rep(unlog,length(side))}
if(length(crunch)==1 & length(side)>1){crunchlist=rep(crunch,length(side))}
if(length(logpretty)==1 & length(side)>1){logprettylist=rep(logpretty,length(side))}
if(length(prettybase)==1 & length(side)>1){prettybaselist=rep(prettybase,length(side))}
if(length(powbase)==1 & length(side)>1){powbaselist=rep(powbase,length(side))}
if(length(grid)==1 & length(side)>1){gridlist=rep(grid,length(side))}
if(unlog[1]==''){unloglist=rep(FALSE,length(side))}
if(unlog[1]=='x'){unloglist=rep(FALSE,length(side));unloglist[side %in% c(1,3)]=TRUE}
if(unlog[1]=='y'){unloglist=rep(FALSE,length(side));unloglist[side %in% c(2,4)]=TRUE}
if(unlog[1]=='xy' | unlog[1]=='yx'){unloglist=rep(TRUE,length(side))}

if(length(majornlist) != length(side)){stop('Length of majorn vector mismatches number of axes!')}
if(length(minornlist) != length(side)){stop('Length of minorn vector mismatches number of axes!')}
if(length(labelslist) != length(side)){stop('Length of labels vector mismatches number of axes!')}
if(length(unloglist) != length(side)){stop('Length of unlog vector mismatches number of axes!')}
if(length(crunchlist) != length(side)){stop('Length of crunch vector mismatches number of axes!')}
if(length(logprettylist) != length(side)){stop('Length of logpretty vector mismatches number of axes!')}
if(length(prettybaselist) != length(side)){stop('Length of prettybase vector mismatches number of axes!')}
if(length(powbaselist) != length(side)){stop('Length of powbase vector mismatches number of axes!')}
if(length(gridlist) != length(side)){stop('Length of grid vector mismatches number of axes!')}

currentfamily=par('family')
if(hersh & family=='serif'){par(family='HersheySerif')}
if(hersh & family=='sans'){par(family='HersheySans')}
if(hersh==F & family=='serif'){par(family='serif')}
if(hersh==F & family=='sans'){par(family='sans')}

lwd=par()$lwd
if(usepar){
  tcl=par()$tcl
  mgp=par()$mgp
}

for(i in 1:length(side)){
		currentside=side[i]
    majorn=majornlist[i]
    minorn=minornlist[i]
    labels=labelslist[i]
		unlog=unloglist[i]
		crunch=crunchlist[i]
		logpretty=logprettylist[i]
    prettybase=prettybaselist[i]
    powbase=powbaselist[i]
    grid=gridlist[i]
    lims=par("usr")
    if(currentside %in% c(1,3)){
      lims=lims[1:2];if(par('xlog')){logged=T}else{logged=F}
    }else{
      lims=lims[3:4];if(par('ylog')){logged=T}else{logged=F}
    }
    lims=sort(lims)
    
    if(unlog=='auto'){if(logged){unlog=T}else{unlog=F}}
    if((logged | unlog) & powbase==10){usemultloc=(10^lims[2])/(10^lims[1])<50}else{usemultloc=F}
    
    if(unlog){
      sci.tick=maglab(10^lims,n=majorn,log=T,exptext=T,crunch=crunch,logpretty=logpretty,usemultloc=usemultloc,prettybase=prettybase, powbase=powbase, hersh=hersh)
      major.ticks = log(sci.tick$tickat,powbase)
      uselabels = sci.tick$exp
      labloc = log(sci.tick$labat,powbase)
      if(usemultloc==F){
        if(minorn=='auto'){
 		      splitmin=(powbase^major.ticks[2])/(powbase^major.ticks[1])
 		    }else{
 		       splitmin=minorn+1
 		    }
        if(splitmin>10){
          minors = seq(major.ticks[1], major.ticks[2])-major.ticks[1]
        }else{
          minors = log(seq(powbase^major.ticks[1],powbase^major.ticks[2],len=splitmin),powbase)-major.ticks[1]
        }
      }
 		}
 		if(logged & unlog==F){
 		  sci.tick=maglab(10^lims, n=majorn, log=T, exptext=F, crunch=crunch, logpretty=logpretty,usemultloc=usemultloc, prettybase=prettybase, powbase=powbase, hersh=hersh)
 		  major.ticks = log(sci.tick$tickat,powbase)
  		uselabels = sci.tick$exp
  		labloc = log(sci.tick$labat,powbase)
 		  if(usemultloc==F){
 		    if(minorn=='auto'){
 		      splitmin=(powbase^major.ticks[2])/(powbase^major.ticks[1])
 		    }else{
 		       splitmin=minorn+1
 		    }
 		    if(splitmin>10){
          minors = seq(major.ticks[1], major.ticks[2])-major.ticks[1]
        }else{
          minors = log(seq(powbase^major.ticks[1],powbase^major.ticks[2],len=splitmin),powbase)-major.ticks[1]
        }
 		  }
 		}

 		if(logged==F & unlog==F){
 		  sci.tick=maglab(lims,n=majorn,log=F,exptext=F,prettybase=prettybase, hersh=hersh)
 		  major.ticks = sci.tick$tickat
  		uselabels = sci.tick$exp
  		labloc = sci.tick$labat
  		if(minorn=='auto'){splitmin=length(pretty(major.ticks[1:2]))}else{splitmin=minorn+1}
  		minors = seq(major.ticks[1],major.ticks[2],len=splitmin)-major.ticks[1]
 		}
    
    if(grid){
      if(currentside==1){
        if(logged){
          abline(v=powbase^labloc, col=grid.col, lty=grid.lty, lwd=grid.lty)
        }else{
          abline(v=labloc, col=grid.col, lty=grid.lty, lwd=grid.lty)
        }
      }
      if(currentside==2){
        if(logged){
          abline(h=powbase^labloc, col=grid.col, lty=grid.lty, lwd=grid.lty)
        }else{
          abline(h=labloc, col=grid.col, lty=grid.lty, lwd=grid.lty)
        }
      }
    }

 		if(logged){
 		  do.call("axis", c(list(side=currentside,at=powbase^major.ticks,tcl=tcl,labels=FALSE,mgp=mgp,lwd=lwd,lwd.ticks=lwd),dotsaxis))
 		}else{
 		  do.call("axis", c(list(side=currentside,at=major.ticks,tcl=tcl,labels=FALSE,mgp=mgp,lwd=lwd,lwd.ticks=lwd),dotsaxis))
 		}
 		
    if(labels){
      if(logged){
        do.call("axis", c(list(side=currentside,at=powbase^labloc,tick=F,labels=uselabels,mgp=mgp,lwd=lwd,lwd.ticks=lwd),dotsaxis))
      }else{
        do.call("axis", c(list(side=currentside,at=labloc,tick=F,labels=uselabels,mgp=mgp,lwd=lwd,lwd.ticks=lwd),dotsaxis))
      }
    }
    
    if(usemultloc==F & minorn>1){
      minors = minors[-c(1,length(minors))]
      minor.ticks = c(outer(minors, major.ticks, `+`))
      if(logged){
        do.call("axis", c(list(side=currentside,at=powbase^minor.ticks,tcl=tcl*ratio,labels=FALSE,mgp=mgp,lwd=lwd,lwd.ticks=lwd),dotsaxis))
      }else{
        do.call("axis", c(list(side=currentside,at=minor.ticks,tcl=tcl*ratio,labels=FALSE,mgp=mgp,lwd=lwd,lwd.ticks=lwd),dotsaxis))
      }
    }
  }

  if(length(dotsmtext)>0){
    names(dotsmtext)=c('cex', 'col', 'font')[match(names(dotsmtext), dotskeepmtext)]
  }
  if(is.null(xlab)==FALSE){
    do.call("mtext", c(list(text=xlab, side=ifelse(side[1] %in% c(1,3), side[1], side[2]), line=mtline[1]), dotsmtext))
  }
  if(is.null(ylab)==FALSE){
    do.call("mtext", c(list(text=ylab, side=ifelse(side[2] %in% c(2,4), side[2], side[1]), line=mtline[2]), dotsmtext))
  }

if(frame.plot){box()}
par(family=currentfamily)
}
