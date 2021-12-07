magplot =
  function(x,
           y,
           z = NULL,
           log = '',
           main = '',
           side = 1:2,
           majorn = 5,
           minorn = 'auto',
           tcl = 0.5,
           ratio = 0.5,
           labels = TRUE,
           unlog = "auto",
           mgp = c(2, 0.5, 0),
           mtline = 2,
           xlab = '',
           ylab = '',
           crunch = TRUE,
           logpretty = TRUE,
           prettybase = 10,
           powbase = 10,
           hersh = FALSE,
           family = "sans",
           frame.plot = TRUE,
           usepar = FALSE,
           grid = TRUE,
           grid.col = 'grey90',
           grid.lty = 1,
           grid.lwd = 1,
           xlim = NULL,
           ylim = NULL,
           lwd = 1,
           axis.lwd = 1,
           ticks.lwd = axis.lwd,
           axis.col = 'black',
           zcol = hcl.colors(21),
           zstretch = 'lin',
           dobar = TRUE,
           ...) {
    if (class(x)[1] == 'histogram') {
      dots = list(...)
      do.call('maghist', c(
        list(
          x = x,
          xlim = xlim,
          ylim = ylim,
          log = log,
          side = side,
          majorn = majorn,
          minorn = minorn,
          tcl = tcl,
          ratio = ratio,
          labels = labels,
          mgp = mgp,
          mtline = mtline,
          xlab = xlab,
          ylab = ylab,
          crunch = crunch,
          logpretty = logpretty,
          prettybase = prettybase,
          powbase = powbase,
          hersh = hersh,
          family = family,
          frame.plot = frame.plot,
          usepar = usepar,
          grid = grid,
          grid.col = grid.col,
          grid.lty = grid.lty,
          grid.lwd = grid.lwd
        ),
        dots
      ))
      return(invisible(NULL))
    } else{
      logsplit = strsplit(log[1], '')[[1]]
      
      if (missing(y)) {
        if (is.data.frame(x) | is.matrix(x)) {
          if (ncol(x) > 2) {
            x = as.data.frame(x)
            plot(x, ...)
            return(invisible(NULL))
          } else{
            y = x[, 2]
            x = x[, 1]
            #Carry on with our newly defined x and y
          }
        } else{
          plot(
            x = x,
            axes = FALSE,
            xlab = '',
            ylab = '',
            main = main,
            log = log,
            frame.plot = FALSE,
            panel.first = if (side[1] != FALSE) {
              magaxis(
                side = side,
                majorn = majorn,
                minorn = minorn,
                tcl = tcl,
                ratio = ratio,
                labels = labels,
                unlog = unlog,
                mgp = mgp,
                mtline = mtline,
                xlab = xlab,
                ylab = ylab,
                crunch = crunch,
                logpretty = logpretty,
                prettybase = prettybase,
                powbase = powbase,
                hersh = hersh,
                family = family,
                frame.plot = frame.plot,
                usepar = usepar,
                grid = grid,
                grid.col = grid.col,
                grid.lty = grid.lty,
                grid.lwd = grid.lwd,
                axis.lwd = axis.lwd,
                ticks.lwd = ticks.lwd,
                axis.col = axis.col,
                ...
              )
            },
            lwd = lwd,
            xlim = xlim,
            ylim = ylim,
            ...
          )
          return(invisible(NULL))
        }
      }
      
      # Lots of auto limit logic follows...
      
      xsel = !is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)
      if ('x' %in% logsplit) {
        xsel = xsel & x > 0
      }
      ysel = !is.na(y) & !is.nan(y) & !is.null(y) & is.finite(y)
      if ('y' %in% logsplit) {
        ysel = ysel & y > 0
      }
      
      sel = xsel & ysel
      
      if (length(which(sel)) > 1) {
        if (is.null(xlim)) {
          xlim = range(x[sel], na.rm = TRUE)
          man_xlim = FALSE
        } else{
          man_xlim = TRUE
        }
        if (length(xlim) == 1) {
          xlim = magclip(x[sel], sigma = xlim)$range
          man_xlim = FALSE
        }
        
        if (is.null(ylim)) {
          ylim = range(y[sel], na.rm = TRUE)
          man_ylim = FALSE
        } else{
          man_ylim = TRUE
        }
        if (length(ylim) == 1) {
          ylim = magclip(y[sel], sigma = ylim)$range
          man_ylim = FALSE
        }
      } else{
        man_xlim = TRUE
        man_ylim = TRUE
      }
      
      if (man_xlim == FALSE) {
        xlim = range(x[sel][y[sel] >= min(ylim, na.rm = TRUE) &
                              y[sel] <= max(ylim, na.rm = TRUE)], na.rm = TRUE)
      }
      if (man_ylim == FALSE) {
        ylim = range(y[sel][x[sel] >= min(xlim, na.rm = TRUE) &
                              x[sel] <= max(xlim, na.rm = TRUE)], na.rm = TRUE)
      }
      
      if (is.null(z)) { #No z-axis (this is your classic simple scatter plot basically)
        plot(
          x = x,
          y = y,
          axes = FALSE,
          xlab = '',
          ylab = '',
          main = main,
          log = log,
          frame.plot = FALSE,
          panel.first = if (side[1] != FALSE) {
            magaxis(
              side = side,
              majorn = majorn,
              minorn = minorn,
              tcl = tcl,
              ratio = ratio,
              labels = labels,
              unlog = unlog,
              mgp = mgp,
              mtline = mtline,
              xlab = xlab,
              ylab = ylab,
              crunch = crunch,
              logpretty = logpretty,
              prettybase = prettybase,
              powbase = powbase,
              hersh = hersh,
              family = family,
              frame.plot = frame.plot,
              usepar = usepar,
              grid = grid,
              grid.col = grid.col,
              grid.lty = grid.lty,
              grid.lwd = grid.lwd,
              axis.lwd = axis.lwd,
              ticks.lwd = ticks.lwd,
              axis.col = axis.col,
              ...
            )
          },
          lwd = lwd,
          xlim = xlim,
          ylim = ylim,
          ...
        )
        return(invisible(NULL))
      } else{ # With z-axis (more complex z axis colouring)
        dots = list(...)
        dotskeepmap = c("locut", "hicut", "flip", "type", "stretchscale", "clip")
        dotskeepbar = c(
          "position",
          "orient",
          "scale",
          "inset",
          "labN",
          "title",
          "titleshift",
          "centrealign"
        )
        
        if (length(dots) > 0) {
          dotsmap = dots[names(dots) %in% dotskeepmap]
          dots = dots[!names(dots) %in% dotskeepmap]
          dotsbar = dots[names(dots) %in% dotskeepbar]
          dots = dots[!names(dots) %in% dotskeepbar]
        } else{
          dotsmap = {
          }
          dotsbar = {
          }
        }
        if ('z' %in% logsplit & !is.null(z)) {
          z = log10(z)
          if (missing(unlog)) {
            unlog = paste(logsplit[-which(logsplit == 'z')], collapse = '')
          }
          if (missing(zcol)) {
            zstretch = 'log'
          }
        }
        colmap = do.call("magmap", c(
          list(
            data = z,
            stretch = zstretch,
            range = c(1, length(zcol)),
            bad = NA
          ),
          dotsmap
        ))
        plot(
          x = NA,
          y = NA,
          axes = FALSE,
          xlab = '',
          ylab = '',
          main = main,
          log = log,
          frame.plot = FALSE,
          panel.first = if (side[1] != FALSE) {
            magaxis(
              side = side,
              majorn = majorn,
              minorn = minorn,
              tcl = tcl,
              ratio = ratio,
              labels = labels,
              unlog = unlog,
              mgp = mgp,
              mtline = mtline,
              xlab = xlab,
              ylab = ylab,
              crunch = crunch,
              logpretty = logpretty,
              prettybase = prettybase,
              powbase = powbase,
              hersh = hersh,
              family = family,
              frame.plot = frame.plot,
              usepar = usepar,
              grid = grid,
              grid.col = grid.col,
              grid.lty = grid.lty,
              grid.lwd = grid.lwd,
              axis.lwd = axis.lwd,
              ticks.lwd = ticks.lwd,
              axis.col = axis.col,
              ...
            )
          },
          lwd = lwd,
          xlim = xlim,
          ylim = ylim
        )
        do.call("points", c(list(
          x = x, y = y, col = zcol[colmap$map]
        ), dots))
        if (dobar) {
          do.call("magbar", c(
            list(
              range = colmap$datalim,
              log = zstretch == 'log',
              col = zcol
            ),
            dotsbar
          ))
        }
        return(invisible(NULL))
      }
    }
  }
