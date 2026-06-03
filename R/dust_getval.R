# Interface to the Schlegel/Finkbeiner/Davis C code via R's .C interface.

dust_getval <- function(gall = NULL,
                        galb = NULL,
                        map = c("Ebv", "I100", "X", "T", "mask"),
                        ipath = system.file('extdata', package = 'magicaxis'),
                        interp = FALSE,
                        noloop = FALSE,
                        verbose = FALSE,
                        infile = NULL,
                        outfile = NULL,
                        code_dir = system.file("src", package = "magicaxis"),
                        shared_object = NULL,
                        rebuild = FALSE) {
  map <- match.arg(map)
  code_dir <- normalizePath(code_dir, mustWork = TRUE)
  ipath <- dust_getval_resolve_ipath(ipath)

  coords <- dust_getval_resolve_coords(gall = gall, galb = galb, infile = infile)

  if (isTRUE(interp) && identical(map, "mask")) {
    interp <- FALSE
  }

  file_pair <- dust_getval_map_files(map = map, ipath = ipath)
  dll <- dust_getval_load_library(
    code_dir = code_dir,
    shared_object = shared_object,
    rebuild = rebuild
  )

  result <- .Call(
    "r_dust_getval_call",
    file_pair[["north"]],
    file_pair[["south"]],
    as.double(coords[["gall"]]),
    as.double(coords[["galb"]]),
    isTRUE(interp),
    isTRUE(noloop),
    isTRUE(verbose),
    PACKAGE = dll[["name"]]
  )

  parsed <- dust_getval_format_output(coords, result, map)

  if (!is.null(outfile)) {
    dust_getval_write_output(parsed, map, outfile)
  }

  parsed
}

dust_getval_resolve_coords <- function(gall = NULL, galb = NULL, infile = NULL) {
  if (!is.null(infile) && (!is.null(gall) || !is.null(galb))) {
    stop("Provide either gall/galb or infile, not both.", call. = FALSE)
  }

  if (!is.null(infile)) {
    infile <- normalizePath(infile, mustWork = TRUE)
    values <- utils::read.table(
      infile,
      header = FALSE,
      stringsAsFactors = FALSE
    )

    if (ncol(values) < 2L) {
      stop("infile must contain at least two columns: gall and galb.", call. = FALSE)
    }

    coords <- data.frame(
      gall = as.numeric(values[[1L]]),
      galb = as.numeric(values[[2L]]),
      stringsAsFactors = FALSE
    )
  } else {
    if (is.null(gall) || is.null(galb)) {
      stop("Provide both gall and galb, or supply infile.", call. = FALSE)
    }

    coords <- data.frame(
      gall = as.numeric(gall),
      galb = as.numeric(galb),
      stringsAsFactors = FALSE
    )
  }

  if (!nrow(coords)) {
    stop("At least one coordinate pair is required.", call. = FALSE)
  }

  if (length(coords[["gall"]]) != length(coords[["galb"]])) {
    stop("gall and galb must have the same length.", call. = FALSE)
  }

  if (anyNA(coords[["gall"]]) || anyNA(coords[["galb"]])) {
    stop("Coordinates must be numeric and non-missing.", call. = FALSE)
  }

  coords
}

dust_getval_resolve_ipath <- function(ipath = NULL) {
  if (!is.null(ipath)) {
    ipath <- path.expand(ipath)
    if (!dir.exists(ipath)) {
      stop(sprintf("Directory does not exist: %s", ipath), call. = FALSE)
    }
    return(ipath)
  }

  dust_dir <- Sys.getenv("DUST_DIR", unset = "")
  if (nzchar(dust_dir)) {
    map_dir <- file.path(dust_dir, "maps")
    if (dir.exists(map_dir)) {
      return(map_dir)
    }
  }

  "."
}

dust_getval_map_files <- function(map, ipath) {
  map_files <- list(
    Ebv = c("SFD_dust_4096_ngp.fits", "SFD_dust_4096_sgp.fits"),
    I100 = c("SFD_i100_4096_ngp.fits", "SFD_i100_4096_sgp.fits"),
    X = c("SFD_xmap_ngp.fits", "SFD_xmap_sgp.fits"),
    T = c("SFD_temp_ngp.fits", "SFD_temp_sgp.fits"),
    mask = c("SFD_mask_4096_ngp.fits", "SFD_mask_4096_sgp.fits")
  )

  files <- file.path(ipath, map_files[[map]])
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    stop(
      sprintf("Missing map file(s): %s", paste(basename(missing), collapse = ", ")),
      call. = FALSE
    )
  }

  too_long <- nchar(files, type = "bytes") >= 80L
  if (any(too_long)) {
    warning(
      sprintf(
        "Map file path exceeds the legacy C limit of 79 bytes: %s\nCopying maps to working directory.",
        files[[which(too_long)[1L]]]
      ),
      call. = FALSE
    )
    system(paste('cp',files,'.'))
    ipath='./'
    files <- file.path(ipath, map_files[[map]])
    missing <- files[!file.exists(files)]
    if (length(missing)) {
      stop(
        sprintf("Copy failed: %s", paste(basename(missing), collapse = ", ")),
        call. = FALSE
      )
    }
  }
  list(north = files[[1L]], south = files[[2L]])
}

dust_getval_load_library <- function(code_dir = system.file("src", '', package = "magicaxis"),
                                     shared_object = NULL,
                                     rebuild = FALSE) {
  code_dir <- normalizePath(code_dir, mustWork = TRUE)

  if (is.null(shared_object)) {
    ext <- .Platform$dynlib.ext
    shared_object <- file.path(code_dir, paste0("r_dust_getval", ext))
  } else {
    shared_object <- normalizePath(shared_object, mustWork = FALSE)
  }

  sources <- file.path(
    code_dir,
    c(
      "r_dust_getval.c",
      "subs_lambert.c",
      "subs_fits.c",
      "subs_memory.c",
      "subs_inoutput.c"
    )
  )

  if (!all(file.exists(sources))) {
    stop("Required C sources for the R interface are missing.", call. = FALSE)
  }

  needs_build <- rebuild || !file.exists(shared_object)
  if (!needs_build) {
    so_time <- file.info(shared_object)$mtime
    src_time <- max(file.info(sources)$mtime)
    needs_build <- is.na(so_time) || is.na(src_time) || so_time < src_time
  }

  if (needs_build) {
    dll_name <- tools::file_path_sans_ext(basename(shared_object))
    if (dll_name %in% names(getLoadedDLLs())) {
      dyn.unload(getLoadedDLLs()[[dll_name]][["path"]])
    }
    dust_getval_compile_library(code_dir = code_dir, shared_object = shared_object)
  }

  dll_name <- tools::file_path_sans_ext(basename(shared_object))
  if (!dll_name %in% names(getLoadedDLLs())) {
    dyn.load(shared_object)
  }

  list(name = dll_name, path = shared_object)
}

dust_getval_compile_library <- function(code_dir, shared_object) {
  r_bin <- file.path(R.home("bin"), "R")
  built_path <- file.path(code_dir, basename(shared_object))
  args <- c(
    "CMD", "SHLIB",
    "-o", basename(built_path),
    "r_dust_getval.c",
    "subs_lambert.c",
    "subs_fits.c",
    "subs_memory.c",
    "subs_inoutput.c"
  )

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(code_dir)

  unlink(c(
    basename(built_path),
    "r_dust_getval.o",
    "subs_lambert.o",
    "subs_fits.o",
    "subs_memory.o",
    "subs_inoutput.o"
  ))

  old_cflags <- Sys.getenv("PKG_CFLAGS", unset = NA_character_)
  old_libs <- Sys.getenv("PKG_LIBS", unset = NA_character_)
  on.exit({
    if (is.na(old_cflags)) {
      Sys.unsetenv("PKG_CFLAGS")
    } else {
      Sys.setenv(PKG_CFLAGS = old_cflags)
    }
    if (is.na(old_libs)) {
      Sys.unsetenv("PKG_LIBS")
    } else {
      Sys.setenv(PKG_LIBS = old_libs)
    }
  }, add = TRUE)
  Sys.setenv(PKG_CFLAGS = "-D__LINUX", PKG_LIBS = "-lm")

  status <- system2(
    r_bin,
    args = args,
    stdout = TRUE,
    stderr = TRUE
  )
  if (!is.null(attr(status, "status"))) {
    stop(paste(status, collapse = "\n"), call. = FALSE)
  }

  if (!file.exists(built_path)) {
    stop("Failed to build the shared library for dust_getval.", call. = FALSE)
  }

  if (!identical(normalizePath(built_path), normalizePath(shared_object, mustWork = FALSE))) {
    file.copy(built_path, shared_object, overwrite = TRUE)
  }
}

dust_getval_format_output <- function(coords, values, map) {
  if (identical(map, "mask")) {
    mask <- as.integer(round(values))
    data.frame(
      gall = coords[["gall"]],
      galb = coords[["galb"]],
      hcons = bitwAnd(mask, 3L),
      asteroid = as.logical(bitwAnd(mask, bitwShiftL(1L, 2L))),
      glitch = as.logical(bitwAnd(mask, bitwShiftL(1L, 3L))),
      source = as.logical(bitwAnd(mask, bitwShiftL(1L, 4L))),
      no_list = as.logical(bitwAnd(mask, bitwShiftL(1L, 5L))),
      big_obj = as.logical(bitwAnd(mask, bitwShiftL(1L, 6L))),
      no_IRAS = as.logical(bitwAnd(mask, bitwShiftL(1L, 7L))),
      stringsAsFactors = FALSE
    )
  } else {
    value_name <- switch(
      map,
      Ebv = "ebv",
      I100 = "i100",
      X = "x",
      T = "temperature"
    )

    out <- coords
    out[[value_name]] <- values
    out
  }
}

dust_getval_write_output <- function(parsed, map, outfile) {
  outfile <- path.expand(outfile)

  if (identical(map, "mask")) {
    lines <- sprintf(
      "%8.3f %7.3f  %1dhcons %7s %7s %7s %7s %7s %7s",
      parsed[["gall"]],
      parsed[["galb"]],
      parsed[["hcons"]],
      ifelse(parsed[["asteroid"]], "asteroi", "OK"),
      ifelse(parsed[["glitch"]], "glitch", "OK"),
      ifelse(parsed[["source"]], "source", "OK"),
      ifelse(parsed[["no_list"]], "no_list", "OK"),
      ifelse(parsed[["big_obj"]], "big_obj", "OK"),
      ifelse(parsed[["no_IRAS"]], "no_IRAS", "OK")
    )
  } else {
    value_name <- names(parsed)[[3L]]
    lines <- sprintf(
      "%8.3f %7.3f %12.5f",
      parsed[["gall"]],
      parsed[["galb"]],
      parsed[[value_name]]
    )
  }

  writeLines(lines, outfile)
}
