# Interface to the Schlegel/Finkbeiner/Davis C code via R's .C interface.

# Define the main dust map lookup wrapper.
dust_getval <- function(gall = NULL, galb = NULL, map = c("Ebv", "I100", "X", "T", "mask"), ipath = system.file("extdata", package = "magicaxis"), interp = FALSE, noloop = FALSE, verbose = FALSE, infile = NULL, outfile = NULL, code_dir = system.file("src", package = "magicaxis"), shared_object = NULL, rebuild = FALSE) {
  # Match the requested map name against the supported options.
  map <- match.arg(map)
  # Normalize the source directory used to build or load the shared object.
  code_dir <- normalizePath(code_dir, mustWork = TRUE)
  # Resolve the directory that should contain the dust map FITS files.
  ipath <- dust_getval_resolve_ipath(ipath)

  # Resolve galactic coordinates from vectors or an input file.
  coords <- dust_getval_resolve_coords(gall = gall, galb = galb, infile = infile)

  # Disable interpolation for the categorical mask map.
  if (isTRUE(interp) && identical(map, "mask")) {
    # Force interpolation off for mask lookups.
    interp <- FALSE
  }

  # Resolve the north and south FITS files for the selected map.
  file_pair <- dust_getval_map_files(map = map, ipath = ipath)
  # Load or rebuild the native shared library used for the lookup.
  dll <- dust_getval_load_library(code_dir = code_dir, shared_object = shared_object, rebuild = rebuild)

  # Call the native lookup routine with the selected files and coordinates.
  result <- .Call("r_dust_getval_call", file_pair[["north"]], file_pair[["south"]], as.double(coords[["gall"]]), as.double(coords[["galb"]]), isTRUE(interp), isTRUE(noloop), isTRUE(verbose), PACKAGE = dll[["name"]])

  # Convert the native output into the expected R data frame format.
  parsed <- dust_getval_format_output(coords, result, map)

  # Write the parsed output to disk when an output file has been requested.
  if (!is.null(outfile)) {
    # Persist the formatted lookup output.
    dust_getval_write_output(parsed, map, outfile)
  }

  # Return the parsed lookup results.
  parsed
}

# Define a helper that resolves coordinate input from vectors or a file.
dust_getval_resolve_coords <- function(gall = NULL, galb = NULL, infile = NULL) {
  # Reject mixed usage of explicit coordinates and an input file.
  if (!is.null(infile) && (!is.null(gall) || !is.null(galb))) {
    # Stop when both coordinate input modes are supplied.
    stop("Provide either gall/galb or infile, not both.", call. = FALSE)
  }

  # Read coordinates from a file when one has been supplied.
  if (!is.null(infile)) {
    # Normalize the input file path before reading it.
    infile <- normalizePath(infile, mustWork = TRUE)
    # Read the coordinate table without treating the first row as a header.
    values <- utils::read.table(infile, header = FALSE, stringsAsFactors = FALSE)

    # Ensure the input file contains at least longitude and latitude columns.
    if (ncol(values) < 2L) {
      # Stop when the file does not contain both coordinate columns.
      stop("infile must contain at least two columns: gall and galb.", call. = FALSE)
    }

    # Convert the first two columns into a standard coordinate data frame.
    coords <- data.frame(gall = as.numeric(values[[1L]]), galb = as.numeric(values[[2L]]), stringsAsFactors = FALSE)
  } else {
    # Require both coordinate vectors when no input file is supplied.
    if (is.null(gall) || is.null(galb)) {
      # Stop when the explicit coordinate inputs are incomplete.
      stop("Provide both gall and galb, or supply infile.", call. = FALSE)
    }

    # Convert the supplied vectors into a standard coordinate data frame.
    coords <- data.frame(gall = as.numeric(gall), galb = as.numeric(galb), stringsAsFactors = FALSE)
  }

  # Require at least one coordinate pair.
  if (!nrow(coords)) {
    # Stop when no coordinates were provided.
    stop("At least one coordinate pair is required.", call. = FALSE)
  }

  # Ensure longitude and latitude vectors have matching lengths.
  if (length(coords[["gall"]]) != length(coords[["galb"]])) {
    # Stop when the coordinate vectors are misaligned.
    stop("gall and galb must have the same length.", call. = FALSE)
  }

  # Reject missing or non-numeric coordinates after coercion.
  if (anyNA(coords[["gall"]]) || anyNA(coords[["galb"]])) {
    # Stop when any coordinate value is invalid.
    stop("Coordinates must be numeric and non-missing.", call. = FALSE)
  }

  # Return the validated coordinate data frame.
  coords
}

# Define a helper that resolves the directory containing the dust maps.
dust_getval_resolve_ipath <- function(ipath = NULL) {
  # Prefer an explicitly supplied directory when present.
  if (!is.null(ipath)) {
    # Expand any user-home shorthand in the supplied directory path.
    ipath <- path.expand(ipath)
    # Ensure the supplied directory exists before using it.
    if (!dir.exists(ipath)) {
      # Stop when the requested map directory is missing.
      stop(sprintf("Directory does not exist: %s", ipath), call. = FALSE)
    }
    # Return the validated directory path.
    return(ipath)
  }

  # Read the optional legacy environment variable that points to the dust maps.
  dust_dir <- Sys.getenv("DUST_DIR", unset = "")
  # Use the maps subdirectory beneath DUST_DIR when available.
  if (nzchar(dust_dir)) {
    # Build the expected maps directory path.
    map_dir <- file.path(dust_dir, "maps")
    # Return the legacy maps directory when it exists.
    if (dir.exists(map_dir)) {
      # Use the discovered DUST_DIR/maps location.
      return(map_dir)
    }
  }

  # Fall back to the current directory when no other location is available.
  "."
}

# Define a helper that maps a logical map name to its FITS file pair.
dust_getval_map_files <- function(map, ipath) {
  # Store the north and south FITS filenames for each supported map.
  map_files <- list(Ebv = c("SFD_dust_409_ngp.fits", "SFD_dust_409_sgp.fits"), I100 = c("SFD_i100_4096_ngp.fits", "SFD_i100_4096_sgp.fits"), X = c("SFD_xmap_ngp.fits", "SFD_xmap_sgp.fits"), T = c("SFD_temp_ngp.fits", "SFD_temp_sgp.fits"), mask = c("SFD_mask_4096_ngp.fits", "SFD_mask_4096_sgp.fits"))

  # Build the full paths to the required north and south FITS files.
  files <- file.path(ipath, map_files[[map]])
  # Identify any missing FITS files.
  missing <- files[!file.exists(files)]
  # Stop early when one or both FITS files are unavailable.
  if (length(missing)) {
    # Report the missing FITS files in a compact error message.
    stop(sprintf("Missing map file(s): %s", paste(basename(missing), collapse = ", ")), call. = FALSE)
  }

  # Detect paths that exceed the fixed-size buffer in the legacy C code.
  too_long <- nchar(files, type = "bytes") >= 80L
  # Copy the FITS files locally when any path is too long for the native code.
  if (any(too_long)) {
    # Warn that the files are being copied to satisfy the legacy path limit.
    warning(sprintf("Map file path exceeds the legacy C limit of 79 bytes:\n%s\nCopying maps to working directory.", files[[which(too_long)[1L]]]), call. = FALSE)
    # Copy the FITS files into the current working directory.
    file.copy(from = files, to = getwd(), overwrite = TRUE)
    # Switch the map directory to the working directory after copying.
    ipath <- "./"
    # Rebuild the FITS file paths using the shorter local directory.
    files <- file.path(ipath, map_files[[map]])
    # Recheck whether any copied FITS files are still missing.
    missing <- files[!file.exists(files)]
    # Stop when the attempted copy did not produce both files.
    if (length(missing)) {
      print(missing)
      # Report the files that still could not be found after copying.
      stop(sprintf("Copy failed: %s", paste(basename(missing), collapse = ", ")), call. = FALSE)
    }
  }
  # Return the resolved north and south FITS file paths.
  list(north = files[[1L]], south = files[[2L]])
}

# Define a helper that loads or rebuilds the native shared library interface.
dust_getval_load_library <- function(code_dir = system.file("src", "", package = "magicaxis"), shared_object = NULL, rebuild = FALSE) {
  # Normalize the directory that should contain the native code.
  code_dir <- normalizePath(code_dir, mustWork = TRUE)

  # Derive the default shared-object path when one is not supplied.
  if (is.null(shared_object)) {
    # Capture the platform-specific shared-library extension.
    ext <- .Platform$dynlib.ext
    # Build the default shared-object path inside the code directory.
    shared_object <- file.path(code_dir, paste0("r_dust_getval", ext))
  } else {
    # Normalize any explicitly supplied shared-object path.
    shared_object <- normalizePath(shared_object, mustWork = FALSE)
  }

  # Build the list of C source files required for compilation.
  sources <- file.path(code_dir, c("r_dust_getval.c", "subs_lambert.c", "subs_fits.c", "subs_memory.c", "subs_inoutput.c"))

  # Ensure every required C source file is present before proceeding.
  if (!all(file.exists(sources))) {
    # Stop when any native source file is missing.
    stop("Required C sources for the R interface are missing.", call. = FALSE)
  }

  # Decide whether the shared object must be rebuilt.
  needs_build <- rebuild || !file.exists(shared_object)
  # Compare timestamps when a shared object already exists.
  if (!needs_build) {
    # Capture the modification time of the current shared object.
    so_time <- file.info(shared_object)$mtime
    # Find the newest modification time among the source files.
    src_time <- max(file.info(sources)$mtime)
    # Rebuild when timestamps are missing or the sources are newer than the binary.
    needs_build <- is.na(so_time) || is.na(src_time) || so_time < src_time
  }

  # Rebuild the shared object when required.
  if (needs_build) {
    # Derive the DLL name from the requested shared-object path.
    dll_name <- tools::file_path_sans_ext(basename(shared_object))
    # Unload any already loaded copy before recompiling it.
    if (dll_name %in% names(getLoadedDLLs())) {
      # Unload the active shared library to avoid replacing a loaded binary.
      dyn.unload(getLoadedDLLs()[[dll_name]][["path"]])
    }
    # Compile the native code into the requested shared object.
    dust_getval_compile_library(code_dir = code_dir, shared_object = shared_object)
  }

  # Recompute the DLL name for the final load step.
  dll_name <- tools::file_path_sans_ext(basename(shared_object))
  # Load the shared library when it is not already in memory.
  if (!dll_name %in% names(getLoadedDLLs())) {
    # Dynamically load the compiled shared object.
    dyn.load(shared_object)
  }

  # Return the DLL name and path used by the .Call invocation.
  list(name = dll_name, path = shared_object)
}

# Define a helper that compiles the dust lookup C sources into a shared library.
dust_getval_compile_library <- function(code_dir, shared_object) {
  # Resolve the R executable used to invoke CMD SHLIB.
  r_bin <- file.path(R.home("bin"), "R")
  # Define the location where the shared object will be built.
  built_path <- file.path(code_dir, basename(shared_object))
  # Assemble the command-line arguments for building the shared library.
  args <- c("CMD", "SHLIB", "-o", basename(built_path), "r_dust_getval.c", "subs_lambert.c", "subs_fits.c", "subs_memory.c", "subs_inoutput.c")

  # Record the current working directory so it can be restored later.
  old_wd <- getwd()
  # Ensure the original working directory is restored on exit.
  on.exit(setwd(old_wd), add = TRUE)
  # Switch into the native source directory before building.
  setwd(code_dir)

  # Remove stale build outputs from earlier compilations.
  unlink(c(basename(built_path), "r_dust_getval.o", "subs_lambert.o", "subs_fits.o", "subs_memory.o", "subs_inoutput.o"))

  # Capture any existing compiler flags so they can be restored later.
  old_cflags <- Sys.getenv("PKG_CFLAGS", unset = NA_character_)
  # Capture any existing linker flags so they can be restored later.
  old_libs <- Sys.getenv("PKG_LIBS", unset = NA_character_)
  # Restore the previous compiler and linker flags on exit.
  on.exit({
    # Unset PKG_CFLAGS when it was not previously defined.
    if (is.na(old_cflags)) {
      # Remove the temporary compiler flags from the environment.
      Sys.unsetenv("PKG_CFLAGS")
    } else {
      # Restore the prior PKG_CFLAGS value.
      Sys.setenv(PKG_CFLAGS = old_cflags)
    }
    # Unset PKG_LIBS when it was not previously defined.
    if (is.na(old_libs)) {
      # Remove the temporary linker flags from the environment.
      Sys.unsetenv("PKG_LIBS")
    } else {
      # Restore the prior PKG_LIBS value.
      Sys.setenv(PKG_LIBS = old_libs)
    }
  }, add = TRUE)
  # Set the build flags required by the legacy C code.
  Sys.setenv(PKG_CFLAGS = "-D__LINUX", PKG_LIBS = "-lm")

  # Invoke R CMD SHLIB and capture its output.
  status <- system2(r_bin, args = args, stdout = TRUE, stderr = TRUE)
  # Stop with the compiler output when the build fails.
  if (!is.null(attr(status, "status"))) {
    # Surface the captured build output as the error message.
    stop(paste(status, collapse = "\n"), call. = FALSE)
  }

  # Ensure the expected shared object was produced.
  if (!file.exists(built_path)) {
    # Stop when the build command completed without producing the binary.
    stop("Failed to build the shared library for dust_getval.", call. = FALSE)
  }

  # Copy the built shared object when the requested output path differs.
  if (!identical(normalizePath(built_path), normalizePath(shared_object, mustWork = FALSE))) {
    # Copy the compiled shared library to the requested destination.
    file.copy(built_path, shared_object, overwrite = TRUE)
  }
}

# Define a helper that formats the native output for the requested map type.
dust_getval_format_output <- function(coords, values, map) {
  # Decode the integer bitmask when the requested map is the mask product.
  if (identical(map, "mask")) {
    # Round and coerce the returned values into integer mask values.
    mask <- as.integer(round(values))
    # Return the decoded mask flags alongside the original coordinates.
    data.frame(gall = coords[["gall"]], galb = coords[["galb"]], hcons = bitwAnd(mask, 3L), asteroid = as.logical(bitwAnd(mask, bitwShiftL(1L, 2L))), glitch = as.logical(bitwAnd(mask, bitwShiftL(1L, 3L))), source = as.logical(bitwAnd(mask, bitwShiftL(1L, 4L))), no_list = as.logical(bitwAnd(mask, bitwShiftL(1L, 5L))), big_obj = as.logical(bitwAnd(mask, bitwShiftL(1L, 6L))), no_IRAS = as.logical(bitwAnd(mask, bitwShiftL(1L, 7L))), stringsAsFactors = FALSE)
  } else {
    # Map the selected dust product to its output column name.
    value_name <- switch(map, Ebv = "ebv", I100 = "i100", X = "x", T = "temperature")

    # Start from the original coordinate data frame.
    out <- coords
    # Append the returned dust values under the appropriate column name.
    out[[value_name]] <- values
    # Return the completed data frame.
    out
  }
}

# Define a helper that writes formatted lookup results to a text file.
dust_getval_write_output <- function(parsed, map, outfile) {
  # Expand any user-home shorthand in the output path.
  outfile <- path.expand(outfile)

  # Format mask output using the legacy textual representation.
  if (identical(map, "mask")) {
    # Build one formatted output line per decoded mask row.
    lines <- sprintf("%8.3f %7.3f  %1dhcons %7s %7s %7s %7s %7s %7s", parsed[["gall"]], parsed[["galb"]], parsed[["hcons"]], ifelse(parsed[["asteroid"]], "asteroi", "OK"), ifelse(parsed[["glitch"]], "glitch", "OK"), ifelse(parsed[["source"]], "source", "OK"), ifelse(parsed[["no_list"]], "no_list", "OK"), ifelse(parsed[["big_obj"]], "big_obj", "OK"), ifelse(parsed[["no_IRAS"]], "no_IRAS", "OK"))
  } else {
    # Capture the third column name, which holds the selected dust quantity.
    value_name <- names(parsed)[[3L]]
    # Build one formatted output line per coordinate pair.
    lines <- sprintf("%8.3f %7.3f %12.5f", parsed[["gall"]], parsed[["galb"]], parsed[[value_name]])
  }

  # Write the formatted lines to the requested output file.
  writeLines(lines, outfile)
}


