## Tests for magclip, maghist, magrun, magerr, magcurve, magcon, magbar,
## magcutout, and magmap type='rank'.
##
## Each group covers:
##  1. Correct return values / output structure.
##  2. That edge-case or previously-buggy paths work without error.

library(magicaxis)

options(rgl.useNULL = TRUE)

with_null_dev <- function(expr) {
  pdf(nullfile())
  on.exit(dev.off())
  force(expr)
}

# ── magclip ───────────────────────────────────────────────────────────────────

test_that("magclip returns a list with x, clip, range, clipiters", {
  set.seed(1)
  result <- magclip(c(rnorm(200), runif(20, 5, 10)))
  expect_true(is.list(result))
  expect_true(all(c("x", "clip", "range", "clipiters") %in% names(result)))
})

test_that("magclip with extra=FALSE returns NA for clip and range", {
  result <- magclip(rnorm(100), extra = FALSE)
  expect_true(is.na(result$clip))
  expect_true(is.na(result$range))
})

test_that("magclip removes obvious outliers", {
  set.seed(2)
  x <- c(rnorm(500), 100, -100)
  result <- magclip(x, sigma = 3)
  expect_true(max(result$x) < 50)
  expect_true(min(result$x) > -50)
})

test_that("magclip estimate='lo' and estimate='hi' work without error", {
  set.seed(3)
  x <- c(rnorm(300), runif(30, 4, 8))
  expect_no_error(magclip(x, estimate = 'lo'))
  expect_no_error(magclip(x, estimate = 'hi'))
})

test_that("magclip with sigma='auto' and very small input does not error", {
  expect_no_error(magclip(c(1, 2, 3)))
})

test_that("magclip clip vector has same length as input x", {
  set.seed(4)
  x <- rnorm(100)
  result <- magclip(x)
  expect_equal(length(result$clip), length(x))
})

# ── magmap type='rank' (previously broken) ─────────────────────────────────

test_that("magmap type='rank' actually reorders data (bug fix)", {
  set.seed(5)
  d <- c(3, 1, 4, 1, 5, 9, 2, 6)
  result <- magmap(d, type = 'rank', range = c(1, 8))
  # After ranking, the minimum ranked value should map near range[1]
  # and the maximum near range[2].
  expect_true(min(result$map, na.rm = TRUE) <= 1.5)
  expect_true(max(result$map, na.rm = TRUE) >= 7.5)
  # The mapping must actually differ across elements (was all identical before fix)
  expect_true(length(unique(result$map)) > 1)
})

test_that("magmap type='rank' with flip=TRUE inverts the mapping", {
  d <- 1:10
  normal  <- magmap(d, type = 'rank', range = c(0, 1))$map
  flipped <- magmap(d, type = 'rank', range = c(0, 1), flip = TRUE)$map
  expect_true(cor(normal, flipped) < -0.9)
})

# ── maghist ───────────────────────────────────────────────────────────────────

test_that("maghist returns a histogram object", {
  result <- maghist(rnorm(500), plot = FALSE, verbose = FALSE)
  expect_true(inherits(result, "histogram"))
})

test_that("maghist appends 'summary' and 'ranges' to the histogram", {
  result <- maghist(rnorm(200), plot = FALSE, verbose = FALSE)
  expect_true("summary" %in% names(result))
  expect_true("ranges" %in% names(result))
})

test_that("maghist with xlim scalar performs sigma clipping without error", {
  result <- maghist(c(rnorm(500), 100), xlim = 3, plot = FALSE, verbose = FALSE)
  expect_true(inherits(result, "histogram"))
})

test_that("maghist with log='x' works on positive data", {
  result <- maghist(10^runif(300, 0, 3), log = 'x', plot = FALSE, verbose = FALSE)
  expect_true(inherits(result, "histogram"))
})

test_that("maghist cumsum=TRUE gives non-decreasing counts", {
  result <- maghist(rnorm(300), cumsum = TRUE, plot = FALSE, verbose = FALSE)
  expect_true(all(diff(result$counts) >= 0))
})

test_that("maghist renders without error", {
  with_null_dev({
    expect_no_error(maghist(rnorm(300), verbose = FALSE))
  })
})

# ── magrun ───────────────────────────────────────────────────────────────────

test_that("magrun returns a list with correct element names", {
  set.seed(6)
  xy <- cbind(seq(0, 2, len = 200), rnorm(200))
  result <- magrun(xy)
  expect_true(is.list(result))
  # Check the names that the code actually returns
  expect_true(all(c("x", "y", "xquan", "yquan", "xsd", "ysd",
                    "bincens", "binlims", "Nbins") %in% names(result)))
})

test_that("magrun Nbins sums match input count", {
  set.seed(7)
  n <- 1000
  xy <- cbind(runif(n), runif(n))
  result <- magrun(xy, bins = 5, equalN = TRUE)
  expect_equal(sum(result$Nbins), n)
})

test_that("magrun with type='mean' works", {
  set.seed(8)
  xy <- cbind(seq(0, 1, len = 100), rnorm(100))
  expect_no_error(magrun(xy, type = 'mean'))
})

test_that("magrun with custom breaks works", {
  set.seed(9)
  xy <- cbind(seq(0, 2, len = 200), rnorm(200))
  result <- magrun(xy, bins = c(0, 0.5, 1.0, 1.5, 2.0))
  expect_equal(length(result$x), 4)
})

test_that("magrun Nscale=TRUE reduces spread relative to Nscale=FALSE", {
  set.seed(10)
  xy <- cbind(seq(0, 1, len = 500), rnorm(500))
  r_noscale <- magrun(xy, bins = 5, Nscale = FALSE)
  r_scale   <- magrun(xy, bins = 5, Nscale = TRUE)
  # Nscale divides by sqrt(N), so quantile widths should be narrower
  width_no <- mean(r_noscale$yquan[, 2] - r_noscale$yquan[, 1])
  width_sc <- mean(r_scale$yquan[, 2] - r_scale$yquan[, 1])
  expect_true(width_sc < width_no)
})

# ── magerr ───────────────────────────────────────────────────────────────────

test_that("magerr draws y error bars without error", {
  with_null_dev({
    magplot(1:5, 1:5)
    expect_no_error(magerr(x = 1:5, y = 1:5, ylo = 0.2, yhi = 0.3))
  })
})

test_that("magerr draws x error bars without error", {
  with_null_dev({
    magplot(1:5, 1:5)
    expect_no_error(magerr(x = 1:5, y = 1:5, xlo = 0.1))
  })
})

test_that("magerr draws both x and y error bars without error", {
  with_null_dev({
    magplot(1:10, 1:10)
    expect_no_error(magerr(x = 1:10, y = 1:10, xlo = 0.15, ylo = 0.25))
  })
})

test_that("magerr with poly=TRUE draws error polygon without error", {
  with_null_dev({
    magplot(1:5, 1:5)
    expect_no_error(
      magerr(x = 1:5, y = 1:5, ylo = rep(0.2, 5), yhi = rep(0.3, 5),
             poly = TRUE, col = 'lightgrey', border = NA)
    )
  })
})

test_that("magerr on log-x axis works without error", {
  with_null_dev({
    magplot(10^(1:5), 1:5, log = 'x')
    expect_no_error(
      magerr(x = 10^(1:5), y = 1:5, xlo = 10^(1:5) * 0.2)
    )
  })
})

test_that("magerr accepts x as a two-column matrix", {
  with_null_dev({
    magplot(1:5, 1:5)
    xy <- cbind(1:5, 1:5)
    expect_no_error(magerr(x = xy, ylo = 0.2))
  })
})

# ── magcurve ─────────────────────────────────────────────────────────────────

test_that("magcurve returns a list with x and y", {
  with_null_dev({
    result <- magcurve(sin(x), from = 0, to = 2 * pi)
    expect_true(is.list(result))
    expect_equal(names(result), c("x", "y"))
    expect_equal(length(result$x), 101)
  })
})

test_that("magcurve add=TRUE adds to an existing plot without error", {
  with_null_dev({
    magplot(0, 0, xlim = c(0, 6), ylim = c(-1.5, 1.5))
    expect_no_error(magcurve(cos(x), from = 0, to = 2 * pi, add = TRUE))
  })
})

test_that("magcurve with log x axis works on positive domain", {
  with_null_dev({
    expect_no_error(magcurve(log10(x), from = 1, to = 1000, log = 'x'))
  })
})

# ── magcon ───────────────────────────────────────────────────────────────────

test_that("magcon returns a list with x, y, z components", {
  with_null_dev({
    set.seed(11)
    x <- rnorm(300); y <- x + rnorm(300, sd = 0.5)
    result <- magcon(x, y, h = c(0.3, 0.3), dobar = FALSE)
    expect_true(is.list(result))
    expect_true(all(c("x", "y", "z") %in% names(result)))
  })
})

test_that("magcon doim=FALSE, docon=TRUE works without error", {
  with_null_dev({
    set.seed(12)
    x <- rnorm(200); y <- rnorm(200)
    expect_no_error(magcon(x, y, h = c(0.3, 0.3), doim = FALSE, dobar = FALSE))
  })
})

test_that("magcon with add=TRUE adds to existing plot", {
  with_null_dev({
    set.seed(13)
    x <- rnorm(200); y <- rnorm(200)
    magplot(x, y)
    expect_no_error(magcon(x, y, h = c(0.3, 0.3), add = TRUE, dobar = FALSE))
  })
})

# ── magbar ────────────────────────────────────────────────────────────────────

test_that("magbar renders a vertical colour bar without error", {
  with_null_dev({
    magplot(1, 1, xlim = c(0, 10), ylim = c(0, 10))
    expect_no_error(magbar(range = c(0, 100), orient = 'v'))
  })
})

test_that("magbar renders a horizontal colour bar without error", {
  with_null_dev({
    magplot(1, 1, xlim = c(0, 10), ylim = c(0, 10))
    expect_no_error(magbar(range = c(0, 100), orient = 'h'))
  })
})

test_that("magbar with log=TRUE works without error", {
  with_null_dev({
    magplot(1, 1, xlim = c(0, 10), ylim = c(0, 10))
    expect_no_error(magbar(range = c(1, 1000), log = TRUE))
  })
})

test_that("magbar position argument variants work without error", {
  with_null_dev({
    for (pos in c('topright', 'topleft', 'bottomright', 'bottomleft')) {
      magplot(1, 1, xlim = c(0, 10), ylim = c(0, 10))
      expect_no_error(magbar(position = pos, range = c(0, 1)))
    }
  })
})

# ── magcutout ────────────────────────────────────────────────────────────────

test_that("magcutout extracts correct submatrix from interior", {
  m <- matrix(1:100, 10, 10)
  result <- magcutout(m, loc = c(5, 5), box = c(4, 4))
  expect_equal(dim(result$image), c(4, 4))
})

test_that("magcutout returns loc, loc.orig, loc.diff", {
  m <- matrix(1:100, 10, 10)
  result <- magcutout(m, loc = c(5, 5), box = c(4, 4))
  expect_true(all(c("image", "loc", "loc.orig", "loc.diff", "xsel", "ysel") %in%
                    names(result)))
})

test_that("magcutout with paddim=TRUE pads near-edge cutouts to requested box size", {
  m <- matrix(1:100, 10, 10)
  result <- magcutout(m, loc = c(1, 1), box = c(6, 6), paddim = TRUE)
  expect_equal(dim(result$image), c(6, 6))
})

test_that("magcutout with paddim=FALSE clips to available data", {
  m <- matrix(1:100, 10, 10)
  result <- magcutout(m, loc = c(1, 1), box = c(6, 6), paddim = FALSE)
  # Should not exceed matrix boundaries
  expect_true(all(dim(result$image) <= c(6, 6)))
})

test_that("magcutout plots without error", {
  with_null_dev({
    m <- matrix(rnorm(400), 20, 20)
    expect_no_error(magcutout(m, loc = c(10, 10), box = c(8, 8), plot = TRUE))
  })
})
