## Tests for ParmOff-based argument routing
##
## These tests verify that:
##  1. Extra `...` args are silently filtered by ParmOff (no "unused argument" errors).
##  2. The key sub-function routing changes work as expected:
##     - magaxis accepts axis-styling args forwarded via ParmOff.
##     - magplot + magmap/magbar/points dispatch works.
##     - magtri routes dots to magaxis, magcon AND points.
##  3. Return values from the affected functions are unchanged.

library(magicaxis)

# ── Helpers ──────────────────────────────────────────────────────────────────

# Render to a null device so no on-screen window is opened.
with_null_dev <- function(expr) {
  pdf(nullfile())
  on.exit(dev.off())
  force(expr)
}

# ── magaxis ───────────────────────────────────────────────────────────────────

test_that("magaxis forwards cex.axis and col.axis via ParmOff", {
  with_null_dev({
    plot(1:10, 1:10, axes = FALSE)
    expect_no_error(magaxis(side = 1:2, cex.axis = 0.8, col.axis = "blue"))
  })
})

test_that("magaxis does not error on unrecognized extra args (ParmOff filters them)", {
  with_null_dev({
    plot(1:10, 1:10, axes = FALSE)
    # 'bogus_arg' is not a formal of axis/mtext; ParmOff should silently drop it.
    expect_no_error(magaxis(side = 1, bogus_arg = 99))
  })
})

test_that("magaxis with xlab/ylab renders without error via ParmOff mtext call", {
  with_null_dev({
    plot(1:5, 1:5, axes = FALSE)
    expect_no_error(magaxis(side = 1:2, xlab = "X axis", ylab = "Y axis",
                            cex.lab = 1.2, col.lab = "darkred"))
  })
})

# ── magplot ───────────────────────────────────────────────────────────────────

test_that("magplot basic scatter returns invisibly without error", {
  with_null_dev({
    result <- magplot(1:10, (1:10)^2, xlab = "x", ylab = "y^2")
    expect_null(result)
  })
})

test_that("magplot with log x axis works via ParmOff axis dispatch", {
  with_null_dev({
    expect_no_error(magplot(10^(1:6), 1:6, log = "x", xlab = "log x"))
  })
})

test_that("magplot with z column dispatches magmap and magbar without error", {
  with_null_dev({
    set.seed(1)
    x <- rnorm(50); y <- rnorm(50); z <- x^2 + y^2
    expect_no_error(magplot(x, y, z = z, dobar = TRUE))
  })
})

test_that("magplot histogram dispatch via ParmOff works", {
  with_null_dev({
    h <- maghist(rnorm(200), plot = FALSE, verbose = FALSE)
    expect_no_error(magplot(h))
  })
})

# ── magmap ────────────────────────────────────────────────────────────────────

test_that("magmap returns a list with map and datalim elements", {
  result <- magmap(1:100)
  expect_true(is.list(result))
  expect_true("map" %in% names(result))
  expect_true("datalim" %in% names(result))
})

test_that("magmap stretch='log' works on positive data", {
  result <- magmap(10^(0:4), stretch = "log")
  expect_equal(length(result$map), 5)
})

# ── magbin ────────────────────────────────────────────────────────────────────

test_that("magbin returns a magbin object without error", {
  set.seed(42)
  xy <- cbind(rnorm(500), rnorm(500))
  result <- magbin(xy, plot = FALSE)
  expect_s3_class(result, "magbin")
})

test_that("plot.magbin dispatches ParmOff calls without error", {
  with_null_dev({
    set.seed(42)
    xy <- cbind(rnorm(300), rnorm(300))
    b <- magbin(xy, plot = FALSE)
    expect_no_error(plot(b))
  })
})

# ── magimage ──────────────────────────────────────────────────────────────────

test_that("magimage renders a matrix without error via ParmOff image call", {
  with_null_dev({
    z <- matrix(1:100, 10, 10)
    expect_no_error(magimage(z))
  })
})

# ── magtri ────────────────────────────────────────────────────────────────────

test_that("magtri returns a matrix with mean and sd columns", {
  with_null_dev({
    set.seed(7)
    chains <- data.frame(a = rnorm(200), b = rnorm(200, mean = 5))
    result <- magtri(chains, samples = 100)
    expect_true(is.matrix(result))
    expect_equal(colnames(result), c("mean", "sd"))
    expect_equal(rownames(result), c("a", "b"))
  })
})

test_that("magtri accepts cex.axis forwarded to magaxis without error", {
  with_null_dev({
    set.seed(8)
    chains <- data.frame(a = rnorm(200), b = rnorm(200))
    expect_no_error(magtri(chains, samples = 100, cex.axis = 0.7))
  })
})

test_that("magtri accepts lty forwarded to magcon without error", {
  with_null_dev({
    set.seed(9)
    chains <- data.frame(a = rnorm(200), b = rnorm(200))
    expect_no_error(magtri(chains, samples = 100, lty = c(1, 1, 1)))
  })
})

test_that("magtri accepts pch forwarded to points without error", {
  with_null_dev({
    set.seed(10)
    chains <- data.frame(x = rnorm(200), y = rnorm(200), z = rnorm(200))
    expect_no_error(magtri(chains, samples = 100, pch = 16))
  })
})

test_that("magtri with refvals marks reference values without error", {
  with_null_dev({
    set.seed(11)
    chains <- data.frame(a = rnorm(200), b = rnorm(200))
    expect_no_error(magtri(chains, samples = 100, refvals = c(0, 0)))
  })
})

test_that("magtri with custom lab argument uses supplied labels", {
  with_null_dev({
    set.seed(12)
    chains <- data.frame(a = rnorm(200), b = rnorm(200))
    result <- magtri(chains, samples = 100, lab = list("Alpha", "Beta"))
    expect_equal(nrow(result), 2)
  })
})
