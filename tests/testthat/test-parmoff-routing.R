test_that("internal ParmOff helpers split prefixed arguments", {
  dots = list(
    "magaxis.cex.axis" = 2,
    "magaxis.cex.lab" = 1.5,
    "magcon.lty" = 3,
    pch = 16
  )
  split = magicaxis:::.mag_split_args(dots, prefix = "magaxis")
  expect_equal(split$args$cex.axis, 2)
  expect_equal(split$args$cex.lab, 1.5)
  expect_equal(names(split$rest), c("magcon.lty", "pch"))
})

test_that("magplot routes nested arguments with ParmOff", {
  calls = new.env(parent = emptyenv())
  calls$magmap = list()
  local_mocked_bindings(
    magmap = function(...) {
      args = list(...)
      calls$magmap = c(calls$magmap, list(args))
      list(map = seq_along(args$data), datalim = range(args$data))
    },
    magbar = function(...) {
      calls$magbar = list(...)
      invisible(NULL)
    },
    points = function(...) {
      calls$points = list(...)
      invisible(NULL)
    },
    plot = function(...) invisible(NULL),
    magaxis = function(...) invisible(NULL),
    .package = "magicaxis"
  )
  magplot(1:3, 4:6, z = 7:9, magmap.locut = 0.25, position = "bottomleft", points.pch = 3, cex = 2)
  expect_equal(calls$magmap[[1]]$locut, 0.25)
  expect_equal(calls$magbar$position, "bottomleft")
  expect_equal(calls$points$pch, 3)
  expect_equal(calls$points$cex, 2)
})

test_that("magtri routes prefixed arguments to magaxis, magcon and points", {
  calls = new.env(parent = emptyenv())
  calls$magaxis = list()
  calls$magcon = list()
  calls$points = list()
  local_mocked_bindings(
    magaxis = function(...) {
      calls$magaxis = c(calls$magaxis, list(list(...)))
      invisible(NULL)
    },
    magcon = function(...) {
      calls$magcon = c(calls$magcon, list(list(...)))
      invisible(NULL)
    },
    points = function(...) {
      calls$points = c(calls$points, list(list(...)))
      invisible(NULL)
    },
    plot = function(...) invisible(NULL),
    density = function(...) structure(list(x = 1:2, y = c(1, 1)), class = "density"),
    layout = function(...) invisible(NULL),
    par = function(...) invisible(NULL),
    plot.window = function(...) invisible(NULL),
    plot.new = function(...) invisible(NULL),
    box = function(...) invisible(NULL),
    abline = function(...) invisible(NULL),
    .package = "magicaxis"
  )
  output = magtri(cbind(a = 1:6, b = 2:7), samples = 4, magaxis.cex.axis = 2, magcon.lty = 5, points.pch = 16)
  expect_equal(dim(output), c(2, 2))
  expect_true(any(vapply(calls$magaxis, function(x) identical(x$cex.axis, 2), logical(1))))
  expect_true(any(vapply(calls$magcon, function(x) identical(x$lty, 5), logical(1))))
  expect_true(all(vapply(calls$points, function(x) identical(x$pch, 16), logical(1))))
})
