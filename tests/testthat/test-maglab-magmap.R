## Tests for maglab and magmap (pure computation, no plotting).

library(magicaxis)

test_that("maglab returns tickat, labat and exp", {
  out <- maglab(c(0, 10))
  expect_named(out, c("tickat", "labat", "exp"))
  expect_true(all(is.finite(out$labat)))
  expect_equal(out$tickat, out$labat)
})

test_that("maglab trim=TRUE keeps labels within limits", {
  out <- maglab(c(0.5, 9.5), trim = TRUE)
  expect_true(all(out$labat >= 0.5 & out$labat <= 9.5))
})

test_that("maglab log=TRUE gives power-of-ten label locations", {
  out <- maglab(c(1, 1000), log = TRUE)
  expect_true(all(abs(log10(out$labat) - round(log10(out$labat))) < 1e-8))
  expect_true(min(out$labat) <= 1 && max(out$labat) >= 1000)
})

test_that("maglab usemultloc requires log=TRUE", {
  expect_error(maglab(c(1, 100), usemultloc = TRUE, log = FALSE))
})

test_that("magmap output lies within the requested range", {
  out <- magmap(seq(1, 100), range = c(0, 1))
  expect_true(all(out$map >= 0 & out$map <= 1))
  expect_length(out$map, 100)
})

test_that("magmap flip=TRUE reverses a linear mapping", {
  x <- seq(1, 10)
  a <- magmap(x, type = "num", locut = 1, hicut = 10, range = c(0, 1))$map
  b <- magmap(x, type = "num", locut = 1, hicut = 10, range = c(0, 1), flip = TRUE)$map
  expect_equal(a, 1 - b)
})
