# AGENTS.md

Guidance for AI agents and contributors working on `magicaxis` (R package for base-graphics scientific plotting with minor ticks and log axes).

## Layout
- `/R` one file per main function (`magaxis.R`, `magplot.R`, `magimage.R`, `magmap.R`, ...). `NAMESPACE` uses `exportPattern("^[[:alpha:]]+")`, so any function whose name starts with a letter is exported; prefix internal helpers with `.`.
- `/man` hand-written Rd files (no roxygen) — update the matching `.Rd` when changing arguments.
- `/tests/testthat` testthat (edition 3) tests; `/vignettes` knitr vignettes; `/data`, `/inst` supporting data.
- Extra `...` arguments are routed to base graphics via `ParmOff` (a GitHub-only dependency, `asgr/ParmOff`, see `Remotes` in `DESCRIPTION`).

## Commands
```r
devtools::load_all(); devtools::test()      # run tests
devtools::check()                            # or R CMD check
covr::package_coverage()                     # coverage
```

## Testing conventions
- Tests that draw must open a null device (`pdf(nullfile())` + `on.exit(dev.off())`), as in the `with_null_dev` helper in `tests/testthat/test-functions.R`; never rely on an interactive device.
- Prefer asserting on return values/structure; for pure plotting use `expect_no_error`.
- Add a test with any bug fix or new argument.

## CI
- `.github/workflows/R-CMD-check.yaml`: R CMD check on Windows/macOS/Ubuntu.
- `.github/workflows/test-coverage.yaml`: covr coverage on Ubuntu.

## Style
Keep the existing terse base-R style; do not reformat unrelated code. Bump `Version`/`Date` in `DESCRIPTION` for releases.
