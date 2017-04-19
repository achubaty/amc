test_that("get_deps is happy", {
  skip_on_cran()

  library(magrittr)
  on.exit(detach("package:magrittr"))

  tmpdir <- file.path(tempdir(), "test_get_deps")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  ## doParallel doesn't have spaces btw pkg and version
  res <- download.packages("doParallel", destdir = tmpdir, repos = "https://cran.rstudio.com")
  untar(res[, 2], exdir = tmpdir)

  deps <- get_deps(file.path(tmpdir, "doParallel")) ## default dependencies = NA
  expect_identical(deps, c("foreach", "iterators", "parallel", "utils"))

  deps <- get_deps(file.path(tmpdir, "doParallel"), dependencies = TRUE)
  expect_identical(deps, c("caret", "foreach", "iterators", "mlbench", "parallel", "rpart", "utils"))

  expect_warning(get_deps(file.path(tmpdir, "doParallel"), dependencies = NULL))
  expect_error(get_deps(file.path(tmpdir, "doParallel"), dependencies = "turkey"))

  ## BH doesn't have any deps
  res <- download.packages("BH", destdir = tmpdir, repos = "https://cran.rstudio.com")
  untar(res[, 2], exdir = tmpdir) ## this takes a while...

  deps <- get_deps(file.path(tmpdir, "BH")) ## default dependencies = NA
  expect_identical(deps, character(0))

  deps <- get_deps(file.path(tmpdir, "BH"), dependencies = TRUE)
  expect_identical(deps, character(0))

  all_deps <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  deps <- get_deps(file.path(tmpdir, "BH"), dependencies = all_deps)
  expect_identical(deps, character(0))
})
