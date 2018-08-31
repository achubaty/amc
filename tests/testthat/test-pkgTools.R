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

  deps <- get_deps(file.path(tmpdir, "doParallel")) # default dependencies = NA
  expect_identical(deps, c("foreach", "iterators", "parallel", "utils"))

  deps <- get_deps(file.path(tmpdir, "doParallel"), dependencies = TRUE)
  expect_identical(deps,
                   c("caret", "foreach", "iterators", "mlbench", "parallel", "rpart", "utils"))

  ## R >= 3.5.0 no longer throws warnings
  if (numeric_version(paste0(R.version$major, ".", R.version$minor)) < "3.5.0") {
    expect_warning(get_deps(file.path(tmpdir, "doParallel"), dependencies = NULL))
  } else {
    expect_identical(get_deps(file.path(tmpdir, "doParallel"), dependencies = NULL), character(0))
  }
  expect_error(get_deps(file.path(tmpdir, "doParallel"), dependencies = "turkey"))

  ## BH doesn't have any deps
  res <- download.packages("BH", destdir = tmpdir, repos = "https://cran.rstudio.com")
  untar(res[, 2], exdir = tmpdir) ## this takes a while...

  deps <- get_deps(file.path(tmpdir, "BH")) # default dependencies = NA
  expect_identical(deps, character(0))

  deps <- get_deps(file.path(tmpdir, "BH"), dependencies = TRUE)
  expect_identical(deps, character(0))

  allDeps <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  deps <- get_deps(file.path(tmpdir, "BH"), dependencies = allDeps)
  expect_identical(deps, character(0))
})
