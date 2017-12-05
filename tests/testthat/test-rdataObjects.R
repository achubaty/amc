test_that("saving and load objects works correctly", {
  tmpdir <- file.path(tempdir(), "test-save_load_objects")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  expect_true(dir.exists(tmpdir))

  testfile.grd <- system.file("external/test.grd", package = "raster") # nolint
  testfile.gri <- raster::extension(testfile.grd, ".gri") # nolint
  f.grd <- file.path(tmpdir, "test.grd") # nolint
  f.gri <- raster::extension(f.grd, ".gri") # nolint
  expect_true(file.copy(testfile.grd, f.grd))
  expect_true(file.exists(f.grd))
  expect_true(file.copy(testfile.gri, f.gri))
  expect_true(file.exists(f.gri))

  a <- letters
  b <- 1:26 * pi
  d <- data.frame(A = a, B = b)
  r <- raster::raster(f.grd)
  objNames <- c("a", "b", "d", "r")

  ## saving
  saveObjects(objNames, tmpdir)
  expected_files <- c(paste0(objNames, ".RData"), basename(c(f.grd, f.gri))) # nolint
  expect_identical(list.files(tmpdir), expected_files)

  ## loading
  loadObjects(objNames, tmpdir)
  expect_true(all(vapply(objNames, exists, logical(1), envir = environment())))

  ## removing
  rm(a, b, d, r)
  rmObjects(objNames, tmpdir)
  expect_identical(list.files(tmpdir), basename(c(f.grd, f.gri)))
})
