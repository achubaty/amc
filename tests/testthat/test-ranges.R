test_that("inRange handles various inputs", {
  ## inputs for x
  expect_equal(inRange(0.5, 0, 1),   TRUE)
  expect_equal(inRange(-0.5, 0, 1),  FALSE)
  expect_equal(inRange(NA_real_),    NA)
  expect_equal(inRange(NA_integer_), NA)
  expect_equal(inRange(NULL),        NULL)
  expect_error(inRange())
  expect_error(inRange("non-numeric"), "x must be numeric.")

  f <- system.file("external/test.grd", package = "raster")
  r <- raster::raster(f)
  ids <- which(inRange(r, 850, 875))
  expect_equal(ids, c(1741L, 2774L, 3091L, 3092L, 3171L, 3645L, 3873L, 3878L, 3951L,
                      3952L, 4031L, 7486L, 7646L))

  f <- system.file("ex/test.grd", package = "terra")
  r <- raster::raster(f)
  ids <- which(inRange(r, 850, 875))
  expect_equal(ids, c(1741L, 2774L, 3091L, 3092L, 3171L, 3645L, 3873L, 3878L, 3951L,
                      3952L, 4031L, 7486L, 7646L))

  # inputs for a & b
  expect_error(inRange(0.5, 1, 0))
  expect_error(inRange(-0.5, NA_integer_, 1))
  expect_error(inRange(-0.5, NA_real_, 1))
  expect_error(inRange(-0.5, 0, NA_integer_))
  expect_error(inRange(-0.5, 0, NA_real_))
  expect_error(inRange(-0.5, NULL, 1))
  expect_error(inRange(-0.5, 0, NULL))
})

test_that("rescale works correctly", {
  # rescale a single value
  expect_equal(rescale(50, c(0, 1), c(0, 100)), 0.5)
  expect_equal(rescale(50, c(-1, 1), c(0, 100)), 0)

  # simple rescaling of numeric vectors
  x <- 0:100

  x1 <- rescale(x)
  expect_lt(min(x1), max(x1))
  expect_equal(min(x1), 0)
  expect_equal(max(x1), 1)

  x2 <- rescale(x, c(-1, 1))
  expect_lt(min(x2), max(x2))
  expect_equal(min(x2), -1)
  expect_equal(max(x2), 1)

  ## rescaling values of a RasterLayer
  f <- system.file("external/test.grd", package = "raster")
  r <- raster::raster(f)

  r1 <- rescale(r)
  expect_lt(raster::minValue(r1), raster::maxValue(r1))
  expect_equal(raster::minValue(r1), 0)
  expect_equal(raster::maxValue(r1), 1)

  r2 <- rescale(r, c(-1, 1))
  expect_lt(raster::minValue(r2), raster::maxValue(r2))
  expect_equal(raster::minValue(r2), -1)
  expect_equal(raster::maxValue(r2), 1)

  r[] <- 0
  r3 <- rescale(r)
  expect_equal(r, r3)

  ## rescaling values of a SpatRaster
  f <- system.file("ex/test.grd", package = "terra")
  r <- terra::rast(f)

  r1 <- rescale(r)
  expect_lt(terra::minmax(r1)["min", ], terra::minmax(r1)["max", ])
  expect_equal(terra::minmax(r1)["min", ], 0)
  expect_equal(terra::minmax(r1)["max", ], 1)

  r2 <- rescale(r, c(-1, 1))
  expect_lt(terra::minmax(r2)["min", ], terra::minmax(r2)["max", ])
  expect_equal(terra::minmax(r2)["min", ], -1)
  expect_equal(terra::minmax(r2)["max", ], 1)

  r[] <- 0
  r3 <- rescale(r)
  expect_equal(r, r3)
})
