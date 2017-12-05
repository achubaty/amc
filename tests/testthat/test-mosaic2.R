test_that("mosaic2 works correctly", {
  library(magrittr)
  on.exit({
    detach("package:magrittr")
  }, add = TRUE)

  f <- system.file("external/test.grd", package = "raster")
  r <- raster::raster(f)

  m <- raster::mosaic(r, r, fun = sum) %>% set_names("test")
  m2 <- mosaic2(r, r, fun = sum)

  expect_equivalent(m, m2)
  expect_equal(sp::proj4string(m), sp::proj4string(2 * r))
  #expect_equal(sp::proj4string(m), sp::proj4string(m2)) ## should be the same!! # nolint

  ## note: NAs get converted to zeroes in `masoic`
  m[which(m[] == 0)] <- NA
  m2[which(m2[] == 0)] <- NA
  expect_equal(m, 2 * r)
  expect_equivalent(m2, 2 * r)
})
