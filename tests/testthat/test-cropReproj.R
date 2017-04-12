test_that("cropReproj works correctly", {

  skip("figure out why order matters with crop-reproj")

  library(magrittr)
  library(raster)
  on.exit({
    detach("package:magrittr")
    detach("package:raster")
  }, add = TRUE)

  f <- system.file("external/test.grd", package = "raster")
  r <- raster(f)

  ext <- extent(c(xmin = 179000, xmax = 181000, ymin = 330000, ymax = 333000))
  ext.sp <- as(ext, "SpatialPolygons")
  proj4string(ext.sp) <- proj4string(r)
  ext.spdf <- SpatialPolygonsDataFrame(ext.sp, data.frame(X = NA))

  prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  ext.prj <- spTransform(ext.sp, CRS(prj))
  ext.prj.spdf <- spTransform(ext.spdf, CRS(prj))

  ## with RasterLayer
  sa.rast1 <- crop(r, ext) %>% projectRaster(crs = CRS(prj), method = "ngb")
  sa.rast2 <- projectRaster(r, crs = CRS(prj), method = "ngb") %>% crop(ext.prj)

  rc1 <- cropReproj(r, sa.rast1, layerNames = "test")
  rc2 <- cropReproj(r, sa.rast2, layerNames = "test")

  expect_equal(rc1, rc2)
  expect_equivalent(rc1, rc2)

  expect_equal(stack(sa.rast), rc1)
  expect_equivalent(stack(sa.rast), rc1)

  expect_equal(stack(sa.rast), rc2)
  expect_equivalent(stack(sa.rast), rc2)

  plot(rc1)
  plot(rc2)
  plot(ext.prj, add = TRUE)

  plot(crop(r, ext))
  plot(ext, add = TRUE)

  plot(sa.rast1)
  plot(ext.prj, add = TRUE)

  plot(sa.rast2)
  plot(ext.prj, add = TRUE)

  ## with SpatialPolygonsDataFrame
  sa.spdf1 <- crop(r, ext.spdf) %>% projectRaster(crs = CRS(prj), method = "ngb")
  sa.spdf2 <- projectRaster(r, crs = CRS(prj), method = "ngb") %>% crop(ext.prj.spdf)

  rc3 <- cropReproj(r, sa.spdf1, layerNames = "test")
  rc4 <- cropReproj(r, sa.spdf2, layerNames = "test")

  expect_equal(stack(sa.spdf1), rc3)
  expect_equal(stack(sa.spdf2), rc4)

  ## compare raster vs spdf outputs
  expect_equal(rc1, rc3)
  expect_equal(rc2, rc4)
})
