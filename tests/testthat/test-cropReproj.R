test_that("cropReproj works correctly", {
  library(magrittr)
  library(raster)
  on.exit({
    detach("package:magrittr")
    detach("package:raster")
  }, add = TRUE)

  f <- system.file("external/test.grd", package = "raster")
  r <- raster(f)
  s <- stack(r, r * 2, r * 3)

  ext <- extent(c(xmin = 179000, xmax = 181000, ymin = 330000, ymax = 333000))
  ext.sp <- as(ext, "SpatialPolygons")
  proj4string(ext.sp) <- proj4string(r)
  ext.spdf <- SpatialPolygonsDataFrame(ext.sp, data.frame(X = NA))

  prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  ext.prj <- spTransform(ext.sp, CRS(prj))
  ext.prj.spdf <- spTransform(ext.spdf, CRS(prj))

  ## RasterLayer,RasterLayer
  sa.rast <- projectRaster(r, crs = CRS(prj), method = "ngb") %>% crop(ext.prj)
  rc1 <- cropReproj(r, sa.rast, layerNames = "test")

  expect_true(is(rc1, "RasterStack"))
  expect_equivalent(stack(sa.rast), rc1)
  expect_equivalent(sa.rast, unstack(rc1)[[1]])

  # visual inspection
  if (interactive()) {
    plot(rc1)
    plot(ext.prj, add = TRUE)

    plot(crop(r, ext))
    plot(ext, add = TRUE)

    plot(sa.rast)
    plot(ext.prj, add = TRUE)
  }

  ## RasterLayer,SpatialPolygonsDataFrame
  sa.spdf <- projectRaster(r, crs = CRS(prj), method = "ngb") %>% crop(ext.prj.spdf)
  rc2 <- cropReproj(r, sa.spdf, layerNames = "test")
  expect_true(is(rc2, "RasterStack"))
  expect_equivalent(stack(sa.spdf), rc2)

  ## RasterStack,RasterLayer
  sln <- c("one", "two", "three")
  sc1 <- cropReproj(s, sa.rast, layerNames = sln)
  expect_true(is(sc1, "RasterStack"))

  sa.stck <- stack(sa.rast, sa.rast * 2, sa.rast * 3) %>% set_names(sln)
  expect_equivalent(sa.stck, sc1)

  ## RasterStack,SpatialPolygonsDataFrame
  sc2 <- cropReproj(s, sa.spdf, layerNames = sln)
  expect_true(is(sc2, "RasterStack"))
  expect_equivalent(sa.stck, sc2)

  ## compare raster vs spdf outputs
  expect_equivalent(rc1, rc2)
  expect_equivalent(sc1, sc2)
})
