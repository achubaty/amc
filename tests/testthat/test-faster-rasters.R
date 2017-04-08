test_that("faster-rasters functions produce correct results", {
  Sr1 <- sp::Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
  Sr2 <- sp::Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
  Sr3 <- sp::Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))

  Srs1 <- sp::Polygons(list(Sr1), "s1")
  Srs2 <- sp::Polygons(list(Sr2), "s2")
  Srs3 <- sp::Polygons(list(Sr3), "s3")
  shp <- sp::SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
  d <- data.frame(vals = 1:3, other = letters[1:3])
  row.names(d) <- names(shp)
  shp <- sp::SpatialPolygonsDataFrame(shp, data = d)
  poly <- list()
  poly[[1]] <- raster::raster(raster::extent(shp), vals = 0, res = c(0.5, 0.5))
  poly[[2]] <- raster::raster(raster::extent(shp), vals = 1, res = c(0.5, 0.5))
  origStack <- raster::stack(poly)

  ## rasterize
  shpRas1_character <- raster::rasterize(shp, origStack, field = "other")
  shpRas1_numeric <- raster::rasterize(shp, origStack, field = "vals")
  shpRas1_multiF <- raster::rasterize(shp, origStack, field = c("other", "vals"))
  shpRas1_missingF <- raster::rasterize(shp, origStack)

  shpRas2_character <- fastRasterize(shp, origStack, field = "other")
  shpRas2_numeric <- fastRasterize(shp, origStack, field = "vals")
  shpRas2_multiF <- fastRasterize(shp, origStack, field = c("other", "vals"))
  shpRas2_missingF <- fastRasterize(shp, origStack)

  testthat::expect_equal(shpRas1_character, shpRas2_character)
  testthat::expect_equal(shpRas1_numeric, shpRas2_numeric)
  testthat::expect_equal(shpRas1_multiF, shpRas2_multiF)
  testthat::expect_equal(shpRas1_missingF, shpRas2_missingF)

  ## mask
  newStack1 <- raster::mask(origStack, mask = shp)
  newStack2 <- fastMask(stack = origStack, polygon = shp)
  expect_identical(newStack1, newStack2)
})
