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
  shpRas1 <- raster::rasterize(shp, origStack)
  shpRas2 <- fastRasterize(shp, origStack)
  expect_equal(shpRas1, shpRas2)

  ## mask
  newStack1 <- raster::mask(origStack, mask = shp)
  newStack2 <- fastMask(stack = origStack, polygon = shp)
  expect_identical(newStack1, newStack2)
})
