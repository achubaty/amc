test_that("faster-rasters functions produce correct results", {
  Sr1 <- sp::Polygon(cbind(c(2, 4, 4, 0.9, 2), c(2, 3, 5, 4, 2)))
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

  # character
  expect_false(identical(shpRas1_character, shpRas2_character)) # because no label
  expect_equivalent(shpRas1_character, shpRas2_character)

  shpRas2b_character <- raster(extent(shpRas2_character), res=res(shpRas2_character), vals=0)
  shpRas2b_character[] <- shpRas2_character[]
  expect_true(identical(shpRas1_character, shpRas2b_character)) # because no label

  #numeric
  expect_false(identical(shpRas1_numeric, shpRas2_numeric)) # because of "layer" label in rasterize
  shpRas2_numeric@data@names <- ""
  dataType(shpRas2_numeric) <- "FLT4S"
  shpRas2_numeric@file@nodatavalue <- -Inf
  expect_identical(shpRas1_numeric, shpRas2_numeric)

  expect_equivalent(shpRas1_multiF, shpRas2_multiF)

  expect_equivalent(shpRas1_missingF, shpRas2_missingF)


  ### USING GDAL
  shpRas2_character <- fastRasterize(shp, origStack, field = "other", useGdal = TRUE)
  shpRas2_numeric <- fastRasterize(shp, origStack, field = "vals", useGdal = TRUE)
  shpRas2_multiF <- fastRasterize(shp, origStack, field = c("other", "vals"), useGdal = TRUE)
  shpRas2_missingF <- fastRasterize(shp, origStack, useGdal = TRUE)

  # character
  expect_false(identical(shpRas1_character, shpRas2_character)) # because no label
  expect_equivalent(shpRas1_character, shpRas2_character)

  shpRas2b_character <- raster(extent(shpRas2_character), res=res(shpRas2_character), vals=0)
  shpRas2b_character[] <- shpRas2_character[]
  expect_true(identical(shpRas1_character, shpRas2b_character)) # because no label

  #numeric
  expect_false(identical(shpRas1_numeric, shpRas2_numeric)) # because of "layer" label in rasterize
  shpRas2_numeric@data@names <- ""
  dataType(shpRas2_numeric) <- "FLT4S"
  shpRas2_numeric@file@nodatavalue <- -Inf
  shpRas2_numeric@file@blockcols <- 0L
  shpRas2_numeric@file@blockrows <- 0L
  expect_identical(shpRas1_numeric, shpRas2_numeric)

  expect_equivalent(shpRas1_multiF, shpRas2_multiF)

  expect_equivalent(shpRas1_missingF, shpRas2_missingF)

  # gdal_rasterize and velox have different rounding compared to rasterize
  #  when polygon splits a raster perfectly in 2 -- there is one difference here in Sr1 1 instead of 0.9
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

  #shp@polygons[[1]]@Polygons[[1]]@coords[2,1] <- 1
  shpRas1_numeric <- raster::rasterize(shp, origStack, field = "vals")
  shpRas2_numeric <- fastRasterize(shp, origStack, field = "vals")

  expect_false(identical(shpRas1_numeric[], shpRas2_numeric[]))



  ## mask
  newStack1 <- raster::mask(origStack, mask = shp)
  newStack2 <- fastMask(stack = origStack, polygon = shp)
  expect_identical(newStack1, newStack2)
})
