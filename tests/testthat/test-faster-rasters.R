test_that("faster-rasters functions produce correct results", {
  if (require("SpaDES", quietly = TRUE)) {
    r <- raster::raster(raster::extent(0, 15, 0, 15), vals = 0)
    poly <- list()
    poly[[1]] <- SpaDES::randomPolygons(r, numTypes = 10)
    poly[[2]] <- SpaDES::randomPolygons(r, numTypes = 10)
    origStack <- raster::stack(poly)

    shp <- origStack[[1]]
    shp[shp == shp[1]] <- NA # take a chunk out of the raster
    shp <- raster::rasterToPolygons(shp, dissolve = TRUE) # convert to polygon

    # original mask function in raster
    newStack1 <- raster::mask(origStack, mask = shp)

    # fastMask uses 2 clusters
    newStack2 <- fastMask(stack = origStack, polygon = shp)

    # test all equal
    expect_identical(newStack1, newStack2)
    newStack1 <- raster::stack(newStack1)
    newStack2 <- raster::stack(newStack2)

    ## rasterize
    shpRas1 <- raster::rasterize(shp, origStack)
    shpRas2 <- fastRasterize(shp, origStack)
    expect_identical(shpRas1, shpRas2)
  }
})
