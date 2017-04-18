test_that("dataPrepRast produces correct output", {
  library(raster)
  library(maptools)
  library(sp)

  on.exit({
    detach("package:raster")
    detach("package:maptools")
    detach("package:sp")
  }, add = TRUE)

  data("wrld_simpl")

  # From scratch, create raster
  xy <- matrix(rnorm(2500), 50, 50)
  inputrast <- raster(xy)
  extent(inputrast) <- c(-170, -15, 35, 100) # USA-Canada extent
  projection(inputrast) <- CRS("+proj=longlat +datum=WGS84")

  # Extract Canada boundary (from wrld_simpl)
  canada <- wrld_simpl[which(wrld_simpl@data$NAME == "Canada"),]

  ## Point to test output raster
  # Create point in USA
  coordsUSA <- data.frame(x = -100, y = 40)
  ptUSA <- SpatialPoints(coordsUSA, proj4string = CRS(projection(inputrast)))
  # Create point in Hudson Bay
  coordsHudsonBay <- data.frame(x = -85, y = 60)
  ptHB <- SpatialPoints(coordsHudsonBay, proj4string = CRS(projection(inputrast)))
  # Create point in Northwest Territories
  coordsNWT <- data.frame(x = -100, y = 65)
  ptNWT <- SpatialPoints(coordsNWT, proj4string = CRS(projection(inputrast)))

  # TEST output
  x <- dataPrepRast(inputrast, crop = canada, mask = canada,
                    reprojectTo = inputrast, timings = FALSE)

  # Expect output class to be RasterLayer
  expect_true(is(x, "RasterLayer"))

  ##TEST CROP
  # RasterLayer, SpatialPolygonsDataFrame
  x <- dataPrepRast(inputrast, crop = canada, mask = NULL,
                    reprojectTo = inputrast,  timings = FALSE)

  # Expect USA point to intersect input raster (USA-Canada)
  valueUSA <- extract(inputrast,ptUSA)
  expect_false(is.na(valueUSA))

  # Expect USA point to be NA on output raster (Canada extent)
  expect_true(is.na(extract(x, ptUSA)))

  # Expect Canada point to intersect output raster (crop = Canada, mask = NULL)
  expect_false(is.na(extract(x, ptNWT)))

  # Expect Hudson Bay point to intersect output raster (crop = Canada, mask = NULL)
  expect_false(is.na(extract(x, ptHB)))

  # Compare with manual crop
  canadaproj<-spTransform(canada,crs(inputrast))
  crop2canada<- crop(inputrast,canadaproj, method = "ngb")
  expect_identical(x,crop2canada)

  #RasterLayer, RasterLayer
  canadarast<- raster(canada, crs= crs(canada), vals =1)
  expect_true(is(canadarast,"RasterLayer"))

  x <- dataPrepRast(inputrast, crop = canadarast, mask = NULL,
                    reprojectTo = inputrast,  timings = FALSE)

  crop2canadarast<- crop(inputrast,canadarast, method = "ngb")
  expect_identical(x,crop2canadarast)

  # RasterLayer, Extent
  canadaExtent<- extent(canada)
  expect_true(is(canadaExtent,"Extent"))

  x <- dataPrepRast(inputrast, crop = canadaExtent, mask = NULL,
                    reprojectTo = inputrast,  timings = FALSE)

  crop2canadaExtent<- crop(inputrast,canadaExtent, method = "ngb")
  expect_identical(x,crop2canadaExtent)

  # RasterLayer, crop= Missing
  x <- dataPrepRast(inputrast, mask = NULL,
                    reprojectTo = inputrast,  timings = FALSE)
  expect_identical(x, inputrast)

  ## TEST MASK
  # RasterLayer, SpatialPolygonsDataFrame
  x <- dataPrepRast(inputrast, crop = canada, mask = canada,
                    reprojectTo = inputrast,  timings = FALSE)

  # Expect Hudson Bay point to be NA on output raster (mask = Canada)
  valueHB <- extract(x, ptHB)
  expect_true(is.na(valueHB))

  # RasterLayer, RasterLayer
  mt<- matrix(c(NA,NA,1,NA,NA,NA,1,1,1,NA,1,1,1,1,1), nrow=3, ncol=5, byrow = TRUE)
  rst<-raster(mt)
  extent(rst) <- c(-110, -90, 50, 70)
  projection(rst) <- CRS("+proj=longlat +datum=WGS84")
  # Create point that intersect NA from mask RasterLayer
  coordsNA <- data.frame(x = -107, y = 68)
  ptNA <- SpatialPoints(coordsNA, proj4string = CRS(projection(x)))

  # Expect point to have NA on mask RasterLayer
  valueNA <- extract(rst, ptNA)
  expect_true(is.na(valueNA))

  # Expect point to have a value when mask is missing
  x <- dataPrepRast(inputrast, crop = canada, reprojectTo = inputrast,
                      timings = FALSE)
  valueNA <- extract(x, ptNA)
  expect_true(!is.na(valueNA))

  #Expect pt to be NA after mask is applied
  x <- dataPrepRast(inputrast, mask = rst, reprojectTo = inputrast,
                      timings = FALSE)
  valueNA <- extract(x, ptNA)
  expect_true(is.na(valueNA))

  # Mask manually and expect identical with output function
  rstproj<-projectRaster(rst, inputrast, method = "ngb")
  mask<-mask(inputrast, rstproj)
  expect_identical(x, mask)


  ## TEST REPROJECT
  # Change canada projection before running dataPrepRast
  canadaTransform <- spTransform(canada, CRS("+init=epsg:3857"))

  # Expect projection to be different
  expect_false(compareCRS(x, canadaTransform))

  # Expect error (input raster and CanadaTransform do not overlap due to spTransform
  expect_error(crop(inputrast, canadaTransform))

  # Run dataPrepRast using input arguments with different projection
  x <- dataPrepRast(inputrast, crop = canada, mask = canada,
                    reprojectTo = inputrast,  timings = FALSE)
  y <- dataPrepRast(inputrast, crop = canadaTransform, mask = canadaTransform,
                    reprojectTo = inputrast,  timings = FALSE)

  # Expect x to be identical to y because dataPrepRast reproject data before processing
  expect_identical(x, y)

  ## TEST ERROR WHEN CROP/MASK DO NOT OVERLAP
  # Extract Mexico boundary to test non-overlapping crop and mask
  mexico <- wrld_simpl[which(wrld_simpl@data$NAME == "Mexico"),]

  # Expect error if crop/mask layer do not overlap input raster
  expect_error(dataPrepRast(inputrast, crop = mexico, mask = mexico,
                            reprojectTo = inputrast, timings = FALSE))

})

