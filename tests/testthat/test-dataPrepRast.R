test_that("test-dataPrepRast", {
  library(raster)
  library(maptools)
  library(sp)

  on.exit({
    detach("package:raster")
    detach("package:maptools")
    detach("package:sp")
  }, add = TRUE)

  data("wrld_simpl")

  #From scratch, random raster
  xy <- matrix(rnorm(2500),50,50)
  inputrast <- raster(xy)
  #Set input raster as USA-Canada extent and set projection
  extent(inputrast) <- c(-170,-15,35,100)
  projection(inputrast) <- CRS("+proj=longlat +datum=WGS84")
  #Extract Canada boundary from wrld_simpl
  canada<-wrld_simpl[which(wrld_simpl@data$NAME =='Canada'),]

  #dataPrepRast
  x<-dataPrepRast(x=inputrast, cropWith=canada, maskWith= canada,
                  reprojectTo= inputrast, timings = TRUE)

  # Expect output class to be RasterLayer
  expect_true(is(x,'RasterLayer'))

  ## Point to test output raster
  # Create point in USA
  coordsUSA <-data.frame(x=-100, y=40)
  ptUSA<-SpatialPoints(coordsUSA, proj4string=CRS(projection(x)))
  ## Create point in Hudson Bay
  coordsHudsonBay <-data.frame(x=-85, y=60)
  ptHB<-SpatialPoints(coordsHudsonBay, proj4string=CRS(projection(x)))
  ## Create point in Canada (Northwest Territory)
  coordsNWT <-data.frame(x=-100, y=65)
  ptNWT<-SpatialPoints(coordsNWT, proj4string=CRS(projection(x)))

  ##TEST CROP
  x<-dataPrepRast(x=inputrast, cropWith=canada, maskWith= NULL,
                  reprojectTo= inputrast,  timings = TRUE)

  # Expect USA point to intersect data on input raster (USA-Canada)
  valueUSA<-extract(inputrast,ptUSA)
  expect_false(is.na(valueUSA))
  # Expect USA point to be NA on output raster (Canada extent)
  valueUSA2<-extract(x,ptUSA)
  expect_true(is.na(valueUSA2))

  # Expect Canada point to intersect data on output raster (crop = Canada, mask = NULL)
  valueNWT<-extract(x,ptNWT)
  expect_false(is.na(valueNWT))

  # Expect Hudson Bay point to intersect data on output raster (crop = Canada, mask = NULL)
  valueHB<-extract(x,ptHB)
  expect_false(is.na(valueHB))

  ## TEST MASK
  x<-dataPrepRast(x=inputrast, cropWith=canada, maskWith= canada,
                  reprojectTo= inputrast,  timings = TRUE)

  # Expect Hudson Bay point to be NA on output raster (mask = Canada)
  valueHB<-extract(x,ptHB)
  expect_true(is.na(valueHB))


  ## TEST REPROJECT
  # Change canada projection before running dataPrepRast
  canadaTransform<-spTransform(canada, CRS("+init=epsg:3857"))

  # Expect projection to be different
  expect_false(compareCRS(x,canadaTransform))

  # Expect error because input raster and CanadaTransform do not overlap due to spTransform
  expect_error(crop(x, canadaTransform))

  # Run dataPrepRast using input arguments with different projection
  x<-dataPrepRast(x=inputrast, cropWith=canada, maskWith= canada,
                  reprojectTo= inputrast,  timings = TRUE)
  y<-dataPrepRast(x=inputrast, cropWith=canadaTransform, maskWith= canadaTransform,
                  reprojectTo= inputrast,  timings = TRUE)

  # Expect x to be identical to y because dataPrepRast reproject data before processing
  expect_that(x, is_identical_to(y))

  ## TEST ERROR WHEN CROP/MASK DO NOT OVERLAP
  # Extract Mexico boundary to test non-overlapping crop and mask
  mexico<-wrld_simpl[which(wrld_simpl@data$NAME =='Mexico'),]

  # Expect error if crop/mask layer do not overlap input raster
  expect_error(dataPrepRast(x=inputrast, cropWith=mexico, maskWith= mexico,
                          reprojectTo= inputrast, timings = TRUE))

})
