#' Load a study area from file
#'
#' Simple wrapper around \code{readOGR} to load a kml or shapefile.
#'
#' @param path path to directory containing the file
#' @param filename the name of the file.
#' @param proj (optional) a crs projection string to reproject the study area to.
#'
#' @return A \code{SpatialPolygonsDataFrame}
#'
#' @export
#' @importFrom raster shapefile
#' @importFrom rgdal readOGR writeOGR
#' @importFrom sp spTransform
#' @importFrom tools file_path_sans_ext
loadStudyArea <- function(path = NULL, filename = NULL, proj = NULL) {
  if (is.null(path) | is.null(filename) | !file.exists(file.path(path, filename)))
    stop("'filename' and 'path' must be provided and must exist.")

  fullPath <- file.path(path, filename)

  if (tools::file_ext(fullPath) == "kml") {
    ## convert to shapefile (so raster doesn't have a fit)
    studyArea <- rgdal::readOGR(fullPath)
    writeOGR(studyArea, path, driver = "ESRI Shapefile", overwrite_layer = TRUE,
             layer = tools::file_path_sans_ext(filename))
    shpfile <- fullPath
    raster::extension(shpfile) <- "shp"
    studyArea <- raster::shapefile(shpfile)
  }

  if (!is.null(proj))
    studyArea <- sp::spTransform(studyArea, proj)

  return(studyArea)
}
