#' Load kNN stand age map
#'
#' @param path file path where raster will be saved.
#' @param url URL from which to download the data (default provided if NULL).
#' @param studyArea `SpatialPolygonsDataFrame` giving the study area for which to extract ages.
#' @param ... Additional arguments passed to `Cache` (only `userTags` currently used).
#'
#' @export
#' @importFrom reproducible Cache prepInputs
#' @importFrom tools file_path_sans_ext
loadkNNageMap <- function(path, url = NULL, studyArea = NULL, ...) {
  dots <- list(...)

  if (is.null(url)) {
    url <- paste0("http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                  "canada-forests-attributes_attributs-forests-canada/",
                  "2001-attributes_attributs-2001/",
                  "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif")
  }

  standAgeMapFilename <- file.path(path, "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif")
  Cache(prepInputs,
        targetFile = basename(standAgeMapFilename),
        destinationPath = path,
        url = url,
        fun = "raster::raster",
        studyArea = studyArea,
        method = "bilinear",
        datatype = "INT2U",
        filename2 = paste0(tools::file_path_sans_ext(basename(standAgeMapFilename)), "_cropped"),
        overwrite = TRUE,
        userTags = dots$userTags)
}
