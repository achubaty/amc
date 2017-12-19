#' Intelligently download data
#'
#' Only downloads the specified files if it is not found locally.
#' Optionally unzips the files.
#'
#' @param urls      A character vector of data file URLs.
#' @param dest      The directory path in which data should be downloaded.
#' @param checksum  Logical indicating whether downloaded files should be checksummed.
#' @param unzip     Logical indicating whether the file should be unzipped after download.
#'
#' @author Alex Chubaty and Eliot Mcintire
#' @export
#' @importFrom digest digest
#' @importFrom utils download.file installed.packages read.table write.table
#' @rdname dl.data
#'
dl.data <- function(urls, dest = ".", checksum = TRUE, unzip = FALSE) { # nolint
  tmp <- lapply(urls, function(f) {
    destFile <- file.path(dest, basename(f))
    csumFile <- file.path(dest, paste0(sub("^([^.]*).*", "\\1", basename(f)), ".checksum"))
    needDownload <- TRUE
    if (file.exists(destFile)) {
      if (checksum) {
        if (file.exists(csumFile)) {
          hash <- digest::digest(file = destFile, algo = "xxhash64")
          hashCheck <- read.table(csumFile, stringsAsFactors = FALSE, header = TRUE)
          if (hash == hashCheck$checksum) {
            message("File ", basename(destFile), " already exists. Skipping download.")
            needDownload <- FALSE
          }
        } else {
          message("No hash file exists. Assuming current file (", basename(f), ") is correct.")
          hash <- digest::digest(file = destFile, algo = "xxhash64")
          write.table(data.frame(filename = destFile, checksum = hash),
                      file = csumFile, row.names = FALSE)
          needDownload <- FALSE
        }
      } else {
        message("File ", basename(destFile), " already exists. Skipping download.")
        needDownload <- FALSE
      }
    }

    if (needDownload)  {
      download.file(f, destFile)
      hash <- digest::digest(file = destFile, algo = "xxhash64")
      write.table(data.frame(filename = destFile, checksum = hash),
                  file = csumFile, row.names = FALSE)

      if (unzip) unzip(destFile, exdir = dest, overwrite = TRUE)
    }
  })
  return(invisible())
}
