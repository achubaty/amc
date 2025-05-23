% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/symlinks.R
\name{flink}
\alias{flink}
\title{Create a link to a file}
\usage{
flink(from, to, symlink = TRUE)
}
\arguments{
\item{from, to}{character vectors, containing file names or paths (can alternatively
be the path to a single existing directory).}

\item{symlink}{Logical indicating whether to use symlink (instead of hardlink).
Default \code{TRUE}.}
}
\description{
Creates a symbolic link (symlink) to a file if possible, possibly falling back to a hard link.
Hard links are for files only, and won't work across different physical drives.
Symlinks won't work on Windows without admin privileges.
}
\note{
Use caution with files-backed objects (e.g., rasters). See examples.
}
\examples{
if (require("datasets", quietly = TRUE)) {
  library(raster)

  tmpDir <- file.path(tempdir(), 'symlink-test') |>
    normalizePath(winslash = '/', mustWork = FALSE)
  dir.create(tmpDir)

  f0 <- file.path(tmpDir, "file0.csv")
  write.csv(iris, f0)

  d1 <- file.path(tmpDir, "dir1")
  dir.create(d1)
  write.csv(iris, file.path(d1, "file1.csv"))

  d2 <- file.path(tmpDir, "dir2")
  f2 <- file.path(tmpDir, "file2.csv")

  ## create a link to the the directory; d2 should look like d1
  flink(d1, d2)  ## symlink
  dir.exists(d2) ## TRUE
  identical(d1, Sys.readlink(d2)) ## TRUE
  file.exists(file.path(d2, "file1.csv")) ## TRUE

  ## create link to a file
  flink(f0, f2) ## symlink
  file.exists(f2) ## TRUE
  identical(read.csv(f0), read.csv(f2)) ## TRUE

  ## deleting the link shouldn't delete the original file
  unlink(d2, recursive = TRUE)
  file.exists(file.path(d2, "file1.csv")) ## FALSE
  file.exists(file.path(d1, "file1.csv")) ## TRUE

  unlink(f2)
  file.exists(f2) ## FALSE
  file.exists(f0) ## TRUE

  ## using rasters and other file-backed objects
  f3 <- system.file("external/test.grd", package = "raster")
  r3 <- raster(f3)
  f4 <- file.path(tmpDir, "raster4.grd")
  flink(f3, f4, FALSE) ## hardlink the grd and gri files
  flink(extension(f3, "gri"), extension(f4, "gri"), FALSE)

  file.exists(f4) ## TRUE
  file.exists(extension(f4, "gri")) ## TRUE
  r4 <- raster(f4) ## hardlink

  f5 <- file.path(tmpDir, "raster5.grd")
  flink(f3, f5, TRUE) ## symlink the grd and gri files
  flink(extension(f3, "gri"), extension(f5, "gri"), TRUE)
  file.exists(f5) ## TRUE
  file.exists(extension(f5, "gri")) ## TRUE
  r5 <- raster(f5) ## symlink works
  identical(r3, r5) ## TRUE

  ## cleanup
  unlink(tmpDir, recursive = TRUE)
}
}
\seealso{
\code{\link[=file.link]{file.link()}}, \code{\link[=file.symlink]{file.symlink()}}, \code{\link[=Sys.readlink]{Sys.readlink()}}
}
\author{
Alex Chubaty
}
