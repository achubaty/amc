#' Check system memory
#'
#' This tells you the **TOTAL** system memory (RAM) available.
#' Other processes running on the computer will eat into this total,
#' and as such, you should take these numbers with a grain of salt.
#'
#' @param  x  Units to use for output. One of either \code{"KB"}, \code{"MB"}, \code{"GB"}.
#'
#' @return Total amount of system memory (RAM) in \code{units}.
#'
#' @author Alex Chubaty
#' @docType methods
#' @export
#' @importFrom utils memory.limit
#' @rdname sysmem
#'
#' @examples
#' sysmem()
#'
sysmem <- function(x = "GB") {
  x <- toupper(as.character(x))
  allowed <- c("KB", "MB", "GB")
  if (!(x %in% allowed)) {
    last <- length(allowed)
    err.msg <- paste("specify", paste(allowed[-last], collapse = ", "), "or", allowed[last], sep = " ")
    stop(err.msg)
  }

  # check OS and determine total RAM
  OS <- Sys.info()[["sysname"]]
  if (OS == "Darwin") {
    mem <- system("sysctl hw.memsize", intern = TRUE)
    mem <- as.numeric(strsplit(mem, " ")[[1]][2])
    ram.kb <- mem / 1024
    ram.mb <- floor(ram.kb / 1024)
    ram.gb <- floor(ram.mb / 1024)
  } else if (OS == "Linux") {
    mem <- system("grep MemTotal /proc/meminfo", intern = TRUE)
    mem <- strsplit(mem, " ")
    mem <- mem[[1]][which(mem[[1]] != "")]
    ram.kb <- as.numeric(mem[2])
    ram.mb <- floor(ram.kb / 1024)
    ram.gb <- floor(ram.mb / 1024)
  } else if (OS == "Windows") {
    ram.mb <- memory.limit() # total RAM in MB
    ram.gb <- floor(ram.mb / 1024)
    ram.kb <- floor(ram.mb * 1024)
  } else {
    stop("Unable to determine total RAM.")
  }

  switch(x,
         "KB" = ram.kb,
         "MB" = ram.mb,
         "GB" = ram.gb,
         NA_character_
  )
}

#' Guesstimate the number of CPUs for cluster operations
#'
#' Take a wild stab at guessing how many CPUs to use in cluster when you have
#' some idea of how much RAM is needed per CPU.
#'
#' Tries to be conservative by assuming no more than 80% system memory use.
#'
#' @note You should take these numbers with several grains of salt.
#'
#' @param ram  How much ram is required per CPU.
#'
#' @param prop  Proportion of overall RAM to devote to R. Default \code{0.80}.
#'
#' @param units Units of memory. One of either \code{"KB"}, \code{"MB"}, \code{"GB"}.
#'
#' @return Integer. Number of CPUs to allocate to cluster.
#'
#' @author Alex Chubaty
#' @docType methods
#' @export
#' @rdname guesstimate
#'
#' @examples
#' \dontrun{
#' guesstimate(4)
#' guesstimate(4, 0.90, "MB")
#' }
#'
guesstimate <- function(ram, prop = 0.80, units = "gb") {
  if (ram >= sysmem(units)) {
    stop("Not enough system memory.")
  } else {
    if (ram >= prop * sysmem(units)) {
      warning("High RAM requirement for this system!", immediate. = TRUE)
    }
    cpus <- floor(prop * sysmem(units) / ram)
    if (cpus < 1) cpus <- 1
    return(as.integer(cpus))
  }
}

#' Manual garbage collection
#'
#' This shouldn't be necessary, since R (usually) handles this correctly and
#' automatically. However, sometimes when working with large geospatial data
#' (using \code{raster} and \code{sp} packages) it can help to free recently
#' unallocated memory manually.
#'
#' @author Alex Chubaty
#' @docType methods
#' @export
#' @rdname gc
#' @seealso \code{\link{gc}}
#'
.gc <- function() {
  for (i in 1:10) gc()
}
