#' Check system memory
#'
#' This tells you the **TOTAL** system memory (RAM) available.
#' Other processes running on the computer will eat into this total,
#' and as such, you should take these numbers with a grain of salt.
#'
#' @param  x  Units to use for output. One of either `"KB"`, `"MB"`, `"GB"`.
#'
#' @return Total amount of system memory (RAM) in `units`.
#'
#' @author Alex Chubaty
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
    errmsg <- paste("specify", paste(allowed[-last], collapse = ", "), "or",
                    allowed[last], sep = " ")
    stop(errmsg)
  }

  # check OS and determine total RAM
  os <- Sys.info()[["sysname"]]
  if (os == "Darwin") {
    mem <- system("sysctl hw.memsize", intern = TRUE)
    mem <- as.numeric(strsplit(mem, " ")[[1]][2])
    ramKB <- mem / 1024
    ramMB <- floor(ramKB / 1024)
    ramGB <- floor(ramMB / 1024)
  } else if (os == "Linux") {
    mem <- system("grep MemTotal /proc/meminfo", intern = TRUE)
    mem <- strsplit(mem, " ")
    mem <- mem[[1]][which(mem[[1]] != "")]
    ramKB <- as.numeric(mem[2])
    ramMB <- floor(ramKB / 1024)
    ramGB <- floor(ramMB / 1024)
  } else if (os == "Windows") {
    ramMB <- memory.limit() # total RAM in MB
    ramGB <- floor(ramMB / 1024)
    ramKB <- floor(ramMB * 1024)
  } else {
    stop("Unable to determine total RAM.")
  }

  switch(x,
         "KB" = ramKB,
         "MB" = ramMB,
         "GB" = ramGB,
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
#' @param prop  Proportion of overall RAM to devote to R. Default `0.80`.
#'
#' @param units Units of memory. One of either `"KB"`, `"MB"`, `"GB"`.
#'
#' @return Integer. Number of CPUs to allocate to cluster.
#'
#' @author Alex Chubaty
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
#' (e.g., using `raster` and `sp` packages) it can help to free recently
#' unallocated memory manually.
#'
#' @author Alex Chubaty
#' @export
#' @rdname gc
#' @seealso [gc()]
.gc <- function() {
  for (i in 1:10) gc()
}
