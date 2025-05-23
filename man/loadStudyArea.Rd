% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/studyArea.R
\name{loadStudyArea}
\alias{loadStudyArea}
\title{Load a study area from file}
\usage{
loadStudyArea(path = NULL, filename = NULL, proj = NULL)
}
\arguments{
\item{path}{path to directory containing the file}

\item{filename}{the name of the file}

\item{proj}{(optional) a crs projection string to reproject the study area to.}
}
\value{
An \code{sf} object.
}
\description{
Simple wrapper around \code{\link[sf:st_read]{sf::st_read()}} to load a kml or shapefile,
and optionally reproject it.
}
