% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/homogeneizar_bits.R
\name{homogeneizar_bits}
\alias{homogeneizar_bits}
\title{Create standard columns and types from data in BITS shapefiles}
\usage{
homogeneizar_bits(datos)
}
\arguments{
\item{datos}{\code{data.frame} data.frame with data from BITS' shapefiles.}
}
\value{
A \code{data.frame} with columns edo, oid, id, tile, predicted,
interpreta and area_r.
}
\description{
Data in different tiles has diferent columns, this function standardizes the
information and variable types.
}
\examples{
sampling_frame <- data.frame(id = 1:100,
  str = sample(1:5, 100, replace = TRUE),
  val = rnorm(100))
allo <- sampling_frame \%>\%
    group_by(str) \%>\%
    summarise(n = 0.4 * n())
select_sample(allo, sampling_frame, n, str)
}
