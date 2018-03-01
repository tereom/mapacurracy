#' Create standard columns and types from data in BITS shapefiles
#'
#' Data in different tiles has diferent columns, this function standardizes the
#' information and variable types.
#' @param datos \code{data.frame} data.frame with data from BITS' shapefiles.
#' @return A \code{data.frame} with columns edo, oid, id, tile, predicted,
#' interpreta and area_r.

#' @examples
#' sampling_frame <- data.frame(id = 1:100,
#'   str = sample(1:5, 100, replace = TRUE),
#'   val = rnorm(100))
#' allo <- sampling_frame %>%
#'     group_by(str) %>%
#'     summarise(n = 0.4 * n())
#' select_sample(allo, sampling_frame, n, str)

#' @importFrom magrittr %>%
#' @export
homogeneizar_bits <- function(datos){
    datos_h <- datos %>%
        dplyr::select(-edo) %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::mutate_if(is.character, as.numeric)
    if(!("oid" %in% colnames(datos))) datos_h$oid <- datos_h$oid_1
    datos_h$edo <- as.character(datos$edo)
    datos_h %>%
        dplyr::select(edo, oid, id, tile, predicted, interpreta, area_r)
}
