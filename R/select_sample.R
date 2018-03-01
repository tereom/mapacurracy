#' Randomly select samples from a sampling frame with or without strata.
#'
#' Given a data.frame with sample size allocation per strata and a data.frame
#' with the sampling frame it selects random samples.
#' @param allocation \code{data.frame} with a column defining the strata and a
#'   column with sample size allocations for each stratum (one line per stratum).
#' @param sampling_frame \code{data.frame} with the sampling frame it must contain a
#'   column with the stratum.
#' @param sample_size unquoted column with sample sizes in the allocation
#'   data.frame
#' @param stratum unquoted column with strata in the allocation and
#'   sampling_frame \code{data.frame}'s (the columns must have the same name in the
#'   two \code{data.frame}'s)
#' @return A \code{data.frame} with the selected sample, it will have the same
#'   columns as the original sampling frame plus a column indicating the sample
#'   size in the stratum of each selected observation.

#' @examples
#' sampling_frame <- data.frame(id = 1:100,
#'   str = sample(1:5, 100, replace = TRUE),
#'   val = rnorm(100))
#' allo <- sampling_frame %>%
#'     group_by(str) %>%
#'     summarise(n = 0.4 * n())
#' select_sample(allo, sampling_frame, n, str)

#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @export
select_sample <- function(allocation, sampling_frame,
    sample_size = sample_size, stratum = stratum){

    sample_size <- dplyr::enquo(sample_size)
    sample_size_name <- dplyr::quo_name(sample_size)

    stratum_var_string <- deparse(substitute(stratum))
    stratum <- dplyr::enquo(stratum)

    # if sample size not integer we round it
    allocation <- allocation %>%
        dplyr::mutate(!!sample_size_name := round(!!sample_size)) # %>%
        #select(!!!stratum, sample_size)

    sample <- sampling_frame %>%
        dplyr::left_join(allocation, by = stratum_var_string) %>%
        split(.[stratum_var_string]) %>%
        purrr::map_df(~dplyr::sample_n(.,
            size = dplyr::pull(., sample_size_name)[1]))

    sample <- sampling_frame %>%
        dplyr::left_join(allocation, by = stratum_var_string) %>%
        split(.[stratum_var_string]) %>%
        purrr::map_df(~dplyr::sample_n(.,
            size = .[, sample_size_name][1]))

    return(sample)
}
