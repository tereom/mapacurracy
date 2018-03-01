#' Compute ratio estimator for strata and combined ratio estimator for a
#' stratified sample of polygons.
#'
#' Ratio estimator is computed for each strata and then a combined version of
#' the ratio estimator is computed where the estimates of the strata are
#' weighted according to the area of each stratum (unlike traditional estimator
#' that weights with number of polygons sampled).
#' \code{strata_ratio_est} returns the estimates for each strata in the sample
#'  (assuming SRS within strata).
#' @param data \code{data.frame} each line is an observation and includes the
#'   stratum and columns used in ratio estimation.
#' @param allocation \code{data.frame} with a column defining the strata and a
#'   column with sample size allocations for each stratum (one line per stratum).
#' @param stratum unquoted column with strata in the data and allocation
#'   \code{data.frame}'s (the columns must have the same name in the
#'   two \code{data.frame}'s)
#' @param M: unquoted variable in data corresponding to denominator in ratio
#' (eg. polygon areas)
#' @param y: unquoted variable in data corresponding to numerator in ratio
#' (eg. binary vector 1 correctly classified 0 ow)
#' @param N: number of polygons in the strata.
#' @return A \code{list} with estimates per strata and the combined ratio
#'   estimate for overall population.
#' @examples
#' sampling_frame <- data_frame(id = 1:100, str = sample(1:5, 100, replace = TRUE))
#' allo <- sampling_frame %>%
#'     group_by(str) %>%
#'     mutate(n = 0.4 * n())
#' select_sample(allo, sampling_frame, n, str)

#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @export
combined_ratio_est <- function(data, allocation, stratum, M, y, N){
    stratum_var_string <- deparse(substitute(stratum))
    stratum <- dplyr::enquo(stratum)
    M <- dplyr::enquo(M)
    y <- dplyr::enquo(y)
    if (missing(N)){
        p_hs <- data %>%
            dplyr::mutate(M = !!M, y = !!y) %>%
            group_by(!!stratum) %>%
            do(ratio_est(M = .$M, y = .$y))
    } else {
        N <- dplyr::enquo(N)
        p_hs <- data %>%
            dplyr::mutate(M = !!M, y = !!y, N = !!N) %>%
            group_by(!!stratum) %>%
            do(ratio_est(M = .$M, y = .$y, N = .$N[1]))
    }
    p_h_strata <- p_hs %>%
        left_join(allocation, by = stratum_var_string)
    p_h_pop <- strat_est(p_h = p_h_strata$p_hat, n_h = p_h_strata$n,
        M_h = p_h_strata$M_h, N_h = p_h_strata$N_h, s2_h = p_h_strata$s2_p_hat)

    return(list(p_h_strata = p_h_strata, p_h_pop = p_h_pop))
}

ratio_est <- function(M, y, N = Inf){
    # computes estimate and standard error of y/M,
    # used for estimates per strata (with SRS within strata) or population
    #   estimates with SRS
    ## Arguments
    # M: denominator in ratio, (eg. vector of polygon areas)
    # y: numerator in ratio (eg. binary vector 1 correctly classified 0 ow)
    # N: number of polygons (in the strata)
    n <- length(M)
    M0_hat <- sum(M)
    M_bar <- mean(M)
    p_hat <- sum(M * y) / M0_hat
    s2_p_hat <- ifelse(n > 1,
        1 / (M_bar ^ 2) * 1 / (n - 1) * sum((M * (y - p_hat)) ^ 2),
        0)
    var_p_hat <- ifelse(n == N, 0, s2_p_hat / n * (1 - n / N))
    return(dplyr::data_frame(p_hat = p_hat, se_p_hat = sqrt(var_p_hat),
        s2_p_hat = s2_p_hat, n = n))
}

strat_est <- function(p_h, n_h, N_h, M_h, s2_h){
    # computes overall p_hat and se(p_hat), when p_strata is a ratio estimator
    ## Arguments
    # p_h: vector of ratio estimators per strata (as returned by ratio_est)
    # n_h: vector of number of polygons sampled per strata
    # s2_h: estimate of s^2 per strata (as returned by ratio_est)
    # N_h: vector of  number of polygons per strata (population)
    # M_h: vector of strata areas
    M0 <- sum(M_h) # total area
    p_hat <- sum(M_h / M0 * p_h)
    se_p_hat <- sqrt(sum((1 - n_h / N_h) * (M_h / M0) ^ 2 * s2_h / n_h))
    return(data.frame(p_hat = p_hat, se_p_hat = se_p_hat))
}

