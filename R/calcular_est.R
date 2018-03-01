#' Create standard columns and types from data in BITS shapefiles
#'
#' Data in different tiles has diferent columns, this function standardizes the
#' information and variable types.
#' @param estados string ot vector of strings with the name of the state as
#'   defined in sampling frame
#' @param bits_df data.frame with BITS classifications.
#' @param edo_val data.frame with expert classification.
#' @return A list with estimates for percentage of area correcrly classified and
#'   percentage of polygons correctly classified.

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
calcular_ests <- function(estados, edo_val, bits_df){
    # variables:
    # predicted - MADMEX
    # Interpr1_p, Interpr2_p (interpretaciones Pedro)

    # necesitamos calcular M_h el tamaño de cada cluster
    # load("datos_procesados/2017-08-18_marco_muestral.Rdata")

    estratos_area <- marco_muestral %>%
        dplyr::filter(edo %in% estados) %>%
        dplyr::group_by(estrato) %>%
        dplyr::summarise(
            M_h = sum(area),
            N_h = n()
        )
    # rm(marco_muestral)

    # preparamos los datos, agregando el estrato, oid es necesario para unir los
    # datos y no está incluída en el marco muestral (valdria la pena cambiar eso)
    # load("datos_procesados/2017-08-18_pais_df.Rdata")
    edo_df <- dplyr::filter(pais_df, edo == estado)
    # rm(pais_df)

    edo_val_df <- edo_val %>%
        dplyr::left_join(edo_df, by = c("oid", "tile")) %>%
        dplyr::select(oid, tile, predicted = predicted.y, edo = edo.x, area_r,
            Interpr1_p, Interpr2_p) %>%
        dplyr::mutate(area_cat = case_when(
            .$area_r <= 50000 ~ "[5000,50000]",
            .$area_r <= 100000 ~ "(50000,100000]",
            .$area_r <= 500000 ~ "(100000,500000]",
            TRUE ~ "(500000,1200000]"),
            estrato = paste(predicted, edo, area_cat, sep = "-"),
            y_h = (predicted == Interpr1_p | predicted == Interpr2_p)
        ) %>%
        dplyr::left_join(estratos_area)



    ## % área clasificado correcto por Madmex (predicted)
    p_hs <- edo_val_df %>%
        dplyr::group_by(estrato) %>%
        dplyr::do(ratio_est(M = .$area_r, y = .$y_h, N = .$N_h[1])) %>%
        dplyr::left_join(estratos_area, by = c("estrato"))

    est_madmex_area <- strat_est(p_h = p_hs$p_hat, n_h = p_hs$n, M_h = p_hs$M_h,
        N_h = p_hs$N_h, s2_h = p_hs$s2_p_hat)

    edo_bits_df <- bits_df %>%
        dplyr::filter(edo %in% edos)

    p_hs <- edo_bits_df %>%
        dplyr::mutate(y_h = (interpreta == Interpr1_p | interpreta == Interpr2_p)) %>%
        dplyr::left_join(estratos_area) %>%
        dplyr::group_by(estrato) %>%
        dplyr::do(ratio_est(M = .$area_r, y = .$y_h, N = .$N_h[1])) %>%
        dplyr::left_join(estratos_area, by = c("estrato"))

    est_bits_area <- strat_est(p_h = p_hs$p_hat, n_h = p_hs$n, M_h = p_hs$M_h,
        N_h = p_hs$N_h, s2_h = p_hs$s2_p_hat)

    ## % polígonos clasificados correcto Bits
    p_porcent <- edo_bits_df %>%
        dplyr::mutate(y_h = (interpreta == Interpr1_p | interpreta == Interpr2_p))%>%
        dplyr::left_join(estratos_area) %>%
        dplyr::group_by(estrato) %>%
        dplyr::summarise(
            n_h = n(),
            N_h = first(N_h),
            prop_h = N_h / sum(estratos_area$N_h),
            f_h = n_h / first(N_h),
            p_h = mean(y_h),
            p_aux_h = prop_h * p_h,
            var_aux = (1 - f_h) * prop_h ^ 2 * p_h * (1 - p_h) / (n_h - 1)
        )

    ests_bits_porcent <- c(p_hat = sum(p_porcent$p_aux_h),
        se_p_hat = sqrt(sum(p_porcent$var_aux[p_porcent$N_h > 1])))

    # Madmex
    p_porcent <- edo_bits_df %>%
        dplyr::mutate(y_h = (predicted.x == Interpr1_p | predicted.x == Interpr2_p)) %>%
        dplyr::left_join(estratos_area) %>%
        dplyr::group_by(estrato) %>%
        dplyr::summarise(
            n_h = n(),
            N_h = first(N_h),
            prop_h = N_h / sum(estratos_area$N_h),
            f_h = n_h / first(N_h),
            p_h = mean(y_h),
            p_aux_h = prop_h * p_h,
            var_aux = (1 - f_h) * prop_h ^ 2 * p_h * (1 - p_h) / (n_h - 1)
        )

    ests_madmex_porcent <- c(p_hat = sum(p_porcent$p_aux_h),
        se_p_hat = sqrt(sum(p_porcent$var_aux[p_porcent$N_h > 1])))

    edo_svy <-  edo_bits_df %>%
        dplyr::mutate(
            id = stringr::str_c(oid, tile, sep = "-"),
            class_madmex = factor(predicted.x, levels = 1:32),
            class_bits = factor(interpreta, levels = 1:32),
            class_expert_1 = factor(Interpr1_p, levels = 1:32),
            class_expert_2 = factor(Interpr2_p, levels = 1:32),
            y_madmex = (predicted.x == Interpr1_p | predicted.x == Interpr2_p),
            y_M_madmex = y_madmex * area_r,
            y_bits = (interpreta == Interpr1_p | interpreta == Interpr2_p),
            y_M_bits = y_bits * area_r
        ) %>%
        dplyr::select(id, edo, class_madmex, class_expert_1, class_expert_2,
            class_bits, N_h, y_madmex,
            y_M_madmex, y_bits, y_M_bits, estrato, area_r
        )

    edo_design <- edo_svy %>%
        srvyr::as_survey_design(ids = id, strata = estrato, fpc = N_h)

    est_area_comb <- edo_design %>%
        srvyr::summarise(
            est_area_madmex = srvyr::survey_ratio(numerator = y_M_madmex,
                denominator = area_r),
            est_area_bits = srvyr::survey_ratio(numerator = y_M_bits,
                denominator = area_r)
        )

    est_area_clase_madmex <- edo_design %>%
        srvyr::group_by(class_madmex) %>%
        srvyr::summarise(
            est_area_madmex = srvyr::survey_ratio(numerator = y_M_madmex,
                denominator = area_r),
        )

    est_area_clase_bits <- edo_design %>%
        srvyr::group_by(class_bits) %>%
        srvyr::summarise(
            est_area_bits = srvyr::survey_ratio(numerator = y_M_bits,
                denominator = area_r)
        )

    est_area_edo <-  edo_design %>%
        srvyr::group_by(edo) %>%
        srvyr::summarise(
            est_area_madmex = srvyr::survey_ratio(numerator = y_M_madmex,
                denominator = area_r),
            est_area_bits = srvyr::survey_ratio(numerator = y_M_bits,
                denominator = area_r)
        )

    return(list(est_madmex_area_sep = est_madmex_area,
        est_bits_area_sep = est_bits_area,
        ests_madmex_porcent = ests_madmex_porcent,
        ests_bits_porcent = ests_bits_porcent,
        est_area_comb, est_area_clase_madmex,
        esta_area_clase_bits, est_area_edo))
}
