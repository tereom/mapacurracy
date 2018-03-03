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
calcular_ests <- function(estados, edo_val, bits_df, marco_muestral){
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
    edo_val_df <- edo_val %>%
        dplyr::left_join(marco_muestral, by = c("edo", "oid", "tile")) %>%
        dplyr::left_join(select(bits_df, -predicted), by = c("edo", "oid", "tile")) %>%
        # dplyr::left_join(estratos_area, by = c("estrato")) %>%
        dplyr::mutate(
            id = stringr::str_c(oid, tile, sep = "-"),
            class_madmex = factor(predicted, levels = 1:32),
            class_bits = factor(interpreta, levels = 1:32),
            class_expert_1 = factor(Interpr1_p, levels = 1:32),
            class_expert_2 = factor(Interpr2_p, levels = 1:32),
            y_madmex = (predicted == Interpr1_p | predicted == Interpr2_p),
            y_bits = (interpreta == Interpr1_p | interpreta == Interpr2_p),
            y_M_madmex = y_madmex * area,
            y_M_bits = y_bits * area
        ) %>%
        dplyr::select(id, edo, estrato, area, class_madmex:y_M_bits)

    ests_madmex_area_sep <- separate_ratio_est(data = edo_val_df,
        estratos_area = estratos_area, stratum = estrato, y = y_madmex, M = area)

    ests_bits_area_sep <- separate_ratio_est(data = edo_val_df,
        estratos_area = estratos_area, stratum = estrato, y = y_bits, M = area)

    # usando srvyr para calcular el combinado
    edo_svy <- edo_val_df %>%
        dplyr::left_join(estratos_area, by = "estrato")

    edo_design <- edo_svy %>%
        srvyr::as_survey_design(ids = id, strata = estrato, fpc = N_h)

    ests_area_combined <- edo_design %>%
        srvyr::summarise(
            est_area_madmex = srvyr::survey_ratio(numerator = y_M_madmex,
                denominator = area),
            est_area_bits = srvyr::survey_ratio(numerator = y_M_bits,
                denominator = area)
        )

    ests_area_edo_combined <- edo_design %>%
        srvyr::group_by(edo) %>%
        srvyr::summarise(
            est_area_madmex = srvyr::survey_ratio(numerator = y_M_madmex,
                denominator = area),
            est_area_bits = srvyr::survey_ratio(numerator = y_M_bits,
                denominator = area)
        )

    ests_area_clase_madmex_combined <- edo_design %>%
        group_by(class_madmex) %>%
        summarise(
            est_area_madmex = srvyr::survey_ratio(numerator = y_M_madmex,
                denominator = area)
        )

    ests_area_clase_bits_combined <- edo_design %>%
        group_by(class_bits) %>%
        summarise(
            est_area_madmex = srvyr::survey_ratio(numerator = y_M_bits,
                denominator = area),
        )

    ests_area_clase_experto_combined <- edo_design %>%
        group_by(class_expert_1) %>%
        summarise(
            est_area_madmex = survey_ratio(numerator = y_M_madmex, denominator = area_r),
            est_area_bits = survey_ratio(numerator = y_M_bits, denominator = area_r)
        )

    ests_porcent_combined <- edo_design %>%
        srvyr::summarise(
            est_porcent_madmex = srvyr::survey_mean(numerator = y_madmex),
            est_porcent_bits = srvyr::survey_mean(numerator = y_bits)
        )

    ests_porcent_edo_combined <- edo_design %>%
        srvyr::group_by(edo) %>%
        srvyr::summarise(
            est_porcent_madmex = srvyr::survey_mean(numerator = y_madmex),
            est_porcent_bits = srvyr::survey_mean(numerator = y_bits)
        )

    ests_porcent_clase_madmex_combined <- edo_design %>%
        group_by(class_madmex) %>%
        summarise(
            est_porcent_madmex = srvyr::survey_mean(numerator = y_madmex)
        )

    ests_porcent_clase_bits_combined <- edo_design %>%
        group_by(class_bits) %>%
        summarise(
            est_porcent_madmex = srvyr::survey_mean(numerator = y_bits)
        )


    ests_porcent_clase_experto_combined <- edo_design %>%
        group_by(class_expert_1) %>%
        summarise(
            est_area_madmex = survey_mean(numerator = y_madmex),
            est_area_bits = survey_mean(numerator = y_bits)
        )



    return(list(est_madmex_area_sep = ests_madmex_area_sep,
        ests_bits_area_sep = ests_bits_area_sep,
        ests_area_combined = ests_area_combined,
        ests_area_edo_combined = ests_area_edo_combined,
        ests_area_clase_madmex_combined = ests_area_clase_madmex_combined,
        ests_area_clase_bits_combined = ests_area_clase_bits_combined,
        ests_area_clase_experto_combined = ests_area_clase_experto_combined,
        ests_porcent_combined = ests_porcent_combined,
        ests_porcent_edo_combined = ests_porcent_edo_combined,
        ests_porcent_clase_madmex_combined = ests_porcent_clase_madmex_combined,
        ests_porcent_clase_bits_combined = ests_porcent_clase_bits_combined,
        ests_porcent_clase_experto_combined = ests_porcent_clase_experto_combined
        ))
}
