#' Calculate rarity indices
#'
#' @details
#' Calculation based on Q index in Minns (1987). 
#' 
#' @references 
#' * Minns, C. K. 'A Method of Ranking Species and Sites for Conservation Using 
#' Presence-Absence Data and Its Application to Native Freshwater Fish in New 
#' Zealand. New Zealand Journal of Zoology 14, no. 1 (January 1987): 43–49. 
#' https://doi.org/10.1080/03014223.1987.10422680.
#'
#' @export

compute_rarity_index <- function() {
    fishPA6 <- path_input_data("Spp_dist_HYBAS6_20230125.csv") |>
        readr::read_csv()

    fishPA6[fishPA6 == 2] <- 1 # change the SAR indicator of '2' to a presence indicator of '1'

    Pvals <- fishPA6 |>
        dplyr::summarize_all(.funs = "mean")
    Pvals <- purrr::as_vector(Pvals)
    Pvals <- Pvals[-1]
    Qvals <- 1 - Pvals
    TotalQ <- sum(Qvals)

    #
    Ivals <- fishPA6
    for (i in 2:ncol(Ivals)) {
        Ivals[i] <- Ivals[i] * Qvals[i - 1]
    }

    Ivals <- Ivals |>
        dplyr::rowwise() |>
        dplyr::mutate(
            QijSij = sum(
                dplyr::c_across(Acipenser_brevirostrum:Percina_shumardi)
            ),
            richness = sum(
                dplyr::c_across(Acipenser_brevirostrum:Percina_shumardi) > 0
            )
        )

    Ivals <- Ivals |>
        dplyr::mutate(Ii = QijSij / TotalQ) |>
        dplyr::mutate(Qi = QijSij / richness)

    hb_lvl6 <- Ivals |> dplyr::select(HYBAS_ID, Ii, Qi)
    readr::write_csv(
        hb_lvl6,
        path_output_data("H6_importance_priority.csv")
    )

    hb_lvl6
}