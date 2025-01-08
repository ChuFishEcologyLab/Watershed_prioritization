#' Calculate rarity indices
#'
#' @export


compute_rarity_index <- function() {
    # ------------------------ Hydrobasin 5
    fishPA5 <- path_input_data("Spp_dist_HYBAS5_20230125.csv") |>
        readr::read_csv()

    # change the SAR indicator of '2' to a presence indicator of '1'
    fishPA5[fishPA5 == 2] <- 1

    Pvals <- fishPA5 |>
        dplyr::summarize_all(.funs = "mean")
    Pvals <- purrr::as_vector(Pvals)
    Pvals <- Pvals[-1]
    Qvals <- 1 - Pvals
    TotalQ <- sum(Qvals)

    #
    Ivals <- fishPA5
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
        ) |>
        dplyr::mutate(
            Ii = QijSij / TotalQ,
            Qi = QijSij / richness
        )

    hb_lvl5 <- Ivals |> dplyr::select(HYBAS_ID, Ii, Qi)
    readr::write_csv(
        hb_lvl5,
        path_output_data("H5_importance_priority.csv")
    )


    # ------------------------ Hydrobasin lvl 6

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

    list(level5 = hb_lvl5, level6 = hb_lvl6)
}