#' Create data frame
#'
#' @details
#' Data are embeded in the R package in `extdata` folder.
#'
#' @export

priorization_6 <- function() {
    ws_data <- path_input_data("Variable_data_20241018.xlsx")
    data6 <- ws_data |>
        readxl::read_xlsx(sheet = "H6_climate") |>
        dplyr::select(HYBAS6_ID, Climate, Stress) |>
        dplyr::rename(HYBAS_ID = HYBAS6_ID) |>
        # path_input_data("HyC_6_df.csv") |>
        # readr::read_csv() |>
        # dplyr::select(
        #     HYBAS_ID,
        #     MEAN_INT_1, # WSI measures
        #     rcp45_55
        # ) |> ## Climate measure
        ## Fish community importance and priority data
        dplyr::left_join(
            path_input_data("H6_importance_priority.csv") |>
                readr::read_csv(show_col_types = FALSE) |>
                dplyr::select(-Ii),
            by = "HYBAS_ID"
        ) |>
        ## FCBI
        dplyr::left_join(
            ws_data |>
                readxl::read_xlsx(sheet = "H6FishChange") |>
                dplyr::select(HYBAS6_ID, Jaccard.D) |>
                dplyr::rename(HYBAS_ID = HYBAS6_ID),
            by = "HYBAS_ID"
        )

    ###########
    # Calculate SARI and Richness
    fishPA6 <- path_input_data("Spp_dist_HYBAS6_20230125.csv") |>
        readr::read_csv(show_col_types = FALSE)
    # remove hydrobasin id name
    fishPA6_nohbid <- fishPA6 |>
        dplyr::select(-c(HYBAS_ID))

    fishPA6 <- fishPA6 |>
        dplyr::mutate(
            SAR_count = rowSums(fishPA6_nohbid == 2),
            total_richness = rowSums(fishPA6_nohbid > 0)
        ) |>
        dplyr::mutate(pSAR = SAR_count / total_richness)

    ### Join with data6
    data6 <- data6 |>
        dplyr::left_join(
            fishPA6 |>
                dplyr::select(HYBAS_ID, SAR_count, total_richness),
            by = "HYBAS_ID"
        )

    colnames(data6) <- c(
        "HYBAS_ID",
        "WSI", # is just HStress
        "CCI", # is rcp45_55
        "Fish_priority",
        "FBCI",
        "SARI", # is count of SAR
        "Fish_richness"
    ) # Is total richness

    # Standardize
    data6 <- data6 |>
        dplyr::mutate(
            FBCI_n = scale_min_max(FBCI),
            WSI_n = scale_min_max(WSI),
            CCI_n = scale_min_max(CCI),
            Priority_n = scale_min_max(Fish_priority),
            SARI_n = scale_min_max(SARI),
            Fish_richness_n = scale_min_max(Fish_richness)
        )

    # join with feow
    data6 <- data6 |>
        dplyr::left_join(
            path_input_data("hyc_6_feow_join.csv") |>
                readr::read_csv(show_col_types = FALSE),
            by = "HYBAS_ID"
        )

    # drop unneeded columns
    data6 |> dplyr::select(
        HYBAS_ID,
        FEOW_ID,
        WSI_n,
        FBCI_n,
        CCI_n,
        SARI_n,
        Fish_richness_n,
        Priority_n
    )
}

