#' Generate prirization main data frame.
#'
#' @details
#' Note that raw data are embeded in the R package in `extdata` folder.
#'
#' @return
#' A data frame with the following columns:
#' * HYBAS_ID: Hydrobassin identifier ,
#' * FEOW_ID: Freshwater Ecoregions of the World identifier,
#' * WSI_n: Watershed stress index from Hirsh-Pearson (2022),
#' * FBCI_n: Fish biodiversity change index. Calculate Jaccard dissimilarity
#' for native spp pool vs. current native + non-native spp pool.
#' * CCI_n: Climate change velocities based on CMIP6 SSP2-4.5 forward
#' velocities for 2040-2070 period.
#' * SARI_n: Species at risk management
#' * Fish_richness_n: Species richness.
#' * Priority_n: Index of priority based on Minns rarity index (1987).
#' Bote that columns ending with `_n` are normalized.
#'
#'
#' @references
#' * Hirsh-Pearson, Kristen, Chris J. Johnson, Richard Schuster, Roger D.
#' Wheate, and Oscar Venter. 'Canada’s Human Footprint Reveals Large Intact
#' Areas Juxtaposed against Areas under Immense Anthropogenic Pressure.' Edited
#' by Raymond Bradley. FACETS 7 (January 1, 2022): 398–419.
#' https://doi.org/10.1139/facets-2021-0063.
#' * Minns, C. K. 'A Method of Ranking Species and Sites for Conservation Using
#' Presence-Absence Data and Its Application to Native Freshwater Fish in New
#' Zealand. New Zealand Journal of Zoology 14, no. 1 (January 1987): 43–49.
#' https://doi.org/10.1080/03014223.1987.10422680.
#'
#' @export

generate_priorization_data <- function() {
    ws_data <- path_input_data("Variable_data_20241018.xlsx")
    df_prio <- ws_data |>
        readxl::read_xlsx(sheet = "H6_climate") |>
        dplyr::select(HYBAS6_ID, Stress, Climate) |>
        dplyr::rename(
            HYBAS_ID = HYBAS6_ID,
            CCI = Climate,
            WSI = Stress
        ) |>
        ## Fish community importance and priority data
        dplyr::left_join(
            path_input_data("H6_importance_priority.csv") |>
                readr::read_csv(show_col_types = FALSE) |>
                dplyr::select(-Ii) |>
                dplyr::rename(Fish_priority = Qi),
            by = "HYBAS_ID"
        ) |>
        ## FCBI
        dplyr::left_join(
            ws_data |>
                readxl::read_xlsx(sheet = "H6FishChange") |>
                dplyr::select(HYBAS6_ID, Jaccard.D) |>
                dplyr::rename(
                    HYBAS_ID = HYBAS6_ID,
                    FBCI = Jaccard.D
                ),
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
            SARI = rowSums(fishPA6_nohbid == 2),
            Fish_richness = rowSums(fishPA6_nohbid > 0)
        )

    ### Join with df_prio
    df_prio <- df_prio |>
        dplyr::left_join(
            fishPA6 |>
                dplyr::select(HYBAS_ID, SARI, Fish_richness),
            by = "HYBAS_ID"
        )

    df_prio <- df_prio |>
        # check and remove NA
        dplyr::filter(
            !is.na(FBCI),
            !is.na(WSI),
            !is.na(CCI),
            !is.na(Fish_priority),
            !is.na(SARI),
            !is.na(Fish_richness)
        ) |>
        # Normalizing indices
        dplyr::mutate(
            FBCI_n = scale_min_max(FBCI),
            WSI_n = scale_min_max(WSI),
            CCI_n = scale_min_max(CCI),
            Priority_n = scale_min_max(Fish_priority),
            SARI_n = scale_min_max(SARI),
            Fish_richness_n = scale_min_max(Fish_richness)
        ) |>
        # join with feow
        dplyr::left_join(
            path_input_data("hyc_6_feow_join.csv") |>
                readr::read_csv(show_col_types = FALSE),
            by = "HYBAS_ID"
        )
    
    # write csv file
    readr::write_csv(
        df_prio,
        path_output_data("watershed_prioritization_no_weight.csv")
    )

    # select final columns
    df_prio |> dplyr::select(
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
