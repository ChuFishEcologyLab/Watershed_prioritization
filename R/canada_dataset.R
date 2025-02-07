#' Watershed priorization data set for Canada
#'
#' Generate a watershed prioritization dataset for the Lake Erie watershed.
#' The dataset includes seven normalized variables used to prioritize watershed 
#' conservation.
#' 
#' @details
#' Note that raw data are embeded in the R package in `extdata` folder.
#' The scaling (variables are normalized) enables comparison between regions, 
#' considering that regions have varying numbers of watersheds and, 
#' consequently, different value ranges for ranking.
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
#' * Protected_area_n: Percentage of protected area overlap (capped at 100% if
#' multiple types of protected areas overlap within the watershed, resulting in
#' a total exceeding this limit).
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

generate_canada_dataset <- function() {
    can_pa <- path_input_data("Spp_dist_HYBAS6_20230125.csv") |>
        readr::read_csv(show_col_types = FALSE)
    #------------ Climate and stress data
    ws_data <- path_input_data("Variable_data_20241018.xlsx")
    df_prio <- ws_data |>
        readxl::read_xlsx(sheet = "H6_climate") |>
        dplyr::select(HYBAS6_ID, Stress, Climate) |>
        dplyr::rename(
            HYBAS_ID = HYBAS6_ID,
            CCI = Climate,
            WSI = Stress
        ) |>
        #------------ Fish community importance and priority data (Minns)
        dplyr::left_join(
            cbind(
                can_pa |>
                    dplyr::select(HYBAS_ID),
                compute_minns_Q_I(
                    can_pa |>
                        dplyr::select(-HYBAS_ID) |>
                        as.matrix()
                ) |>
                    dplyr::select(Qi)
            ) |> dplyr::rename(Fish_priority = Qi),
            by = "HYBAS_ID"
        ) |>
        #------------ Fish biodiversity
        dplyr::left_join(
            ws_data |>
                readxl::read_xlsx(sheet = "H6FishChange") |>
                dplyr::select(HYBAS6_ID, Jaccard.D) |>
                dplyr::rename(
                    HYBAS_ID = HYBAS6_ID,
                    FBCI = Jaccard.D
                ),
            by = "HYBAS_ID"
        ) |>
        #------------ Protected Area overlap
        dplyr::left_join(
            path_input_data("protected_area_overlap.csv") |>
                readr::read_csv(show_col_types = FALSE) |>
                dplyr::select(HYBAS_ID, perc_overlap) |>
                dplyr::mutate(perc_overlap = min(100, perc_overlap)) |> # cap
                dplyr::rename(
                    Protected_area = perc_overlap
                ),
            by = "HYBAS_ID"
        )

    #------------ Calculate SARI and Richness
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
    # join with df_prio
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
            !is.na(Protected_area),
            !is.na(SARI),
            !is.na(Fish_richness)
        ) |>
        # Normalizing indices
        dplyr::mutate(
            FBCI_n = scale_min_max(FBCI),
            WSI_n = scale_min_max(WSI),
            CCI_n = scale_min_max(CCI),
            Priority_n = scale_min_max(Fish_priority),
            Protected_area_n = scale_min_max(Protected_area),
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
        Priority_n,
        Protected_area_n
    )
}
