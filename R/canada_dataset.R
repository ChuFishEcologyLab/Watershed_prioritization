#' Watershed priorization dataset for Canada
#'
#' Generate a watershed prioritization dataset for the Lake Erie watershed.
#' The dataset includes seven normalized variables used to prioritize watershed
#' conservation.
#'
#' @details
#' Note that raw data are embedded in the R package in `extdata` folder.
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
    #------------ Loading presence/absence
    can_pa <- path_input_data("Spp_dist_HYBAS6_20230125.csv") |>
        readr::read_csv(show_col_types = FALSE)
    can_pa_mat <- can_pa |>
        dplyr::select(-HYBAS_ID) |>
        as.matrix()
    #------------ Climate and stress data
    ws_data_path <- path_input_data("Variable_data_20241018.xlsx")
    can_data <- ws_data_path |>
        readxl::read_xlsx(sheet = "H6_CuThreat") |>
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
                compute_minns_Q_I(can_pa_mat)
            ) |> dplyr::rename(Fish_priority = Qi),
            by = "HYBAS_ID"
        ) |>
        #------------ Fish biodiversity
        dplyr::left_join(
            ws_data_path |>
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
        ) |>
        #------------ Calculate SARI and Richness
        dplyr::left_join(cbind(
            can_pa |>
                dplyr::select(HYBAS_ID),
            data.frame(
                SARI = rowSums(can_pa_mat == 2),
                Fish_richness = rowSums(can_pa_mat > 0)
            )
        ), by = "HYBAS_ID") |>
        #------------ Remove NA (3 extra watersheds introduces in one file)
        dplyr::filter(
            !is.na(FBCI),
            !is.na(WSI),
            !is.na(CCI),
            !is.na(Fish_priority),
            !is.na(Protected_area),
            !is.na(SARI),
            !is.na(Fish_richness)
        ) |>
        #------------ Normalizing indices
        dplyr::mutate(
            FBCI_n = scale_min_max(FBCI),
            WSI_n = scale_min_max(WSI),
            CCI_n = scale_min_max(CCI),
            Priority_n = scale_min_max(Fish_priority),
            Protected_area_n = scale_min_max(Protected_area),
            SARI_n = scale_min_max(SARI),
            Fish_richness_n = scale_min_max(Fish_richness)
        ) |>
        # ------------ Joining with feow
        dplyr::left_join(
            path_input_data("hyc_6_feow_join.csv") |>
                readr::read_csv(show_col_types = FALSE),
            by = "HYBAS_ID"
        ) |>
        dplyr::relocate(FEOW_ID, .after = HYBAS_ID)

    can_data
}
