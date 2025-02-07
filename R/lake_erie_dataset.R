#' Watershed priorization data set for Lake Erie
#'
#' Generate a watershed prioritization dataset for the Lake Erie watershed.
#' The dataset includes six normalized variables used to prioritize watershed 
#' conservation.
#' 
#' @details
#' Note that raw data are embeded in the R package in `extdata` folder.
#' The scaling (variables are normalized) enables comparison between regions,
#' considering that regions have varying numbers of watersheds and,
#' consequently, different value ranges for ranking. For Lake Erie, Only
#' 82 units with suffisant data have been retained.
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
#'
#' @export

generate_lake_erie_dataset <- function(parameters) {
    #------------ Loading presence/absence
    le_pa <- path_input_data("H12LakeErie/LEH12_native_nonnative.xlsx") |>
        readxl::read_xlsx(sheet = "LEH12Fishnative_non-native")
    le_pa_mat <- le_pa |>
        dplyr::select(-H12) |>
        as.matrix()
    #------------ Climate velocity
    le_data <- path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
        readxl::read_xlsx(sheet = "LEH12_CCdata") |>
        #------------ Stress data
        dplyr::inner_join(
            path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
                readxl::read_xlsx(sheet = "LEH12_WSIdata"),
            by = "H12"
        ) |>
        #------------ Richness & Jaccard
        dplyr::inner_join(
            le_pa |>
                dplyr::select(H12) |>
                dplyr::mutate(FBCI = get_jaccard(le_pa_mat)) |>
                dplyr::mutate(Fish_richness = rowSums(le_pa_mat > 0))
        ) |>
        #------------ SARI
        dplyr::inner_join(
            path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
                readxl::read_xlsx(sheet = "LEH12FishSAR"),
            by = "H12"
        ) |>
        #------------ Fish community importance and priority data
        dplyr::left_join(
            cbind(
                # use first column
                le_pa |> dplyr::select(H12),
                # use column Qi from the Minns computation
                le_pa |>
                    dplyr::select(-H12) |>
                    as.matrix() |>
                    compute_minns_Q_I() |>
                    dplyr::select(Qi)
            )
            |>
                dplyr::rename(Fish_priority = Qi),
            by = "H12"
        ) |>
        #---------- Rename columns
        dplyr::rename(
            HYBAS_ID = H12,
            CCI = H12FwVel2452050,
            WSI = H12CuThreat,
            SARI = H12CountFishMusselSAR
        )

    le_data <- le_data |>
        # check and remove NA
        dplyr::filter(
            # !is.na(FBCI),
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
        )


    # write csv file
    readr::write_csv(
        le_data,
        path_output_data("watershed_prioritization_le_no_weight.csv")
    )

    # select final columns
    le_data |> dplyr::select(
        HYBAS_ID,
        # FEOW_ID,
        WSI_n,
        FBCI_n,
        CCI_n,
        SARI_n,
        Fish_richness_n,
        Priority_n
    )
}

# for every water, the function creates native vs non native assemblages
# and compute the jaccard distance
get_jaccard <- function(x) {
    stopifnot(inherits(x, "matrix"))
    apply(x, 1, \(x) {
        vegan::vegdist(
            rbind(x == 1, x > 0),
            method = "jaccard",
            binary = TRUE
        )
    })
}