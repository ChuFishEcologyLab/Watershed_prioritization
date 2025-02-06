run_lake_erie_analysis <- function(parameters) {
    #------------ Loading presence/absence
    le_pa <- path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
        readxl::read_xlsx(sheet = "LEH12FishPAmatrix")
    #------------ Climate velocity
    le_data <- path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
        readxl::read_xlsx(sheet = "LEH12_CCdata") |>
        #------------ Stress data
        dplyr::inner_join(
            path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
                readxl::read_xlsx(sheet = "LEH12_WSIdata"),
            by = "H12"
        ) |>
        #------------ Native and non-native species
        dplyr::inner_join(
            path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
                readxl::read_xlsx(sheet = "LEH12Fishnative_non-native"),
            by = "H12"
        ) |>
        dplyr::mutate(
            Fish_richness = H12NativeFishes + `H12Non-nativeFishes`,
            Fish_richness_check = rowSums(le_pa |> dplyr::select(-H12)) # ok
        ) |>
        #------------ SARI and Richness
        dplyr::inner_join(
            path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
                readxl::read_xlsx(sheet = "LEH12FishSAR"),
            by = "H12"
        ) |>
        #----------- Fish biodiversity Jacard.D 2BDone missing old assemblage.
        # dplyr::inner_join(...)  |>
        #------------ Calculate SARI
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
    #----------- Protected area 2BDone missing old assemblage.
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
            #!is.na(FBCI),
            !is.na(WSI),
            !is.na(CCI),
            !is.na(Fish_priority),
            #!is.na(Protected_area),
            !is.na(SARI),
            !is.na(Fish_richness)
        ) |>
        # Normalizing indices
        dplyr::mutate(
            #FBCI_n = scale_min_max(FBCI),
            WSI_n = scale_min_max(WSI),
            CCI_n = scale_min_max(CCI),
            Priority_n = scale_min_max(Fish_priority),
            #Protected_area_n = scale_min_max(Protected_area),
            SARI_n = scale_min_max(SARI),
            Fish_richness_n = scale_min_max(Fish_richness)
        )
        ## Join with H6? or FEOW_ID
        

    # write csv file
    readr::write_csv(
        le_data,
        path_output_data("watershed_prioritization_le_no_weight.csv")
    )

    # select final columns
    le_data |> dplyr::select(
        HYBAS_ID,
        #FEOW_ID,
        WSI_n,
        # FBCI_n,
        CCI_n,
        SARI_n,
        # Protected_area_n,
        Fish_richness_n,
        Priority_n
    )
}