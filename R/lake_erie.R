# run_lake_erie_analysis <- function(parameters) {
#     #------------ Climate velocity
#    le_data <- path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
#        readxl::read_xlsx(sheet = "LEH12_CCdata") |>
#        #------------ Stress data
#        dplyr::inner_join(
#            path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
#                readxl::read_xlsx(sheet = "LEH12_WSIdata"),
#            by = "H12"
#        ) |>
#        #------------ Native and non-native species
#        dplyr::inner_join(
#            path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
#                readxl::read_xlsx(sheet = "LEH12Fishnative_non-native"),
#            by = "H12"
#        ) |>
#        #------------ SAR
#        dplyr::inner_join(
#            path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
#                readxl::read_xlsx(sheet = "LEH12FishSAR"),
#            by = "H12"
#        )
#    #----------- Jacard.D
#    le_pa <- path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
#        readxl::read_xlsx(sheet = "LEH12FishPAmatrix")

#    le_pa |> dplyr::select(-H12)


#     ws_data <- path_input_data("Variable_data_20241018.xlsx")
#     df_prio <- ws_data |>
#         readxl::read_xlsx(sheet = "H6_climate") |>
#         dplyr::select(HYBAS6_ID, Stress, Climate) |>
#         dplyr::rename(
#             HYBAS_ID = HYBAS6_ID,
#             CCI = Climate,
#             WSI = Stress
#         ) |>
#         #------------ Fish community importance and priority data
#         dplyr::left_join(
#             path_input_data("H6_importance_priority.csv") |>
#                 readr::read_csv(show_col_types = FALSE) |>
#                 dplyr::select(-Ii) |>
#                 dplyr::rename(Fish_priority = Qi),
#             by = "HYBAS_ID"
#         ) |>
#         #------------ Fish biodiversity
#         dplyr::left_join(
#             ws_data |>
#                 readxl::read_xlsx(sheet = "H6FishChange") |>
#                 dplyr::select(HYBAS6_ID, Jaccard.D) |>
#                 dplyr::rename(
#                     HYBAS_ID = HYBAS6_ID,
#                     FBCI = Jaccard.D
#                 ),
#             by = "HYBAS_ID"
#         ) |>
#         #------------ Protected Area overlap TODO


#     #------------ Calculate SARI and Richness
#     fishPA6 <- path_input_data("Spp_dist_HYBAS6_20230125.csv") |>
#         readr::read_csv(show_col_types = FALSE)
#     # remove hydrobasin id name
#     fishPA6_nohbid <- fishPA6 |>
#         dplyr::select(-c(HYBAS_ID))
#     fishPA6 <- fishPA6 |>
#         dplyr::mutate(
#             SARI = rowSums(fishPA6_nohbid == 2),
#             Fish_richness = rowSums(fishPA6_nohbid > 0)
#         )
#     # join with df_prio
#     df_prio <- df_prio |>
#         dplyr::left_join(
#             fishPA6 |>
#                 dplyr::select(HYBAS_ID, SARI, Fish_richness),
#             by = "HYBAS_ID"
#         )



#     df_prio <- df_prio |>
#         # check and remove NA
#         dplyr::filter(
#             !is.na(FBCI),
#             !is.na(WSI),
#             !is.na(CCI),
#             !is.na(Fish_priority),
#             !is.na(Protected_area),
#             !is.na(SARI),
#             !is.na(Fish_richness)
#         ) |>
#         # Normalizing indices
#         dplyr::mutate(
#             FBCI_n = scale_min_max(FBCI),
#             WSI_n = scale_min_max(WSI),
#             CCI_n = scale_min_max(CCI),
#             Priority_n = scale_min_max(Fish_priority),
#             Protected_area_n = scale_min_max(Protected_area),
#             SARI_n = scale_min_max(SARI),
#             Fish_richness_n = scale_min_max(Fish_richness)
#         ) |>
#         # join with feow
#         dplyr::left_join(
#             path_input_data("hyc_6_feow_join.csv") |>
#                 readr::read_csv(show_col_types = FALSE),
#             by = "HYBAS_ID"
#         )

#     # write csv file
#     readr::write_csv(
#         df_prio,
#         path_output_data("watershed_prioritization_no_weight.csv")
#     )

#     # select final columns
#     df_prio |> dplyr::select(
#         HYBAS_ID,
#         FEOW_ID,
#         WSI_n,
#         FBCI_n,
#         CCI_n,
#         SARI_n,
#         Protected_area_n,
#         Fish_richness_n,
#         Priority_n
#     )
# }