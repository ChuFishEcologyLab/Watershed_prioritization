#' Applying expert weight 
#'
#' @return 
#' Writes `watershed_prioritization_level5.csv` and 
#' `watershed_prioritization_level6.csv`
#' 
#' @export

apply_weight <- function() {
  ### read weightings
  weights <- path_input_data("Co_author_weightings.csv") |>
    readr::read_csv()

  med_weights <- weights |>
    dplyr::select(-Co_author) |>
    dplyr::summarise_all(stats::median) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(), 
      names_to = "Variable", 
      values_to = "Weight"
    )

  # -----------------------------------------------------------------
  # Level 5
  # priorities for Protected areas
  data5 <- path_input_data("Hybas5_data.csv") |>
    readr::read_csv() |>
    dplyr::mutate(protection_score = WSI_n * med_weights$Weight[1] +
      FBCI_n * med_weights$Weight[2] +
      CCI_n * med_weights$Weight[3] +
      SARI_n * med_weights$Weight[4] +
      Fish_richness_n * med_weights$Weight[5] +
      Priority_n * med_weights$Weight[6]) |>
    # ranked within FEOW
    dplyr::arrange(FEOW_ID, -protection_score) |>
    dplyr::group_by(FEOW_ID) |>
    dplyr::mutate(Prot_rank_feow = dplyr::row_number()) |>
    # priorities for restoration
    dplyr::mutate(restoration_score = WSI_n * med_weights$Weight[7] +
      FBCI_n * med_weights$Weight[8] +
      CCI_n * med_weights$Weight[9] +
      SARI_n * med_weights$Weight[10] +
      Fish_richness_n * med_weights$Weight[11] +
      Priority_n * med_weights$Weight[12]) |>
    # ranked within FEOW
    dplyr::ungroup() |>
    dplyr::arrange(FEOW_ID, -restoration_score) |>
    dplyr::group_by(FEOW_ID) |>
    dplyr::mutate(Rest_rank_feow = dplyr::row_number()) |>
    # priorities for SAR
    dplyr::mutate(SAR_score = WSI_n * med_weights$Weight[13] +
      FBCI_n * med_weights$Weight[14] +
      CCI_n * med_weights$Weight[15] +
      SARI_n * med_weights$Weight[16] +
      Fish_richness_n * med_weights$Weight[17] +
      Priority_n * med_weights$Weight[18]) |>
    # ranked within FEOW
    dplyr::ungroup() |>
    dplyr::arrange(FEOW_ID, -SAR_score) |>
    dplyr::group_by(FEOW_ID) |>
    dplyr::mutate(SAR_rank_feow = dplyr::row_number()) |>
    # priorities for AIS
    dplyr::mutate(AIS_score = WSI_n * med_weights$Weight[19] +
      FBCI_n * med_weights$Weight[20] +
      CCI_n * med_weights$Weight[21] +
      SARI_n * med_weights$Weight[22] +
      Fish_richness_n * med_weights$Weight[23] +
      Priority_n * med_weights$Weight[24]) |>
    # ranked within FEOW
    dplyr::ungroup() |>
    dplyr::arrange(FEOW_ID, -AIS_score) |>
    dplyr::group_by(FEOW_ID) |>
    dplyr::mutate(AIS_rank_feow = dplyr::row_number())

  readr::write_csv(
    data5, 
    path_output_data("watershed_prioritization_level5.csv")
  )

  # -----------------------------------------------------------------
  # Level 6

  data6 <- path_input_data("Hybas6_data.csv") |>
    readr::read_csv() |>
  # priorities for Protected areas
    dplyr::mutate(protection_score = WSI_n * med_weights$Weight[1] +
      FBCI_n * med_weights$Weight[2] +
      CCI_n * med_weights$Weight[3] +
      SARI_n * med_weights$Weight[4] +
      Fish_richness_n * med_weights$Weight[5] +
      Priority_n * med_weights$Weight[6]
    )  |>
  # ranked within FEOW
    dplyr::arrange(FEOW_ID, -protection_score) |>
    dplyr::group_by(FEOW_ID) |>
    dplyr::mutate(Prot_rank_feow = dplyr::row_number())  |>
  # priorities for restoration
    dplyr::mutate(
      restoration_score = WSI_n * med_weights$Weight[7] +
      FBCI_n * med_weights$Weight[8] +
      CCI_n * med_weights$Weight[9] +
      SARI_n * med_weights$Weight[10] +
      Fish_richness_n * med_weights$Weight[11] +
      Priority_n * med_weights$Weight[12]
      )  |>
  # ranked within FEOW
    dplyr::ungroup() |>
    dplyr::arrange(FEOW_ID, -restoration_score) |>
    dplyr::group_by(FEOW_ID) |>
    dplyr::mutate(Rest_rank_feow = dplyr::row_number())  |>
  # priorities for SAR
    dplyr::mutate(SAR_score = WSI_n * med_weights$Weight[13] +
      FBCI_n * med_weights$Weight[14] +
      CCI_n * med_weights$Weight[15] +
      SARI_n * med_weights$Weight[16] +
      Fish_richness_n * med_weights$Weight[17] +
      Priority_n * med_weights$Weight[18]
    )  |>
  # ranked within FEOW
    dplyr::ungroup() |>
    dplyr::arrange(FEOW_ID, -SAR_score) |>
    dplyr::group_by(FEOW_ID) |>
    dplyr::mutate(SAR_rank_feow = dplyr::row_number())  |>
  # priorities for AIS
    dplyr::mutate(
      AIS_score = WSI_n * med_weights$Weight[19] +
      FBCI_n * med_weights$Weight[20] +
      CCI_n * med_weights$Weight[21] +
      SARI_n * med_weights$Weight[22] +
      Fish_richness_n * med_weights$Weight[23] +
      Priority_n * med_weights$Weight[24]
    )  |>
  # ranked within FEOW
    dplyr::ungroup() |>
    dplyr::arrange(FEOW_ID, -AIS_score) |>
    dplyr::group_by(FEOW_ID) |>
    dplyr::mutate(AIS_rank_feow = dplyr::row_number())

  readr::write_csv(
    data6, 
    path_output_data("watershed_prioritization_level6.csv")
  )

  invisible(TRUE)
}