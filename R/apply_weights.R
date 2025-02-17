#' Applying expert weight to watershed prioritization data
#'
#' @param ws_data `[data.frame]`\cr A data frame with the variable of interest
#' (see [generate_canada_dataset()] and [generate_lake_erie_dataset()]).
#'
#' @details
#' Use the median values of expert weights used to generate weighted-scores for
#' four management strategies (weighting schema):
#' * Area-based protection
#' * Restoration
#' * SAR management
#' * Invasive species management
#'
#' @return
#' A data frame with the same data as `ws_data` plus four columns for the
#' scores computed and four additional four columns including the scaled
#' score within FEOW if `ws_data` includes `FEOW_ID`.
#'
#' @export

apply_weights <- function(ws_data) {
  ### read weightings data
  weights <- path_input_data("Co_author_weightings.csv") |>
    readr::read_csv(show_col_types = FALSE)

  med_weights <- weights |>
    dplyr::select(-Co_author) |>
    dplyr::summarise_all(stats::median) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Variable",
      values_to = "Weight"
    )

  ws_data_w <- ws_data |>
    dplyr::mutate(
      #---------------- Priorities for Protected areas
      protection_score =
        WSI_n * med_weights$Weight[1] +
          FBCI_n * med_weights$Weight[2] +
          CCI_n * med_weights$Weight[3] +
          SARI_n * med_weights$Weight[4] +
          Fish_richness_n * med_weights$Weight[5] +
          Priority_n * med_weights$Weight[6],
      #---------------- Priorities for restoration
      restoration_score = WSI_n * med_weights$Weight[7] +
        FBCI_n * med_weights$Weight[8] +
        CCI_n * med_weights$Weight[9] +
        SARI_n * med_weights$Weight[10] +
        Fish_richness_n * med_weights$Weight[11] +
        Priority_n * med_weights$Weight[12],
      #---------------- Priorities for SAR
      SAR_score = WSI_n * med_weights$Weight[13] +
        FBCI_n * med_weights$Weight[14] +
        CCI_n * med_weights$Weight[15] +
        SARI_n * med_weights$Weight[16] +
        Fish_richness_n * med_weights$Weight[17] +
        Priority_n * med_weights$Weight[18],

      #---------------- Priorities for AIS
      AIS_score = WSI_n * med_weights$Weight[19] +
        FBCI_n * med_weights$Weight[20] +
        CCI_n * med_weights$Weight[21] +
        SARI_n * med_weights$Weight[22] +
        Fish_richness_n * med_weights$Weight[23] +
        Priority_n * med_weights$Weight[24]
    )

  if ("FEOW_ID" %in% names(ws_data_w)) {
    # Scale ranks within FEOW to number of FEOW
    ws_data_w <- ws_data_w |>
      # protection_score ranked within FEOW
      dplyr::ungroup() |>
      dplyr::group_by(FEOW_ID) |>
      dplyr::arrange(FEOW_ID, -protection_score) |>
      dplyr::mutate(Prot_rank_feow = dplyr::row_number()) |>
      # restoration_score ranked within FEOW
      dplyr::ungroup() |>
      dplyr::group_by(FEOW_ID) |>
      dplyr::arrange(FEOW_ID, -restoration_score) |>
      dplyr::mutate(Rest_rank_feow = dplyr::row_number()) |>
      # SAR_score ranked within FEOW
      dplyr::ungroup() |>
      dplyr::group_by(FEOW_ID) |>
      dplyr::ungroup() |>
      dplyr::arrange(FEOW_ID, -SAR_score) |>
      dplyr::mutate(SAR_rank_feow = dplyr::row_number()) |>
      # AIS_score ranked within FEOW
      dplyr::ungroup() |>
      dplyr::group_by(FEOW_ID) |>
      dplyr::arrange(FEOW_ID, -AIS_score) |>
      dplyr::mutate(AIS_rank_feow = dplyr::row_number()) |>
      # Scale within FEOW
      dplyr::ungroup() |>
      dplyr::group_by(FEOW_ID) |>
      dplyr::mutate(
        Prot_rank_feow_scaled = scale_rank(Prot_rank_feow),
        Rest_rank_feow_scaled = scale_rank(Rest_rank_feow),
        SAR_rank_feow_scaled = scale_rank(SAR_rank_feow),
        AIS_rank_feow_scaled = scale_rank(AIS_rank_feow)
      )
  } else {
    # scale all value 
    ws_data_w <- ws_data_w |>
      dplyr::mutate(
        Prot_rank_scaled = scale_rank(protection_score),
        Rest_rank_scaled = scale_rank(restoration_score),
        SAR_rank_scaled = scale_rank(SAR_score),
        AIS_rank_scaled = scale_rank(AIS_score)
      )
  }

  ws_data_w
}