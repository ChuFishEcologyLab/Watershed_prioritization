#' FEOW scaling
#' 
#' Scaling ranks level 6 hydrobasins within canadian FEOWs (Freshwater 
#' Ecoregions of the World).
#' 
#' @details 
#' The scaling enables comparison between regions, considering that regions 
#' have varying numbers of watersheds and, consequently, different value ranges 
#' for ranking.
#'
#' @export

feow_scaling <- function() {

    hyb6r <- path_input_data("level6ranks.csv") |>
        readr::read_csv()

    hyb6r <- hyb6r |>
        dplyr::group_by(FEOW_ID) |>
        dplyr::mutate(
            Prot_feow_scaled = scale_rank(Prot_rank_feow),
            Rest_feow_scaled = scale_rank(Rest_rank_feow),
            SAR_feow_scaled = scale_rank(SAR_rank_feow),
            AIS_feow_scaled = scale_rank(AIS_rank_feow)
        )

    readr::write_csv(hyb6r, file = path_output_data("hyb6feowscaled.csv"))
    hyb6r
}

