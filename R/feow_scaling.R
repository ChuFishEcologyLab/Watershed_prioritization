#' FEOW scaling
#'
#' @export

feow_scaling <- function() {
    # scaling ranks level 6 hydrobasins within FEOWs
    # load data
    hyb6r <- path_input_data("level6ranks.csv") |>
        readr::read_csv()

    hyb6r <- hyb6r |>
        dplyr::group_by(FEOW_ID) |>
        dplyr::mutate(
            Prot_feow_scaled = scale_feow(Prot_rank_feow),
            Rest_feow_scaled = scale_feow(Rest_rank_feow),
            SAR_feow_scaled = scale_feow(SAR_rank_feow),
            AIS_feow_scaled = scale_feow(AIS_rank_feow)
        )

    ## export data
    readr::write_csv(hyb6r, file = path_output_data("hyb6feowscaled.csv"))

    hyb6r
}

# helper function
scale_feow <- function(x, mx) {
    ((x - 1) / max(x - 1)) * 99 + 1
}