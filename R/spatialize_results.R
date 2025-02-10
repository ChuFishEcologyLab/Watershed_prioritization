#' Generates a sf object including results.
#'
#' @param x `[data.frame]`\cr A data frame including the results.
#'
#' @details
#' Use the number of rows to determine whether the geometry of Lake Erie or the
#' entire Canada should be used.
#'
#' @export
spatialize_results <- function(x) {
    if (nrow(x) > 1e3) {
        m_path <- path_input_data("map6.gpkg")
    } else {
        m_path <- path_input_data("map5.gpkg", lvl = "H12_LakeErie")
    }
    m_path |>
        sf::read_sf() |>
        dplyr::inner_join(x, by = dplyr::join_by(HYBAS_ID, FEOW_ID))
}