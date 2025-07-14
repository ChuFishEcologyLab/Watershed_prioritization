#' Generates a sf object including results.
#'
#' @param x `[data.frame]`\cr A data frame including the results.
#' @param scale `[character string]`\cr Either "national" or "lake erie".#
#'
#' @details
#' Use the number of rows to determine whether the geometry of Lake Erie or the
#' entire Canada should be used.
#'
#' @export
spatialize_results <- function(x, scale = c("national", "lake erie")) {
    scale <- match.arg(scale)
    switch(scale,
        "national" = path_input_data("map6.gpkg") |>
            sf::read_sf() |>
            dplyr::inner_join(x, by = dplyr::join_by(HYBAS_ID, FEOW_ID)),
        "lake erie" = path_input_data("lakeErieH12.gpkg", lvl = "H12_LakeErie") |>
            sf::read_sf() |>
            dplyr::inner_join(x, by = dplyr::join_by(HYBAS_ID)),
        stop("Unknown scale")
    )
}