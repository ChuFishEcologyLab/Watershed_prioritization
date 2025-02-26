#'  Compare Spatial overlap of high priority watersheds for 2 variables
#'
#' @param map `[sf]`\cr  Map that include the geometries of the watersheds
#' along with the prioritization input variables (normalized).
#' @param var1,var2 `[var]`\cr Variables to be compared.
#' @param xlab,ylab `[character string]`\cr axis labels.
#' @param feow `[sf]`\cr Geometries of the Freshwater Ecoregions of the World (FEOW).
#' @param filename `[character string]`\cr Name of the output file (passed to
#' [ggplot2::ggsave()]).
#' 
#' @details 
#' Used for: 
#' * Spatial overlap of high priority watersheds for habitat restoration and 
#' area-based protection;
#' * Species at risk management and invasive species management.
#'
#' @return
#' The function plots the figure and returns `TRUE` invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' feow <- path_input_data("FEOW_CAN_Extent/FEOW__CAN_Extent.shp") |>
#'     sf::read_sf()
#' map <- generate_canada_dataset() |>
#'     apply_weights() |>
#'     spatialize_results() |>
#'     plot_comparison(Prot_rank_feow_scaled, Rest_rank_feow_scaled, feow, Prot_rank_feow_scaled)
#' }
#'
plot_comparison <- function(
    map,
    feow,
    var1,
    var2,
    xlab = "Priority for area-based protection",
    ylab = "Priority for habitat restoration",
    filename = "protection_vs_restoration.png") {
    # PART 1
    pal <- c(
        viridis::viridis_pal(option = "magma")(20)[20],
        viridis::viridis_pal(option = "viridis")(20)[17],
        viridis::viridis_pal(option = "magma")(20)[17],
        "grey90"
    )
    #
    perc1 <- stats::quantile(map |> dplyr::pull({{ var1 }}))
    perc2 <- stats::quantile(map |> dplyr::pull({{ var2 }}))
    #
    rects <- dplyr::tibble(
        xmins = c(1, 1, perc1[2], perc1[2]),
        xmaxs = c(perc1[2], perc1[2], perc1[5], perc1[5]),
        ymins = c(1, perc2[2], 1, perc2[2]),
        ymaxs = c(perc2[2], perc2[5], perc2[2], perc2[5]),
        fills = c("a", "b", "c", "d")
    )

    p1 <- ggplot() +
        geom_rect(
            data = rects,
            aes(xmin = xmins, ymin = ymins, xmax = xmaxs, ymax = ymaxs, fill = fills)
        ) +
        geom_point(data = map, aes(x = {{ var1 }}, y = {{ var2 }}), size = 0.8) +
        scale_fill_manual(values = pal) +
        xlab(xlab) +
        ylab(ylab) +
        scale_y_reverse(
            breaks = c(11, perc2[5] - 10), labels = c("high", "low")
        ) +
        scale_x_reverse(
            breaks = c(11, perc1[5] - 10), labels = c("high", "low")
        ) +
        theme_minimal() +
        theme(
            legend.position = "none",
            panel.grid = element_blank(),
            axis.text.x = element_text(vjust = 4),
            axis.text.y = element_text(vjust = -3, angle = 90),
            axis.ticks = element_blank()
        )

    # PART 2
    map <- map |>
        dplyr::mutate(
            wsh_fill = dplyr::case_when(
                {{ var1 }} < perc1[2] & {{ var2 }} < perc2[2] ~ "a",
                {{ var1 }} < perc1[2] ~ "b",
                {{ var2 }} < perc2[2] ~ "c",
                .default = "d"
            )
        )
    length(which(map$wsh_fill == "a")) / length(which(map$wsh_fill %in% c("a", "b", "c")))

    m1 <- ggplot() +
        geom_sf(data = map, aes(fill = wsh_fill, col = wsh_fill), alpha = 1) +
        scale_color_manual(
            guide = "none",
            values = pal
        ) +
        scale_fill_manual(
            values = pal,
            guide = "none"
        ) + 
        geom_sf(data = feow, fill = "transparent", color = "black", linewidth = 0.1) +
        theme_minimal() +
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )

    p1 + m1
    ggsave(
        path_output_fig(filename),
        width = 8.5, height = 4, units = "in", dpi = 300
    )

    invisible(TRUE)
}


#' @describeIn plot_comparison PCA to vizualize correlations among scaled variales.
#'
#' @export

plot_comparison_pca <- function(map) {
    newdata <- map |>
        dplyr::select(dplyr::ends_with("scaled")) |>
        sf::st_drop_geometry()
    pca1 <- stats::prcomp(newdata, scale. = FALSE)
    utils::head(pca1$rotation)
    scores <- dplyr::as_tibble(pca1$x)

    ggplot() +
        geom_point(data = scores, aes(x = PC1, y = PC2))
}