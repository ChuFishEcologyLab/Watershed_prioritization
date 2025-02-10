#' Plot scores
#'
#' Plot the four scores based on the six input variables used to compute the
#' the priority score of watersheds.
#'
#' @param map Map that include the geometries of the watersheds along with the
#' prioritization input variables (normalized).
#' @param filename `[character string]`\cr Name of the output file (passed to
#' [ggplot2::ggsave()]).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' map <- generate_canada_dataset() |>
#'     apply_weights() |>
#'     spatialize_results() |>
#' plot_scores(map)
#' plot_scores_feow(map)
#' }
#'
plot_scores <- function(map, filename = "national_priorities.png") {
    map$rank <- rank(-map$protection_score)
    A <- plot_score(map, rank)
    map$rank <- rank(-map$restoration_score)
    B <- plot_score(map, rank)
    map$rank <- rank(-map$SAR_score)
    C <- plot_score(map, rank)
    map$rank <- rank(-map$AIS_score)
    D <- plot_score(map, rank)

    ggpubr::ggarrange(A, B, C, D,
        ncol = 2, nrow = 2,
        common.legend = TRUE,
        labels = c("Area-based protection", "Habitat restoration", "Species at risk\n management", "Invasive species\n management"),
        font.label = list(size = 10, face = "plain", color = "black"),
        hjust = -0.25, vjust = 1.8,
        legend = "bottom"
    )
    ggsave(path_output_fig("national_priorities.png"), width = 8.5, height = 6, units = "in", dpi = 300) # Open a new png file
}



#' @describeIn plot_scores Plot scores within FEOW.
#'
#' @export

plot_scores_feow <- function(map, filename = "priorities_by_feow.png") {
    A1 <- plot_score(map, Prot_rank_feow_scaled)
    B1 <- plot_score(map, Rest_rank_feow_scaled)
    C1 <- plot_score(map, SAR_rank_feow_scaled)
    D1 <- plot_score(map, AIS_rank_feow_scaled)

    ggpubr::ggarrange(A1, B1, C1, D1,
        ncol = 2, nrow = 2, common.legend = TRUE,
        labels = c("Area-based protection", "Habitat restoration", "Species at risk\n management", "Invasive species\n management"),
        font.label = list(size = 10, face = "plain", color = "black"),
        hjust = -0.25, vjust = 1.8,
        legend = "bottom"
    ) # Write the grid.arrange in the file
    ggsave(path_output_fig(filename), width = 8.5, height = 6, units = "in", dpi = 300) # Open a new png file

    invisible(TRUE)
}


plot_score <- function(map, score) {
    vc_score <- map |>
        as.data.frame() |>
        dplyr::select({{ score }}) |>
        dplyr::pull()
    ggplot() +
        geom_sf(
            data = map,
            aes(fill = {{ score }}, col = {{ score }}),
            alpha = 1
        ) +
        viridis::scale_color_viridis(
            guide = "none", end = 0.9, direction = -1,
            option = "magma"
        ) +
        viridis::scale_fill_viridis(
            alpha = 1, end = 0.9, direction = -1, option = "magma",
            breaks = c(1, stats::median(vc_score), max(vc_score)),
            labels = c("High", "Medium", "Low")
        ) +
        theme_minimal() +
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )
}