#' Plot input variables
#'
#' Plot the six input variables used to compute the the priority score of
#' watersheds.
#'
#' @param map `[sf]`\cr  Map that include the geometries of the watersheds 
#' along with the prioritization input variables (normalized).
#' @param filename `[character string]`\cr Name of the output file (passed to
#' [ggplot2::ggsave()]).
#'
#' @return
#' The function plots the figure and returns `TRUE` invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' generate_canada_dataset() |>
#'     apply_weights() |>
#'     spatialize_results() |>
#'     plot_input_variables()
#' }
plot_input_variables <- function(map, filename = "normalized_index_values.png") {
    S2_A <- plot_variable(map, WSI_n)
    S2_B <- plot_variable(map, SARI_n)
    S2_C <- plot_variable(map, Fish_richness_n)
    S2_D <- plot_variable(map, Priority_n)
    S2_E <- plot_variable(map, FBCI_n)
    S2_F <- plot_variable(map, CCI_n)

    ggpubr::ggarrange(
        S2_A, S2_B,
        S2_C, S2_D,
        S2_E, S2_F,
        labels = c("Watershed stress", "Species at risk", "Species richness", "Species rarity", "Community change", "Climate change"),
        font.label = list(size = 10, face = "plain", color = "black"),
        hjust = -0.15, vjust = 1.8,
        ncol = 2, nrow = 3, common.legend = TRUE,
        legend = "bottom"
    ) # Write the grid.arrange in the file
    ggsave(path_output_fig(filename),
        width = 7.5, height = 9, units = "in",
        dpi = 300
    )

    invisible(TRUE)
}


plot_variable <- function(map, input) {
    ggplot() +
        geom_sf(data = map, aes(fill = {{ input }}, col = {{ input }}), alpha = 1) +
        viridis::scale_color_viridis(option = "viridis") +
        viridis::scale_fill_viridis(
            option = "viridis",
            breaks = c(0, 25, 50, 75, 100)
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