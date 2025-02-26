#' Plot percentage of protected area per watershed
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
#' map <- generate_canada_dataset() |>
#'     apply_weights() |>
#'     spatialize_results() |>
#'     plot_protected_areas(map)
#' }
#'
plot_protected_areas <- function(map, filename = "protected_areas.png") {
    P <- plot_variable(map, WSI_n) +
        ggtitle("Protected Areas") + 
        theme(
            plot.margin = unit(c(0.5, 0.5, 1, 0.5), "lines")
        )

    ggsave(path_output_fig(filename),
        width = 6, height = 5, units = "in",
        dpi = 300
    )

    invisible(TRUE)
}