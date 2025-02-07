#' Co-author weighting plot
#'
#' Plot values for weighting each of six watershed indices towards four 
#' conservation objectives.
#'
#' @details
#' Black dots show the suggested weights from each of 8 co-authors for the
#' respective index (y-axis position) and objective (panel). Large red dots
#' indicate the median value across co-authors, which was used for downstream
#' analyses. Positive values (i.e. > 0 ) indicate that watersheds with high
#' values of the index should be selected as priority for the objective, while
#' negative values indicate that watersheds with low values of the index should
#' be selected as priorities.
#'
#' @export
#'

plot_weightings <- function() {
    weights <- path_input_data("Co_author_weightings.csv") |>
        readr::read_csv(show_col_types = FALSE)

    progress_step_fig("Co-authors Weightings")
    pdata1 <- weights[, c(1:6)]
    pdata1 <- tidyr::pivot_longer(pdata1, cols = starts_with("Weight for"))
    pdata1$name <- rep(c(
        "Watershed stress", "Community change", "Climate change",
        "Species at risk", "Species richness", "Species rarity"
    ), 8)
    sumpdata1 <- pdata1 |>
        dplyr::group_by(name) |>
        dplyr::summarize(med = median(value))

    p1 <- ggplot() +
        geom_hline(aes(yintercept = 0), color = "black", linetype = 2) +
        ggbeeswarm::geom_beeswarm(data = pdata1, aes(x = name, y = value), size = 2) +
        geom_point(data = sumpdata1, aes(x = name, y = med), color = "red", alpha = 0.7, size = 6) +
        scale_y_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-5.5, 5.5)) +
        ylab("Weight") +
        xlab("") +
        coord_flip() +
        theme_minimal() +
        ggtitle("Area-based protection") +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )

    pdata2 <- weights[, c(7:12)]
    pdata2 <- tidyr::pivot_longer(pdata2, cols = starts_with("Weight for"))
    pdata2$name <- rep(c(
        "Watershed stress", "Community change", "Climate change",
        "Species at risk", "Species richness", "Species rarity"
    ), 8)
    sumpdata2 <- pdata2 |>
        dplyr::group_by(name) |>
        dplyr::summarize(med = median(value))

    p2 <- ggplot() +
        geom_hline(aes(yintercept = 0), color = "black", linetype = 2) +
        ggbeeswarm::geom_beeswarm(data = pdata2, aes(x = name, y = value), size = 2) +
        geom_point(data = sumpdata2, aes(x = name, y = med), color = "red", alpha = 0.7, size = 6) +
        scale_y_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-5.5, 5.5)) +
        ylab("Weight") +
        xlab("") +
        coord_flip() +
        theme_minimal() +
        ggtitle("Habitat restoration") +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )

    pdata3 <- weights[, c(13:18)]
    pdata3 <- tidyr::pivot_longer(pdata3, cols = starts_with("Weight for"))
    pdata3$name <- rep(c(
        "Watershed stress", "Community change", "Climate change",
        "Species at risk", "Species richness", "Species rarity"
    ), 8)
    sumpdata3 <- pdata3 |>
        dplyr::group_by(name) |>
        dplyr::summarize(med = median(value))

    p3 <- ggplot() +
        geom_hline(aes(yintercept = 0), color = "black", linetype = 2) +
        ggbeeswarm::geom_beeswarm(data = pdata3, aes(x = name, y = value), size = 2) +
        geom_point(data = sumpdata3, aes(x = name, y = med), color = "red", alpha = 0.7, size = 6) +
        scale_y_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-5.5, 5.5)) +
        ylab("Weight") +
        xlab("") +
        coord_flip() +
        theme_minimal() +
        ggtitle("Species at risk management") +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )

    pdata4 <- weights[, c(19:24)]
    pdata4 <- tidyr::pivot_longer(pdata4, cols = starts_with("Weight for"))
    pdata4$name <- rep(c(
        "Watershed stress", "Community change", "Climate change",
        "Species at risk", "Species richness", "Species rarity"
    ), 8)
    sumpdata4 <- pdata4 |>
        dplyr::group_by(name) |>
        dplyr::summarize(med = median(value))

    p4 <- ggplot() +
        geom_hline(aes(yintercept = 0), color = "black", linetype = 2) +
        ggbeeswarm::geom_beeswarm(data = pdata4, aes(x = name, y = value), size = 2) +
        geom_point(data = sumpdata4, aes(x = name, y = med), color = "red", alpha = 0.7, size = 6) +
        scale_y_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-5.5, 5.5)) +
        ylab("Weight") +
        xlab("") +
        coord_flip() +
        theme_minimal() +
        ggtitle("Invasive species management") +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )

    (p1 + p2) / (p3 + p4)
    ggsave(
        path_output_fig("coauthor_weightings.png"), 
        width = 10, height = 7.5, units = "in", dpi = 300
    )

    cli::cli_progress_done()
}
