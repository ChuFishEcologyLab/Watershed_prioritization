#' Generates the set of figures for Canada
#'
#' @param map `[sf]`\cr Results for Canada see [generate_canada_dataset()].
#' @param map5 `[sf]`\cr Results for Canada at level 5.
#' @param filename `[character string]`\cr Name of the output file (passed to
#' [ggplot2::ggsave()]).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' generate_canada_dataset() |>
#'     apply_weights() |>
#'     spatialize_results() |>
#'     plot_scale_dependency(map5)
#' }

plot_scale_dependency <- function(map, map5, filename = "scale_dependency.png") {
    d5 <- map5 |>
        sf::st_drop_geometry() |>
        dplyr::ungroup()

    map <- map |>
        dplyr::ungroup() |>
        dplyr::mutate(
            protection_score5 = d5$protection_score[match(map$corresponding.HYBAS5, d5$HYBAS_ID)],
            restoration_score5 = d5$restoration_score[match(map$corresponding.HYBAS5, d5$HYBAS_ID)],
            SAR_score5 = d5$SAR_score[match(map$corresponding.HYBAS5, d5$HYBAS_ID)],
            AIS_score5 = d5$AIS_score[match(map$corresponding.HYBAS5, d5$HYBAS_ID)],
        )

    obj <- c("protection", "restoration", "SAR", "AIS")
    threshold <- seq(0.05, 0.5, 0.01)
    df <- dplyr::tibble(objective = c(), threshold = c(), ratio = c())

    suppressMessages({
        for (i in seq_len(length(obj))) {
            for (j in seq_len(length(threshold))) {
                var <- paste0(obj[i], "_score")
                tmp <- map |>
                    dplyr::ungroup() |>
                    dplyr::group_by(FEOW_ID) |>
                    dplyr::arrange(FEOW_ID, dplyr::desc(!!var)) |>
                    dplyr::relocate(!!var, .after = last_col()) |>
                    dplyr::top_frac(n = threshold[j])
                map$level6 <- ifelse(map$HYBAS_ID %in% tmp$HYBAS_ID, 1, 0)

                var2 <- paste0(obj[i], "_score5")
                tmp <- map |>
                    dplyr::ungroup() |>
                    dplyr::group_by(FEOW_ID) |>
                    dplyr::arrange(FEOW_ID, dplyr::desc(!!var2)) |>
                    dplyr::relocate(!!var2, .after = last_col()) |>
                    dplyr::top_frac(n = threshold[j])
                map$level5 <- ifelse(map$HYBAS_ID %in% tmp$HYBAS_ID, 1, 0)

                ratio <- length(which(map$level6 == 1 & map$level5 == 0)) / length(which(map$level6 == 1))

                to_add <- dplyr::tibble(objective = obj[i], threshold = threshold[j], ratio = ratio)
                df <- dplyr::bind_rows(df, to_add)
            }
        }
    })

    mylabels <- df |>
        dplyr::filter(threshold == 0.5)
    mylabels$ratio[which(mylabels$objective == "SAR")] <- 0.24 # nudge


    s1 <- ggplot(data = df, aes(x = threshold * 100, y = 100 - (ratio * 100), group = objective, color = objective)) +
        geom_line(linewidth = 1.5) +
        theme_minimal() +
        ylab("Percentage of priority watersheds covered by\nprioritization at larger watershed scale") +
        scale_x_continuous(
            name = "Percentage of watersheds prioritized",
            limits = c(0, 59),
            breaks = c(10, 20, 30, 40, 50)
        ) +
        theme(
            legend.position = "none",
            panel.grid.minor = element_blank()
        ) +
        geom_text(
            data = mylabels,
            aes(
                x = 50.5,
                y = 100 - (ratio * 100),
                label = objective,
                color = objective
            ),
            hjust = 0
        ) +
        scale_color_viridis_d(option = "viridis")

    s1
    ggsave(
        path_output_fig(filename),
        width = 5, height = 4, units = "in", dpi = 300
    )

    invisible(TRUE)
}
