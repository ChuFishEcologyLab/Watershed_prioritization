#' Generates the set of figures for Canada
#'
#' @param map_can `[sf]`\cr Results for Canada see [generate_canada_dataset()].
#' 
#' @import ggplot2 patchwork
#'
#' @export
#' 
#' @examples
#' \dontrun{
#'  generate_canada_dataset() |>
#'      apply_weights() |>
#'      spatialize_results() |>
#'      generate_canada_results_set()
#' }

generate_canada_results_set <- function(map_can) {
    suppressMessages({
        feow <- path_input_data("FEOW_CAN_Extent/FEOW__CAN_Extent.shp") |>
            sf::read_sf()
        map5 <- path_input_data("map5.gpkg") |>
            sf::read_sf()
    })


    ##############
    ### Comparison plots
    ##############
    cli::cli_progress_step("now drawing fig 5", "fig 5 done", "fig 5 failed")
    pal <- c(
        viridis::viridis_pal(option = "magma")(20)[20],
        viridis::viridis_pal(option = "viridis")(20)[17],
        viridis::viridis_pal(option = "magma")(20)[17],
        "grey90"
    )

    #
    data <- map_can
    perc1 <- stats::quantile(map_can$Prot_rank_feow_scaled)
    perc2 <- stats::quantile(map_can$Rest_rank_feow_scaled)

    rects <- dplyr::tibble(
        xmins = c(1, 1, perc1[2], perc1[2]),
        xmaxs = c(perc1[2], perc1[2], perc1[5], perc1[5]),
        ymins = c(1, perc2[2], 1, perc2[2]),
        ymaxs = c(perc2[2], perc2[5], perc2[2], perc2[5]),
        fills = c("a", "b", "c", "d")
    )

    p1 <- ggplot() +
        geom_rect(data = rects, aes(xmin = xmins, ymin = ymins, xmax = xmaxs, ymax = ymaxs, fill = fills)) +
        geom_point(data = map_can, aes(
            x = Prot_rank_feow_scaled,
            y = Rest_rank_feow_scaled
        )) +
        scale_fill_manual(values = pal) +
        xlab("Priority for area-based protection") +
        ylab("Priority for habitat restoration") +
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
            axis.text.y = element_text(angle = 90),
            axis.ticks = element_blank()
        )
    p1

    stats::cor(map_can$Prot_rank_feow_scaled, map_can$Rest_rank_feow_scaled, method = "spearman")

    map_can <- map_can |>
        dplyr::mutate(wsh_fill = ifelse(
            Prot_rank_feow_scaled < perc1[2] & Rest_rank_feow_scaled < perc2[2], "a",
            ifelse(Prot_rank_feow_scaled < perc1[2], "b",
                ifelse(Rest_rank_feow_scaled < perc2[2], "c", "d")
            )
        ))

    length(which(map_can$wsh_fill == "a")) / length(which(map_can$wsh_fill %in% c("a", "b", "c")))

    m1 <- ggplot() +
        geom_sf(data = map_can, aes(fill = wsh_fill, col = wsh_fill), alpha = 1) +
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

    ggpubr::ggarrange(p1, m1, ncol = 2, nrow = 1)
    ggsave(path_output_fig("protection_vs_restoration.png"), width = 8.5, height = 4, units = "in", dpi = 300) # Open a new png file
    cli::cli_progress_done()



    ##
    cli::cli_progress_step("now drawing fig 6", "fig 6 done", "fig 6 failed")
    perc1 <- stats::quantile(map_can$SAR_rank_feow_scaled)
    perc2 <- stats::quantile(map_can$AIS_rank_feow_scaled)

    rects <- dplyr::tibble(
        xmins = c(1, 1, perc1[2], perc1[2]),
        xmaxs = c(perc1[2], perc1[2], perc1[5], perc1[5]),
        ymins = c(1, perc2[2], 1, perc2[2]),
        ymaxs = c(perc2[2], perc2[5], perc2[2], perc2[5]),
        fills = c("a", "b", "c", "d")
    )

    stats::cor(map_can$SAR_rank_feow_scaled, map_can$AIS_rank_feow_scaled, method = "spearman")

    p2 <- ggplot() +
        geom_rect(data = rects, aes(xmin = xmins, ymin = ymins, xmax = xmaxs, ymax = ymaxs, fill = fills)) +
        geom_point(data = map_can, aes(
            x = SAR_rank_feow_scaled,
            y = AIS_rank_feow_scaled
        )) +
        scale_fill_manual(values = pal) +
        xlab("Priority for species at risk management") +
        ylab("Priority for invasive species management") +
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
            axis.text.y = element_text(angle = 90),
            axis.ticks = element_blank()
        )
    p2

    map_can <- map_can |>
        dplyr::mutate(wsh_fill = ifelse(
            SAR_rank_feow_scaled < perc1[2] & AIS_rank_feow_scaled < perc2[2], "a",
            ifelse(SAR_rank_feow_scaled < perc1[2], "b",
                ifelse(AIS_rank_feow_scaled < perc2[2], "c", "d")
            )
        ))

    length(which(map_can$wsh_fill == "a")) / length(which(map_can$wsh_fill %in% c("a", "b", "c")))


    m2 <- ggplot() +
        geom_sf(data = map_can, aes(fill = wsh_fill, col = wsh_fill), alpha = 1) +
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

    ggpubr::ggarrange(p2, m2, ncol = 2, nrow = 1)
    ggsave(path_output_fig("SAR_vs_AIS.png"), width = 8.5, height = 4, units = "in", dpi = 300)
    cli::cli_process_done()


    ## -------------------------
    ## Comparison analysis
    ## --------------------------

    newdata <- data |>
        dplyr::select(dplyr::ends_with("scaled")) |>
        sf::st_drop_geometry()
    pca1 <- stats::prcomp(newdata, scale. = FALSE)
    utils::head(pca1$rotation)
    scores <- dplyr::as_tibble(pca1$x)

    p_comp <- ggplot() +
        geom_point(data = scores, aes(x = PC1, y = PC2))



    ## -------------------------
    ## Investigating scale
    ## --------------------------

    cli::cli_progress_step("now drawing fig 7", "fig 7 done", "fig 7 failed")

    d5 <- map5 |>
        sf::st_drop_geometry() |>
        dplyr::ungroup()

    map_can <- map_can |>
        dplyr::ungroup() |>
        dplyr::mutate(
            protection_score5 = d5$protection_score[match(map_can$corresponding.HYBAS5, d5$HYBAS_ID)],
            restoration_score5 = d5$restoration_score[match(map_can$corresponding.HYBAS5, d5$HYBAS_ID)],
            SAR_score5 = d5$SAR_score[match(map_can$corresponding.HYBAS5, d5$HYBAS_ID)],
            AIS_score5 = d5$AIS_score[match(map_can$corresponding.HYBAS5, d5$HYBAS_ID)],
        )

    obj <- c("protection", "restoration", "SAR", "AIS")
    threshold <- seq(0.05, 0.5, 0.01)
    df <- dplyr::tibble(objective = c(), threshold = c(), ratio = c())

    suppressMessages({
        for (i in seq_len(length(obj))) {
            for (j in 1:length(threshold)) {
                var <- paste0(obj[i], "_score")
                tmp <- map_can |>
                    dplyr::ungroup() |>
                    dplyr::group_by(FEOW_ID) |>
                    dplyr::arrange(FEOW_ID, desc(!!var)) |>
                    dplyr::relocate(!!var, .after = last_col()) |>
                    dplyr::top_frac(n = threshold[j])
                map_can$level6 <- ifelse(map_can$HYBAS_ID %in% tmp$HYBAS_ID, 1, 0)

                var2 <- paste0(obj[i], "_score5")
                tmp <- map_can |>
                    dplyr::ungroup() |>
                    dplyr::group_by(FEOW_ID) |>
                    dplyr::arrange(FEOW_ID, desc(!!var2)) |>
                    dplyr::relocate(!!var2, .after = last_col()) |>
                    dplyr::top_frac(n = threshold[j])
                map_can$level5 <- ifelse(map_can$HYBAS_ID %in% tmp$HYBAS_ID, 1, 0)

                ratio <- length(which(map_can$level6 == 1 & map_can$level5 == 0)) / length(which(map_can$level6 == 1))

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

    ggpubr::ggarrange(s1, ncol = 1, nrow = 1) # Write the grid.arrange in the file
    ggsave(path_output_fig("scale_dependency.png"), width = 5, height = 4, units = "in", dpi = 300) # Open a new png file
    cli::cli_process_done()


    invisible(TRUE)
}
