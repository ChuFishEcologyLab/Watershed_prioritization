#' Generates the set of figures for Canada
#'
#' @param can_data Canada dataset see [generate_canada_dataset()].
#' 
#' @import ggplot2 patchwork
#'
#' @export

generate_canada_results_set <- function(can_data) {
    suppressMessages({
        weights <- path_input_data("Co_author_weightings.csv") |>
            readr::read_csv(show_col_types = FALSE)
        feow <- path_input_data("FEOW_CAN_Extent/FEOW__CAN_Extent.shp") |>
            sf::read_sf()
        map5 <- path_input_data("map5.gpkg") |>
            sf::read_sf()
        # Load map and join results
        map6 <- path_input_data("map6.gpkg") |>
            sf::read_sf() |>
            dplyr::inner_join(can_data, by = dplyr::join_by(HYBAS_ID, FEOW_ID))
    })



    ###########
    # Hydrobasin 6 index values
    ############
    cli::cli_progress_step("now drawing fig 2", "fig 2 done", "fig 2 failed")
    S2_A <- ggplot() +
        geom_sf(data = map6, aes(fill = WSI_n, col = WSI_n), alpha = 1) +
        viridis::scale_color_viridis(option = "viridis") +
        viridis::scale_fill_viridis(
            option = "viridis",
            breaks = c(0, 25, 50, 75, 100)
        ) +
        theme_minimal() +
        # ggtitle("Watershed stress")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )

    S2_B <- ggplot() +
        geom_sf(data = map6, aes(fill = SARI_n, col = SARI_n), alpha = 1) +
        viridis::scale_color_viridis(option = "viridis") +
        viridis::scale_fill_viridis(
            option = "viridis",
            breaks = c(0, 25, 50, 75, 100)
        ) +
        theme_minimal() +
        # ggtitle("Fish species at risk richness")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )

    S2_C <- ggplot() +
        geom_sf(data = map6, aes(fill = Fish_richness_n, col = Fish_richness_n), alpha = 1) +
        viridis::scale_color_viridis(option = "viridis") +
        viridis::scale_fill_viridis(
            option = "viridis",
            breaks = c(0, 25, 50, 75, 100)
        ) +
        theme_minimal() +
        # ggtitle("Fish species richness")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )

    S2_D <- ggplot() +
        geom_sf(data = map6, aes(fill = Priority_n, col = Priority_n), alpha = 1) +
        viridis::scale_color_viridis(option = "viridis") +
        viridis::scale_fill_viridis(
            option = "viridis",
            breaks = c(0, 25, 50, 75, 100)
        ) +
        theme_minimal() +
        # ggtitle("Fish rarity")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )

    S2_E <- ggplot() +
        geom_sf(data = map6, aes(fill = FBCI_n, col = FBCI_n), alpha = 1) +
        viridis::scale_color_viridis(option = "viridis") +
        viridis::scale_fill_viridis(
            option = "viridis",
            breaks = c(0, 25, 50, 75, 100)
        ) +
        theme_minimal() +
        # ggtitle("Fish biodiversity change")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )

    S2_F <- ggplot() +
        geom_sf(data = map6, aes(fill = CCI_n, col = CCI_n), alpha = 1) +
        viridis::scale_color_viridis(option = "viridis") +
        viridis::scale_fill_viridis(
            option = "viridis",
            breaks = c(0, 25, 50, 75, 100)
        ) +
        theme_minimal() +
        # ggtitle("Climate change")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )


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
    ggsave(path_output_fig("normalized_index_values.png"), width = 7.5, height = 9, units = "in", dpi = 300) # Open a new png file
    cli::cli_progress_done()



    ## ------------------------------
    ## Priorities within ecoregions
    ## ------------------------------
    cli::cli_progress_step("now drawing fig 3", "fig 3 done", "fig 3 failed")
    data <- map6

    #
    A1 <- ggplot() +
        geom_sf(data = data, aes(fill = Prot_rank_feow_scaled, col = Prot_rank_feow_scaled), alpha = 1) +
        viridis::scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
        viridis::scale_fill_viridis(
            alpha = 1, end = 0.9, direction = -1, option = "magma",
            breaks = c(1, median(data$Prot_rank_feow_scaled), max(data$Prot_rank_feow_scaled)),
            labels = c("High", "Medium", "Low")
        ) +
        # geom_sf(data=feow, fill = "transparent", color ="black", linewidth =0.1)+
        theme_minimal() +
        # ggtitle("Area-based protection")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )

    #
    B1 <- ggplot() +
        geom_sf(data = data, aes(fill = Rest_rank_feow_scaled, col = Rest_rank_feow_scaled), alpha = 1) +
        viridis::scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
        viridis::scale_fill_viridis(
            alpha = 1, end = 0.9, direction = -1, option = "magma",
            breaks = c(1, median(data$Rest_rank_feow_scaled), max(data$Rest_rank_feow_scaled)),
            labels = c("High", "Medium", "Low")
        ) +
        # geom_sf(data=feow, fill = "transparent", color ="black", linewidth =0.1)+
        theme_minimal() +
        # ggtitle("Area-based protection")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )

    #
    C1 <- ggplot() +
        geom_sf(data = data, aes(fill = SAR_rank_feow_scaled, col = SAR_rank_feow_scaled), alpha = 1) +
        viridis::scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
        viridis::scale_fill_viridis(
            alpha = 1, end = 0.9, direction = -1, option = "magma",
            breaks = c(1, median(data$SAR_rank_feow_scaled), max(data$SAR_rank_feow_scaled)),
            labels = c("High", "Medium", "Low")
        ) +
        # geom_sf(data=feow, fill = "transparent", color ="black", linewidth =0.1)+
        theme_minimal() +
        # ggtitle("Area-based protection")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )

    #
    D1 <- ggplot() +
        geom_sf(data = data, aes(fill = AIS_rank_feow_scaled, col = AIS_rank_feow_scaled), alpha = 1) +
        viridis::scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
        viridis::scale_fill_viridis(
            alpha = 1, end = 0.9, direction = -1, option = "magma",
            breaks = c(1, median(data$AIS_rank_feow_scaled), max(data$AIS_rank_feow_scaled)),
            labels = c("High", "Medium", "Low")
        ) +
        # geom_sf(data=feow, fill = "transparent", color ="black", linewidth =0.1)+
        theme_minimal() +
        # ggtitle("Area-based protection")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )

    ggpubr::ggarrange(A1, B1, C1, D1,
        ncol = 2, nrow = 2, common.legend = TRUE,
        labels = c("Area-based protection", "Habitat restoration", "Species at risk\n management", "Invasive species\n management"),
        font.label = list(size = 10, face = "plain", color = "black"),
        hjust = -0.25, vjust = 1.8,
        legend = "bottom"
    ) # Write the grid.arrange in the file
    ggsave(path_output_fig("priorities_by_feow.png"), width = 8.5, height = 6, units = "in", dpi = 300) # Open a new png file
    cli::cli_progress_done()


    ## -----------------------------------
    ## Analysis of priorities within FEOW
    ## -----------------------------------

    # map6 |>
    #     st_drop_geometry() |>
    #     ungroup() |>
    #     dplyr::group_by(FEOW_ID) |>
    #     summarise(SAR_p = max(SARI_n)) |>
    #     print(n = Inf)
    # # two ecoregions with no SAR (111, 112)

    # map6 |>
    #     st_drop_geometry() |>
    #     ungroup() |>
    #     dplyr::group_by(FEOW_ID) |>
    #     summarise(SAR_p = max(FBCI_n)) |>
    #     print(n = Inf)




    ###################
    # Hydrobasin 6 national priorities
    #################
    cli::cli_progress_step("now drawing fig 4", "fig 4 done", "fig 4 failed")
    data <- map6
    #
    data$rank <- rank(-data$protection_score)
    A <- ggplot() +
        geom_sf(data = data, aes(fill = rank, col = rank), alpha = 1) +
        viridis::scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
        viridis::scale_fill_viridis(
            alpha = 1, end = 0.9, direction = -1, option = "magma",
            breaks = c(1, median(data$rank), max(data$rank)),
            labels = c("High", "Medium", "Low")
        ) +
        theme_minimal() +
        # ggtitle("Area-based protection")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )
    #
    data$rank <- rank(-data$restoration_score)
    B <- ggplot() +
        geom_sf(data = data, aes(fill = rank, col = rank), alpha = 1) +
        viridis::scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
        viridis::scale_fill_viridis(
            alpha = 1, end = 0.9, direction = -1, option = "magma",
            breaks = c(1, median(data$rank), max(data$rank)),
            labels = c("High", "Medium", "Low")
        ) +
        theme_minimal() +
        # ggtitle("Restoration")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )
    #
    data$rank <- rank(-data$SAR_score)
    C <- ggplot() +
        geom_sf(data = data, aes(fill = rank, col = rank), alpha = 1) +
        viridis::scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
        viridis::scale_fill_viridis(
            alpha = 1, end = 0.9, direction = -1, option = "magma",
            breaks = c(1, median(data$rank), max(data$rank)),
            labels = c("High", "Medium", "Low")
        ) +
        theme_minimal() +
        #  ggtitle("Species at risk management")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )
    #
    data$rank <- rank(-data$AIS_score)
    D <- ggplot() +
        geom_sf(data = data, aes(fill = rank, col = rank), alpha = 1) +
        viridis::scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
        viridis::scale_fill_viridis(
            alpha = 1, end = 0.9, direction = -1, option = "magma",
            breaks = c(1, median(data$rank), max(data$rank)),
            labels = c("High", "Medium", "Low")
        ) +
        theme_minimal() +
        # ggtitle("Invasive species management")+
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
        )

    ggpubr::ggarrange(A, B, C, D,
        ncol = 2, nrow = 2,
        common.legend = TRUE,
        labels = c("Area-based protection", "Habitat restoration", "Species at risk\n management", "Invasive species\n management"),
        font.label = list(size = 10, face = "plain", color = "black"),
        hjust = -0.25, vjust = 1.8,
        legend = "bottom"
    )
    ggsave(path_output_fig("national_priorities.png"), width = 8.5, height = 6, units = "in", dpi = 300) # Open a new png file
    cli::cli_progress_done()




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
    data <- map6
    perc1 <- stats::quantile(data$Prot_rank_feow_scaled)
    perc2 <- stats::quantile(data$Rest_rank_feow_scaled)

    rects <- dplyr::tibble(
        xmins = c(1, 1, perc1[2], perc1[2]),
        xmaxs = c(perc1[2], perc1[2], perc1[5], perc1[5]),
        ymins = c(1, perc2[2], 1, perc2[2]),
        ymaxs = c(perc2[2], perc2[5], perc2[2], perc2[5]),
        fills = c("a", "b", "c", "d")
    )

    p1 <- ggplot() +
        geom_rect(data = rects, aes(xmin = xmins, ymin = ymins, xmax = xmaxs, ymax = ymaxs, fill = fills)) +
        geom_point(data = data, aes(
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

    cor(data$Prot_rank_feow_scaled, data$Rest_rank_feow_scaled, method = "spearman")

    data <- data |>
        dplyr::mutate(wsh_fill = ifelse(
            Prot_rank_feow_scaled < perc1[2] & Rest_rank_feow_scaled < perc2[2], "a",
            ifelse(Prot_rank_feow_scaled < perc1[2], "b",
                ifelse(Rest_rank_feow_scaled < perc2[2], "c", "d")
            )
        ))

    length(which(data$wsh_fill == "a")) / length(which(data$wsh_fill %in% c("a", "b", "c")))

    m1 <- ggplot() +
        geom_sf(data = data, aes(fill = wsh_fill, col = wsh_fill), alpha = 1) +
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
    perc1 <- stats::quantile(data$SAR_rank_feow_scaled)
    perc2 <- stats::quantile(data$AIS_rank_feow_scaled)

    rects <- dplyr::tibble(
        xmins = c(1, 1, perc1[2], perc1[2]),
        xmaxs = c(perc1[2], perc1[2], perc1[5], perc1[5]),
        ymins = c(1, perc2[2], 1, perc2[2]),
        ymaxs = c(perc2[2], perc2[5], perc2[2], perc2[5]),
        fills = c("a", "b", "c", "d")
    )

    stats::cor(data$SAR_rank_feow_scaled, data$AIS_rank_feow_scaled, method = "spearman")

    p2 <- ggplot() +
        geom_rect(data = rects, aes(xmin = xmins, ymin = ymins, xmax = xmaxs, ymax = ymaxs, fill = fills)) +
        geom_point(data = data, aes(
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

    data <- data |>
        dplyr::mutate(wsh_fill = ifelse(
            SAR_rank_feow_scaled < perc1[2] & AIS_rank_feow_scaled < perc2[2], "a",
            ifelse(SAR_rank_feow_scaled < perc1[2], "b",
                ifelse(AIS_rank_feow_scaled < perc2[2], "c", "d")
            )
        ))

    length(which(data$wsh_fill == "a")) / length(which(data$wsh_fill %in% c("a", "b", "c")))


    m2 <- ggplot() +
        geom_sf(data = data, aes(fill = wsh_fill, col = wsh_fill), alpha = 1) +
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
        dplyr::select(ends_with("scaled")) |>
        sf::st_drop_geometry()
    pca1 <- prcomp(newdata, scale. = FALSE)
    head(pca1$rotation)
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

    map6 <- map6 |>
        dplyr::ungroup() |>
        dplyr::mutate(
            protection_score5 = d5$protection_score[match(map6$corresponding.HYBAS5, d5$HYBAS_ID)],
            restoration_score5 = d5$restoration_score[match(map6$corresponding.HYBAS5, d5$HYBAS_ID)],
            SAR_score5 = d5$SAR_score[match(map6$corresponding.HYBAS5, d5$HYBAS_ID)],
            AIS_score5 = d5$AIS_score[match(map6$corresponding.HYBAS5, d5$HYBAS_ID)],
        )

    obj <- c("protection", "restoration", "SAR", "AIS")
    threshold <- seq(0.05, 0.5, 0.01)
    df <- dplyr::tibble(objective = c(), threshold = c(), ratio = c())

    suppressMessages({
        for (i in seq_len(length(obj))) {
            for (j in 1:length(threshold)) {
                var <- paste0(obj[i], "_score")
                tmp <- map6 |>
                    dplyr::ungroup() |>
                    dplyr::group_by(FEOW_ID) |>
                    dplyr::arrange(FEOW_ID, desc(!!var)) |>
                    dplyr::relocate(!!var, .after = last_col()) |>
                    dplyr::top_frac(n = threshold[j])
                map6$level6 <- ifelse(map6$HYBAS_ID %in% tmp$HYBAS_ID, 1, 0)

                var2 <- paste0(obj[i], "_score5")
                tmp <- map6 |>
                    dplyr::ungroup() |>
                    dplyr::group_by(FEOW_ID) |>
                    dplyr::arrange(FEOW_ID, desc(!!var2)) |>
                    dplyr::relocate(!!var2, .after = last_col()) |>
                    dplyr::top_frac(n = threshold[j])
                map6$level5 <- ifelse(map6$HYBAS_ID %in% tmp$HYBAS_ID, 1, 0)

                ratio <- length(which(map6$level6 == 1 & map6$level5 == 0)) / length(which(map6$level6 == 1))

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
