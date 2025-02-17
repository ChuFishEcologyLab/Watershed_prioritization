#' Pipeline
#'
#' This function runs all the steps the reproduce the analysis.
#'
#' @param write a logical, should results be written (2 geopackages)?
#'
#' @import ggplot2 patchwork
#'
#' @export

run_pipeline <- function(write = TRUE) {
    op <- options("readr.show_progress")
    options("readr.show_progress" = FALSE)
    on.exit(options("readr.show_progress" = op))
    # ======================== CANADA
    suppressMessages({
        feow <- path_input_data("FEOW_CAN_Extent/FEOW__CAN_Extent.shp") |>
            sf::read_sf()
        map5 <- path_input_data("map5.gpkg") |>
            sf::read_sf()
    })
    #------------------------
    cli::cli_progress_step("Generate watershed prioritization dataset for Canada")
    map_can <- generate_canada_dataset() |>
        apply_weights() |>
        spatialize_results()
    cli::cli_progress_done()
    if (write) {
        cli::cli_progress_step("Writing prioritization dataset for Canada")
        sf::st_write(map_can, path_output_data("map6.gpkg"),
            quiet = TRUE, append = FALSE
        )
        cli::cli_progress_done()
    }
    #------------------------
    cli::cli_progress_step("Plot co-author weightings")
    plot_weightings()
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("Plot input variables")
    plot_input_variables(map_can)
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("Plot scores")
    plot_scores(map_can)
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("Plot scores by FEOW")
    plot_scores_feow(map_can)
    cli::cli_progress_done()
    #-----------------------
    cli::cli_progress_step("Plot comparison protection vs restoration")
    plot_comparison(
        map_can,
        feow,
        Prot_rank_feow_scaled,
        Rest_rank_feow_scaled
    )
    cli::cli_progress_done()
    #-----------------------
    cli::cli_progress_step("Plot comparison SAR vs AIS")
    plot_comparison(
        map_can,
        feow,
        SAR_rank_feow_scaled,
        AIS_rank_feow_scaled,
        "Priority for species at risk management",
        "Priority for invasive species management",
        "SAR_vs_AIS.png"
    )
    cli::cli_progress_done()
    # not used
    # plot_comparison_pca(map_can)
    #-----------------------
    cli::cli_progress_step("Plot scale dependency")
    plot_scale_dependency(map_can, map5)
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("Results")
    cor1 <- stats::cor(
        map_can$Prot_rank_feow_scaled,
        map_can$Rest_rank_feow_scaled,
        method = "spearman"
    )
    cli::cat_rule(paste(
        "Correlations Protection vs Restoration:",
        cor1
    ))
    cor2 <- stats::cor(
        map_can$SAR_rank_feow_scaled,
        map_can$AIS_rank_feow_scaled,
        method = "spearman"
    )
    cli::cat_rule(paste(
        "Correlations Species at risk management versus invasive species management: ",
        cor2
    ))
    cli::cli_progress_done()
    #-----------------------


    # ======================== Lake Erie
    cli::cli_progress_step("-Generate watershed prioritization dataset for Lake Erie")
    suppressMessages(map_le <- generate_lake_erie_dataset())
    cli::cli_progress_done()
    #-----------------------
}