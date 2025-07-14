#' Pipeline
#'
#' This function runs all the steps the reproduce the analysis.
#'
#' @param write a logical, should results be written (as geopackages)?
#' @param national a logical, should the analysis at the nation scale?
#' @param lake_erie a logical, should the analysis be done for Lake Erie?
#'
#' @import ggplot2 patchwork
#'
#' @export

run_pipeline <- function(write = TRUE, national = TRUE, lake_erie = TRUE) {
    op <- options("readr.show_progress")
    options("readr.show_progress" = FALSE)
    on.exit(options("readr.show_progress" = op))

    #
    if (national) {
        cli::cli_h1("Canada")
        run_pipeline_canada(write = write)
    }

    if (lake_erie) {
        cli::cli_h1("Lake Erie")
        run_pipeline_lake_erie(write = write)
    }
}


# ======================== CANADA
run_pipeline_canada <- function(write = TRUE) {
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
}


# ======================== Lake Erie
run_pipeline_lake_erie <- function(write = TRUE) {
    cli::cli_progress_step("Generate watershed prioritization dataset for Lake Erie")
    suppressMessages({
        map_le <- generate_lake_erie_dataset() |>
            apply_weights() |>
            spatialize_results(scale = "lake erie")
    })
    cli::cli_progress_done()
    if (write) {
        cli::cli_progress_step("Writing prioritization dataset for Lake Erie")
        sf::st_write(map_le, path_output_data("lakeerie.gpkg"),
            quiet = TRUE, append = FALSE
        )
        cli::cli_progress_done()
    }
    #-----------------------
    cli::cli_progress_step("Plot input variables - Lake Erie")
    plot_input_variables(map_le, "LE_normalized_index_values.png")
    cli::cli_progress_done()
    #-----------------------
    cli::cli_progress_step("Plot input variables - Lake Erie")
    plot_input_variables(map_le, "LE_normalized_index_values.png")
    cli::cli_progress_done()
    #-----------------------
    cli::cli_progress_step("Plot scores - Lake Erie")
    plot_scores(map_le, "LE_national_priorities.png")
    cli::cli_progress_done()
    #-----------------------
    cli::cli_progress_step("Plot comparison protection vs restoration - Lake Erie")
    plot_comparison(
        map_le,
        map_le,
        Prot_rank_scaled,
        Rest_rank_scaled,
        filename = "LE_protection_vs_restoration.png"
    )
    cli::cli_progress_done()
    #-----------------------
    cli::cli_progress_step("Plot comparison SAR vs AIS - Lake Erie")
    plot_comparison(
        map_le,
        map_le,
        SAR_rank_scaled,
        AIS_rank_scaled,
        "Priority for species at risk management",
        "Priority for invasive species management",
        "LE_SAR_vs_AIS.png"
    )
    cli::cli_progress_done()
    #-----------------------
}