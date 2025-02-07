#' Pipeline
#'
#' This function runs all the steps the reproduce the analysis.
#'
#' @export

run_pipeline <- function() {
    op <- options("readr.show_progress")
    options("readr.show_progress" = FALSE)
    on.exit(options("readr.show_progress" = op))
    # ======================== CANADA
    #------------------------
    cli::cli_progress_step("-> Generate watershed prioritization dataset for Canada")
    can_data <- generate_canada_dataset() |>
        apply_weights()
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("-> Plot co-author weightings")
    plot_weightings()
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("-> Results")
    generate_canada_results_set(can_data)
    cli::cli_progress_done()
    #-----------------------

    # ======================== Lake Erie
    cli::cli_progress_step("--> Generate watershed prioritization dataset for Lake Erie")
    suppressMessages(generate_lake_erie_dataset())
    cli::cli_progress_done()
    #-----------------------
}