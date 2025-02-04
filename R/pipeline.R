#' Pipeline
#' 
#' This function runs all the steps the reproduce the analysis.
#' 
#' @export

run_pipeline <- function() {
    #------------------------
    cli::cli_progress_step("applying weight")
    suppressMessages(apply_weight())
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("Compute rarity index")
    suppressMessages(compute_rarity_index())
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("Generate Priorization data set")
    suppressMessages(priorization())
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("Scaling")
    suppressMessages(feow_scaling())
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("Results")
    get_results()
    cli::cli_progress_done()
}