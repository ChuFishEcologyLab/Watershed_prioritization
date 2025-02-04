#' Pipeline
#' 
#' This function runs all the steps the reproduce the analysis.
#' 
#' @export

run_pipeline <- function() {    
    #------------------------ (not used - data are in extdata)
    cli::cli_progress_step("Compute fish priority index")
    suppressMessages(compute_fish_priority_index())
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("Generate Priorization data set")
    suppressMessages(generate_priorization_data())
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("Applying co-author weight")
    suppressMessages(apply_weight())
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