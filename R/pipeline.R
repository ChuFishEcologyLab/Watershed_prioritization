#' Pipeline
#' 
#' This function runs all the steps the reproduce the analysis.
#' 
#' @export

run_pipeline <- function() {   
    # ======================== CANADA  
    #------------------------
    cli::cli_progress_step("Generate watershed priorization dataset for Canada")
    suppressMessages(generate_canada_dataset())
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("Applying co-author weight")
    suppressMessages(apply_weight())
    cli::cli_progress_done()
    #------------------------
    cli::cli_progress_step("Results")
    get_results()
    cli::cli_progress_done()
    #-----------------------

    # ======================== Lake Erie
    cli::cli_progress_step("Generate watershed priorization dataset for Lake Erie")
    suppressMessages(generate_lake_erie_dataset())
    cli::cli_progress_done()
    #-----------------------


}