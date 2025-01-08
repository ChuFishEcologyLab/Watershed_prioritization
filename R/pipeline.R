#' Pipeline
#' 
#' @export

run_pipeline <- function() {
    apply_weight()
    priorization_6()
    compute_rarity_index()
    feow_scaling()
    results()
}