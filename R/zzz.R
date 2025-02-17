# path helpers

path_input_data <- function(filename, lvl = "H6_Canada") {
    fs::path_package("watershedPrioritization", "extdata", lvl, filename)
}

path_output_data <- function(filename) {
    fs::dir_create("output_data")
    fs::path("output_data", filename)
}

path_output_fig <- function(filename) {
    fs::dir_create("figs/v2")
    fs::path("figs/v2", filename)
}


# scaling helper
scale_rank <- function(x, mx) {
    stopifnot(length(x) > 1)
    ((x - min(x)) / (max(x) - min(x))) * 99 + 1
}
# NB: min(x) = 1 as we use rank; scale between 1 and 100

# helper function
scale_min_max <- function(x) {
    stopifnot(length(x) > 1)
    100 * (x - min(x)) / (max(x) - min(x))
}
# NB: scale between 0 and 100


utils::globalVariables(c(
    "AIS_rank_feow", "AIS_rank_feow_scaled", "AIS_score", "CCI", "CCI_n", 
    "Climate", "Co_author", "FBCI", "FBCI_n", "FEOW_ID", "Fish_priority", 
    "Fish_richness", "Fish_richness_n", "H12", "H12CountFishMusselSAR", 
    "H12CuThreat", "H12FwVel2452050", "HYBAS6_ID", "HYBAS_ID", "Jaccard.D", 
    "PC1", "PC2", "Priority_n", "Prot_rank_feow", "Prot_rank_feow_scaled", 
    "Protected_area", "Qi", "Rest_rank_feow", "Rest_rank_feow_scaled", "SARI", 
    "SARI_n", "SAR_rank_feow", "SAR_rank_feow_scaled", "SAR_score", "Stress", 
    "WSI", "WSI_n", "cor", "fills", "last_col", "med", "name", "objective", 
    "perc_overlap", "protection_score", "restoration_score", "starts_with", 
    "value", "wsh_fill", "xmaxs", "xmins", "ymaxs", "ymins", 
    "AIS_rank_scaled", "Prot_rank_scaled", "Rest_rank_scaled", "SAR_rank_scaled"
))