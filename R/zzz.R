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
    "AIS_rank_feow", "AIS_rank_feow_scaled", "AIS_rank_scaled", 
    "AIS_score", "CCI", "CCI_n", "Co_author", "cor", "FBCI", "FBCI_n", 
    "FEOW_ID", "fills", "Fish_priority", "Fish_richness", "Fish_richness_n", 
    "H12", "H12CountFishMusselSAR", "H12CuThreat", "H12FwVel2452050", 
    "HYBAS_ID", "HYBAS6_ID", "H6CuThreat", "H6fwvel", "Jaccard.D", "last_col", 
    "med", "name", "objective", "PC1", "PC2", "perc_overlap", "Priority_n", 
    "Prot_rank_feow", "Prot_rank_feow_scaled", "Prot_rank_scaled", 
    "Protected_area", "protection_score", "Qi", "Rest_rank_feow", 
    "Rest_rank_feow_scaled", "Rest_rank_scaled", "restoration_score", 
    "SAR_rank_feow", "SAR_rank_feow_scaled", "SAR_rank_scaled", "SAR_score", 
    "SARI", "SARI_n", "starts_with", "Stress", "value", "wsh_fill", 
    "WSI", "WSI_n", "xmaxs", "xmins", "ymaxs", "ymins"
))