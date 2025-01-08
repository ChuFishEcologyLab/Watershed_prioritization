# helpers 

path_input_data  <- function(filename) {
    fs::path_package("watershedPrioritization", "extdata", filename)
}

path_output_data  <- function(filename) {
    fs::dir_create("output_data")
    fs::path("output_data", filename)
}

path_output_fig <- function(filename) {
    fs::dir_create("figs/v2")
    fs::path("figs/v2", filename)
}