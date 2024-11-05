# helpers 

path_input_data  <- function(filename) {
    fs::path_package("watershedPrioritization", "extdata", filename)
}

path_output_data  <- function(filename) {
    fs::dir_create("output_data")
    fs::path("output_data", filename)
}