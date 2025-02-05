# path helpers

path_input_data <- function(filename) {
    fs::path_package("watershedPrioritization", "extdata", filename)
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