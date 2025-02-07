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

progress_step_fig <- function(id) {
    cli::cli_progress_step(
        cli::pluralize("now drawing fig {id}"),
        cli::pluralize("fig {id} done"),
        cli::pluralize("fig {id} failed")
    )
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
