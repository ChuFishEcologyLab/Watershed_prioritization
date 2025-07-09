# requires to run `run_pipeline()`
library(sf)
sf_use_s2(TRUE)

# weighting means
weights <- path_input_data("Co_author_weightings.csv") |>
    readr::read_csv(show_col_types = FALSE)
n_expert <- nrow(weights)
pdata1 <- weights[, c(1:6)]
pdata1 <- tidyr::pivot_longer(pdata1, cols = starts_with("Weight for"))
pdata1$name <- rep(c(
    "Watershed stress", "Community change", "Climate change",
    "Species at risk", "Species richness", "Species rarity"
), n_expert)
sumpdata1 <- pdata1 |>
    dplyr::group_by(name) |>
    dplyr::summarize(med = stats::median(value))
print(sumpdata1)


##
s_map6 <- st_read("output_data/map6.gpkg") |>
    st_simplify(dTolerance = 1000) |>
    st_transform(crs = st_crs(4326))

s_map6 |>
    st_write("ShinyApp/s_map6.gpkg", append = FALSE)


##
s_mapl <- st_read("output_data/lakeerie.gpkg") |>
    st_transform(crs = st_crs(4326))

s_mapl |>
    st_write("ShinyApp/s_mapl.gpkg", append = FALSE)
