# Results used to be embedded in map6 and directly used in the old analysis
# so the analysis was not actually reproducible, only the figure part of it.
sf::st_read("inst/Analysis/map6_o.gpkg") |>
    dplyr::select(HYBAS_ID, FEOW_ID, corresponding.HYBAS5) |>
    sf::st_write("inst/extdata/H6_Canada/map6.gpkg")
# empty map6