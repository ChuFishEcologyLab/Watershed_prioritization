# Data found here; using 2023
# https://data-donnees.az.ec.gc.ca/data/species/protectrestore/canadian-protected-conserved-areas-database/Database

#------------- GET DATA
download.file(
    "https://data-donnees.az.ec.gc.ca/api/file?path=%2Fspecies%2Fprotectrestore%2Fcanadian-protected-conserved-areas-database%2FDatabases%2FProtectedConservedArea_2023.zip",
    "inst/extdata/ProtectedAreas/ProtectedConservedArea_2023.zip"
)

unzip(
    "inst/extdata/ProtectedAreas/ProtectedConservedArea_2023.zip",
    exdir = "inst/extdata/ProtectedAreas/ProtectedConservedArea_2023"
)


#------------- READ AND COMPUTE
# takes a few minutes to run
cpa <- terra::vect("inst/extdata/ProtectedAreas/ProtectedConservedArea_2023/ProtectedConservedArea_2023/ProtectedConservedArea_2023.gdb")
hb6 <- terra::vect("inst/extdata/map6.gpkg") |>
    terra::project(cpa)

protected_area_overlap <- as.data.frame(hb6[, "HYBAS_ID"])
protected_area_overlap$area_m2 <- terra::expanse(hb6)

geom_intersect <- terra::intersect(cpa[, "ZONE_ID"], hb6[, "HYBAS_ID"])

df_intersect <- as.data.frame(geom_intersect)
df_intersect$area_m2 <- terra::expanse(geom_intersect)


protected_area_overlap <- protected_area_overlap |>
    dplyr::inner_join(
        df_intersect |>
            dplyr::group_by(HYBAS_ID) |>
            dplyr::summarise(overlap_area_m2 = sum(area_m2))
    ) |>
    dplyr::mutate(
        perc_overlap = 100 * overlap_area_m2 / area_m2
    )

# checks
# jj  <- df_intersect[df_intersect$HYBAS_ID == 8060043200, ]
# cpa[cpa$ZONE_ID %in% jj$ZONE_ID, ]
# ll$Shape_Area |> sum()
# hb6[hb6$HYBAS_ID == 8060043200, ]  |> terra::expanse()
# #
# hb6[hb6$HYBAS_ID == 8060043200, ] |> plot()
# plot(geom_intersect[geom_intersect$HYBAS_ID == 8060043200, ], col = "#99999999", add = TRUE)

#------------- WRITE CSV
protected_area_overlap |>
    readr::write_csv("inst/extdata/protected_area_overlap.csv")