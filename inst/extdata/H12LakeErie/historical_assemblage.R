# get H6 

le  <- sf::st_read("inst/extdata/H12LakeErie/lakeErieH12.gpkg")
h6 <- sf::st_read("inst/extdata/map6.gpkg") |>
    sf::st_transform(sf::st_crs(le))
feow <- sf::st_read("inst/extdata/FEOW_CAN_Extent/FEOW__CAN_Extent.shp")  |>
    sf::st_transform(sf::st_crs(le))

path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
    readxl::read_xlsx(sheet = "LEH12_WSIdata")

jj  <- sf::st_intersection(
    le[, "HYBAS_ID"],  
    h6[, "HYBAS_ID"] |>dplyr::rename(HYBAS_ID6 = HYBAS_ID)
) 

kk  <-  jj |> as.data.frame()

kk$area  <- sf::st_area(jj)  

kk |>
    dplyr::group_by(HYBAS_ID)  |>
    dplyr::summarize(area=max(area))


sf::st_within(h6, le)



leb <- le |>
     sf::st_geometry() |>
     sf::st_buffer(0.2)

df_wis <- path_input_data("H12LakeErie/LEH12_wpdata.xlsx") |>
    readxl::read_xlsx(sheet = "LEH12_WSIdata")

plot(leb, col = "#ffffff", border = "transparent")
plot(le |> sf::st_geometry(), add = TRUE)
plot(le |> dplyr::filter(HYBAS_ID %in% df_wis$H12) |> sf::st_geometry(), add = TRUE, col = "#222222")
plot(h6 |> sf::st_geometry() |> sf::st_crop(sf::st_bbox(leb)), add = TRUE, col = "#77777777", lwd = 3)
plot(feow |> sf::st_geometry() |> sf::st_crop(sf::st_bbox(leb)), add = TRUE, lwd = 6)




d %>%
    group_by(group) %>%
    dplyr::mutate(
        first = dplyr::first(value),
        last = dplyr::last(value)
    )