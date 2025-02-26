# requires to run `run_pipeline()`
library(sf)
sf_use_s2(TRUE)

##
s_map6  <- st_read("output_data/map6.gpkg")  |>
    st_simplify(dTolerance = 1000)  |>
    st_transform(crs = st_crs(4326)) 
    
s_map6 |>
    st_write("ShinyApp/s_map6.gpkg", append = FALSE) 


##
s_mapl <- st_read("output_data/lakeerie.gpkg") |>
    st_transform(crs =  st_crs(4326))

s_mapl |>
    st_write("ShinyApp/s_mapl.gpkg", append = FALSE)