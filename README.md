# Watershed_prioritization

This folder includes the data, analyses, and Shiny app built to visualize the results of efforts to prioritize watersheds within Canada for four freshwater fish conservation objectives including: 
- i) area-based protection; 
- ii) habitat restoration; 
- iii) species at risk management; or 
- iv) invasive species management. 

The prioritizations were based on national spatial data of the richness, rarity, and at-risk status of fishes, the amount of climate change and other watershed stressors, and the degree of fish community change (native vs. current species present within watershed). 


## Structure of the repository 

### R package 



- `R/` include the source code
- `man/` cod
- `DESCRIPTION` list basic info of the package (version, dependencies, ...)
- `test` includes unit test
- `inst` includes files installed with package (see below)

### Special

- content of `inst`:
    - `inst/Analysis`: files that include the previous version of the analysis 
    - `inst/extdata`: raw data and shapefiles 
    - `inst/Manuscript`: manuscript 
- `figs` includes figures (see `v2/` for the current version of the figures)
- `ShinyApps` includes a Shiny App to explore the results


## Analysis 

To reproduce the analysis, first install the package: 

```R
install.packages("remotes")
remotes::install_github("DFOChuLab/Watershed_prioritization")
```

Once installed run: 

```R
library(watershedPrioritization)
run_pipeline()
```

See `R/pipeline.R` and the documentation of the package for more details.


## Shiny App 

To run the Shiny App, first download this repository, then set your working to 
`ShinyApp`, then run:

```r
shiny::runApp()
```

Note that the following package are required:

- bslib (>= 0.9.0),
- leaflet (>= 2.2.2),
- leafem (>= 0.2.3),
- shiny (>= 1.9.1),
- shinybusy (>= 0.3.3),
- shinyjs (>= 2.1.0),
- sf (>= 1.0-18)


## Basic session Info

Code (package and Shiny App) was tested under: 

```r
R version 4.4.2 (2024-10-31)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 24.04.2 LTS
```