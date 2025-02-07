# Watershed_prioritization

This folder includes the data, analyses, and Shiny app built to visualize the results of efforts to priortize watersheds within Canada for four freshwater fish conservation objectives including: 
- i) area-based protection; 
- ii) habitat restoration; 
- iii) species at risk management; or 
- iv) invasive species management. 

The prioritizations were based on national spatial data of the richness, rarity, and at-risk status of fishes, the amount of climate change and other watershed stressors, and the degree of fish community change (native vs. current species present within watershed). 


## Structure of the repository 

### R package 

- `R/` code source
- `man/` code source

### Special

- `inst/Analysis`: older version of analysis 
- `inst/extdata`: data 


## Data 

### Inputs 


### Outputs

Data are stored in `output_data/`, the folder is created if missing.


## Analysis 

Install the package 

```R
install.packages("remotes")
remotes::install_github("DFOChuLab/Watershed_prioritization")
```

Once installed run: 

```R
library(watershedPrioritization)
run_pipeline()
```