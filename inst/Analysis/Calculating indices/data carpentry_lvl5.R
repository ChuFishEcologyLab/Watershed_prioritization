require(tidyverse)
require(readxl)
require(viridis)
require(ggmap)

###bring in WSI and CCI
data5 = read_csv("HyC_5_df.csv")
data5 = data5 %>% select(HYBAS_ID, 
                         MEAN_INT_1,  #WSI measures 
                         rcp45_55) ##Climate measure

##Bring in fish community importance and priority data
IQ5 = read_csv("H5_importance_priority.csv")
data5 = left_join(data5, IQ5, by = "HYBAS_ID")
data5 = data5 %>% select(-Ii)

##Bring in FCBI 
FBCI5 = read_csv("FBCI_level5_270123.csv")
tmp5 = FBCI5 %>% select(HYBAS_ID, Jaccard.D)
data5 = left_join(data5, tmp5, by = "HYBAS_ID")

###########
#Calculate SARI and Richness
fishPA5 = read_csv("Spp_dist_HYBAS5_20230125.csv")
fishPA5$SAR_count = NA
fishPA5$pSAR = NA
fishPA5$total_richness = NA

for(i in 1:nrow(fishPA5)){
  fishPA5$SAR_count[i] = length(which(fishPA5[i,] == 2))
  fishPA5$total_richness[i] = length(which(fishPA5[i,] > 0))
  fishPA5$pSAR[i] =  length(which(fishPA5[i,] == 2)) / length(which(fishPA5[i,] %in% c(1,2)))                              
}


###
tmp = fishPA5 %>% select(HYBAS_ID, SAR_count, total_richness)
data5 = left_join(data5, tmp, by="HYBAS_ID")

colnames(data5)<- c("HYBAS_ID", 
                   "WSI",  #is just HStress
                   "CCI",  #is rcp45_55
                   "Fish_priority", 
                   "FBCI",
                   "SARI",  #is count of SAR
                   "Fish_richness") #Is total richness

#Standardize
data5 = data5 %>%
  dplyr::mutate(FBCI_n = (100* (FBCI-min(FBCI)) / ( max(FBCI)-min(FBCI) )  ) ) %>%
  dplyr::mutate(WSI_n = (100* (WSI-min(WSI)) / ( max(WSI)-min(WSI) )  ) ) %>%
  dplyr::mutate(CCI_n = (100* (CCI-min(CCI)) / ( max(CCI)-min(CCI) )  ) ) %>%
  dplyr::mutate(Priority_n = (100* (Fish_priority-min(Fish_priority)) / ( max(Fish_priority)-min(Fish_priority) )  ) ) %>%
  dplyr::mutate(SARI_n = (100* (SARI-min(SARI)) / ( max(SARI)-min(SARI) )  ) ) %>%
  dplyr::mutate(Fish_richness_n = (100* (Fish_richness-min(Fish_richness)) / ( max(Fish_richness)-min(Fish_richness) )  ) )

#join with feow
feow5 = read_csv("hyc_5_feow_join.csv")
data5 = left_join(data5, feow5, by = "HYBAS_ID")

#drop unneeded columns
data5 = data5 %>% select(c(HYBAS_ID, FEOW_ID,
                          WSI_n,
                          FBCI_n,
                          CCI_n, 
                          SARI_n,
                          Fish_richness_n,
                          Priority_n
                          ))


write_csv(data5, "Hybas5_data.csv")


