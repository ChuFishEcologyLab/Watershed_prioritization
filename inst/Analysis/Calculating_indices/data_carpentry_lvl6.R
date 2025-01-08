require(tidyverse)
require(readxl)
require(viridis)
require(ggmap)

###bring in WSI and CCI
data6 = read_csv("HyC_6_df.csv")
data6 = data6 %>% select(HYBAS_ID, 
                         MEAN_INT_1,  #WSI measures 
                         rcp45_55) ##Climate measure

##Bring in fish community importance and priority data
IQ6 = read_csv("H6_importance_priority.csv")
data6 = left_join(data6, IQ6, by = "HYBAS_ID")
data6 = data6 %>% select(-Ii)

##Bring in FCBI 
FBCI6 = read_csv("FBCI_level6_270123.csv")
tmp6 = FBCI6 %>% select(HYBAS_ID, Jaccard.D)
data6 = left_join(data6, tmp6, by = "HYBAS_ID")

###########
#Calculate SARI and Richness
fishPA6 = read_csv("Spp_dist_HYBAS6_20230125.csv")
fishPA6$SAR_count = NA
fishPA6$pSAR = NA
fishPA6$total_richness = NA

for(i in 1:nrow(fishPA6)){
  fishPA6$SAR_count[i] = length(which(fishPA6[i,] == 2))
  fishPA6$total_richness[i] = length(which(fishPA6[i,] > 0))
  fishPA6$pSAR[i] =  length(which(fishPA6[i,] == 2)) / length(which(fishPA6[i,] %in% c(1,2)))                              
}


###
tmp = fishPA6 %>% select(HYBAS_ID, SAR_count, total_richness)
data6 = left_join(data6, tmp, by="HYBAS_ID")

colnames(data6)<- c("HYBAS_ID", 
                    "WSI",  #is just HStress
                    "CCI",  #is rcp45_55
                    "Fish_priority", 
                    "FBCI",
                    "SARI",  #is count of SAR
                    "Fish_richness") #Is total richness

#Standardize
data6 = data6 %>%
  dplyr::mutate(FBCI_n = (100* (FBCI-min(FBCI)) / ( max(FBCI)-min(FBCI) )  ) ) %>%
  dplyr::mutate(WSI_n = (100* (WSI-min(WSI)) / ( max(WSI)-min(WSI) )  ) ) %>%
  dplyr::mutate(CCI_n = (100* (CCI-min(CCI)) / ( max(CCI)-min(CCI) )  ) ) %>%
  dplyr::mutate(Priority_n = (100* (Fish_priority-min(Fish_priority)) / ( max(Fish_priority)-min(Fish_priority) )  ) ) %>%
  dplyr::mutate(SARI_n = (100* (SARI-min(SARI)) / ( max(SARI)-min(SARI) )  ) ) %>%
  dplyr::mutate(Fish_richness_n = (100* (Fish_richness-min(Fish_richness)) / ( max(Fish_richness)-min(Fish_richness) )  ) )

#join with feow
feow6 = read_csv("hyc_6_feow_join.csv")
data6 = left_join(data6, feow6, by = "HYBAS_ID")


#drop unneeded columns
data6 = data6 %>% select(c(HYBAS_ID, FEOW_ID,
                           WSI_n,
                           FBCI_n,
                           CCI_n, 
                           SARI_n,
                           Fish_richness_n,
                           Priority_n
))


write_csv(data6, "Hybas6_data.csv")
