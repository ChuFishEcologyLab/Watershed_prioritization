require(tidyverse)
require(readr)

## load data
hyb5r = read_csv("level5ranks.csv")
hyb6r = read_csv("level6ranks.csv")

#scaling ranks level 5 hydrobasins within FEOWs
maxrank = max(hyb5r$Prot_rank_feow-1)
hyb5r = hyb5r%>% group_by(FEOW_ID)%>%
  mutate(Prot_feow_scaled=(((Prot_rank_feow-1)*maxrank/max(Prot_rank_feow-1))/maxrank)*(100 - 1) + 1)

maxrank = max(hyb5r$Rest_rank_feow-1)
hyb5r = hyb5r%>% group_by(FEOW_ID)%>%
  mutate(Rest_feow_scaled=(((Rest_rank_feow-1)*maxrank/max(Rest_rank_feow-1))/maxrank)*(100 - 1) + 1)

maxrank = max(hyb5r$SAR_rank_feow-1)
hyb5r = hyb5r%>% group_by(FEOW_ID)%>%
  mutate(SAR_feow_scaled=(((SAR_rank_feow-1)*maxrank/max(SAR_rank_feow-1))/maxrank)*(100 - 1) + 1)

maxrank = max(hyb5r$AIS_rank_feow-1)
hyb5r = hyb5r%>% group_by(FEOW_ID)%>%
  mutate(AIS_feow_scaled=(((AIS_rank_feow-1)*maxrank/max(AIS_rank_feow-1))/maxrank)*(100 - 1) + 1)

#scaling ranks level 6 hydrobasins within FEOWs
maxrank = max(hyb6r$Prot_rank_feow-1)
hyb6r = hyb6r%>% group_by(FEOW_ID)%>%
  mutate(Prot_feow_scaled=(((Prot_rank_feow-1)*maxrank/max(Prot_rank_feow-1))/maxrank)*(100 - 1) + 1)

maxrank = max(hyb6r$Rest_rank_feow-1)
hyb6r = hyb6r%>% group_by(FEOW_ID)%>%
  mutate(Rest_feow_scaled=(((Rest_rank_feow-1)*maxrank/max(Rest_rank_feow-1))/maxrank)*(100 - 1) + 1)

maxrank = max(hyb6r$SAR_rank_feow-1)
hyb6r = hyb6r%>% group_by(FEOW_ID)%>%
  mutate(SAR_feow_scaled=(((SAR_rank_feow-1)*maxrank/max(SAR_rank_feow-1))/maxrank)*(100 - 1) + 1)

maxrank = max(hyb6r$AIS_rank_feow-1)
hyb6r = hyb6r%>% group_by(FEOW_ID)%>%
  mutate(AIS_feow_scaled=(((AIS_rank_feow-1)*maxrank/max(AIS_rank_feow-1))/maxrank)*(100 - 1) + 1)

## export data
write_csv(hyb5r, file = "E:/WS_duplicate/Analysis/FEOW_ranking/hyb5feowscaled.csv")
write_csv(hyb6r, file = "E:/WS_duplicate/Analysis/FEOW_ranking/hyb6feowscaled.csv")
