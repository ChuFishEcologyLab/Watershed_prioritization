require(readxl)
require(tidyverse)
setwd("C:/Users/DeyC/Desktop/Cody Work/GLLFAS Round 2/Projects/KBA - Watershed prioritization/Data")

fishPA5 = read_csv("Spp_dist_HYBAS5_20230125.csv")

fishPA5[fishPA5 == 2]<-1 #change the SAR indicator of '2' to a presence indicator of '1'

Pvals = fishPA5 %>% summarize_all(.funs = "mean") 
Pvals = as_vector(Pvals)
Pvals = Pvals[-1]
Qvals = 1-Pvals
TotalQ = sum(Qvals)

#
Ivals = fishPA5
for(i in 2:ncol(Ivals)){
  Ivals[i] <- Ivals[i]*Qvals[i-1]
}

Ivals = Ivals %>% rowwise() %>% 
  mutate(QijSij = sum(c_across(Acipenser_brevirostrum:Percina_shumardi))) %>%
  mutate(richness = sum(c_across(Acipenser_brevirostrum:Percina_shumardi) > 0 ))
 
Ivals = Ivals %>% mutate(Ii = QijSij / TotalQ) %>% mutate(Qi = QijSij / richness)

towrite = Ivals %>% select(HYBAS_ID, Ii, Qi)

write_csv(towrite, "H5_importance_priority.csv")

####################Hydrobasin lvl 6
#fishPA6 = read_excel(path = "NEM_spp_x_hyB.xlsx",
 #                    sheet = "Hybas_level6")

fishPA6 = read_csv("Spp_dist_HYBAS6_20230125.csv")

fishPA6[fishPA6 == 2]<-1 #change the SAR indicator of '2' to a presence indicator of '1'


Pvals = fishPA6 %>% summarize_all(.funs = "mean") 
Pvals = as_vector(Pvals)
Pvals = Pvals[-1]
Qvals = 1-Pvals
TotalQ = sum(Qvals)

#
Ivals = fishPA6
for(i in 2:ncol(Ivals)){
  Ivals[i] <- Ivals[i]*Qvals[i-1]
}

Ivals = Ivals %>% rowwise() %>% 
  mutate(QijSij = sum(c_across(Acipenser_brevirostrum:Percina_shumardi))) %>%
  mutate(richness = sum(c_across(Acipenser_brevirostrum:Percina_shumardi) > 0 ))

Ivals = Ivals %>% mutate(Ii = QijSij / TotalQ) %>% mutate(Qi = QijSij / richness)

towrite = Ivals %>% select(HYBAS_ID, Ii, Qi)

write_csv(towrite, "H6_importance_priority.csv")
