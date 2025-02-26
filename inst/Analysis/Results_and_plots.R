require(tidyverse)
require(viridis)
require(ggmap)
require(ggspatial)
require(sf)
require(GGally)
require(ggbeeswarm)
require(ggpubr)

# load data
######################

weights <- read_csv("Co_author_weightings.csv")
#
data5 <- read_csv("watershed_prioritization_level5.csv")
# map5 = read_sf("Hybas5_map/hyC_5_join_shp_Lamb_up.shp")
# map5 = left_join(map5, data5, by="HYBAS_ID")

data6 <- read_csv("watershed_prioritization_level6.csv")
# map6 = read_sf("Hydrobasin Level 6/hyC_6_join_shp_Lamb_up.shp")
# map6 = left_join(map6, data6, by="HYBAS_ID")
# map6 = map6 %>% filter(HYBAS_ID %in% data6$HYBAS_ID)

feow <- read_sf("FEOW_CAN_Extent/FEOW__CAN_Extent.shp")
###########

# Identify which hy5 each hy6 belongs to
##############
# max_overlap = c()
# for(i in 1:nrow(map6)){
#   sub6 = map6[i,]
#   intersects <- which(st_intersects(sub6, map5, sparse = FALSE))
#   overlaps <- st_intersection(sub6, map5[intersects,])
#   area = st_area(overlaps)
#   max_overlap[i]<-map5$HYBAS_ID[intersects[which.max(area)]]
#   print(i)
# }
#
# map6$corresponding.HYBAS5<-max_overlap

# st_write(map6, "map6.gpkg")
map6 <- read_sf("map6.gpkg")

# st_write(map5, "map5.gpkg")
map5 <- read_sf("map5.gpkg")
############

# Scale ranks within FEOW to number of FEOW
###########

#
maxrank <- max(map6$Prot_rank_feow - 1)
map6 <- map6 %>%
  group_by(FEOW_ID) %>%
  dplyr::mutate(Prot_rank_feow_scaled = ((Prot_rank_feow - 1) * maxrank / max(Prot_rank_feow - 1)) + 1)

maxrank <- max(map6$Rest_rank_feow - 1)
map6 <- map6 %>%
  group_by(FEOW_ID) %>%
  dplyr::mutate(Rest_rank_feow_scaled = ((Rest_rank_feow - 1) * maxrank / max(Rest_rank_feow - 1)) + 1)

maxrank <- max(map6$SAR_rank_feow - 1)
map6 <- map6 %>%
  group_by(FEOW_ID) %>%
  dplyr::mutate(SAR_rank_feow_scaled = ((SAR_rank_feow - 1) * maxrank / max(SAR_rank_feow - 1)) + 1)

maxrank <- max(map6$AIS_rank_feow - 1)
map6 <- map6 %>%
  group_by(FEOW_ID) %>%
  dplyr::mutate(AIS_rank_feow_scaled = ((AIS_rank_feow - 1) * maxrank / max(AIS_rank_feow - 1)) + 1)

############

# Coauthor weightings plot
##########

pdata1 <- weights[, c(1:6)]
pdata1 <- pivot_longer(pdata1, cols = starts_with("Weight for"))
pdata1$name <- rep(c(
  "Watershed stress", "Community change", "Climate change",
  "Species at risk", "Species richness", "Species rarity"
), 8)
sumpdata1 <- pdata1 %>%
  group_by(name) %>%
  summarize(med = stats::median(value))

p1 <- ggplot() +
  geom_hline(aes(yintercept = 0), color = "black", linetype = 2) +
  geom_beeswarm(data = pdata1, aes(x = name, y = value), size = 2) +
  # geom_beeswarm(data=pdata1, aes(x=reorder(name, value, median),  y = value), size=2)+
  geom_point(data = sumpdata1, aes(x = name, y = med), color = "red", alpha = 0.7, size = 6) +
  scale_y_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-5.5, 5.5)) +
  ylab("Weight") +
  xlab("") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Area-based protection") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


pdata2 <- weights[, c(7:12)]
pdata2 <- pivot_longer(pdata2, cols = starts_with("Weight for"))
pdata2$name <- rep(c(
  "Watershed stress", "Community change", "Climate change",
  "Species at risk", "Species richness", "Species rarity"
), 8)
sumpdata2 <- pdata2 %>%
  group_by(name) %>%
  summarize(med = stats::median(value))

p2 <- ggplot() +
  geom_hline(aes(yintercept = 0), color = "black", linetype = 2) +
  geom_beeswarm(data = pdata2, aes(x = name, y = value), size = 2) +
  # geom_beeswarm(data=pdata2, aes(x=reorder(name, value, median),  y = value), size=2)+
  geom_point(data = sumpdata2, aes(x = name, y = med), color = "red", alpha = 0.7, size = 6) +
  scale_y_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-5.5, 5.5)) +
  ylab("Weight") +
  xlab("") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Habitat restoration") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

pdata3 <- weights[, c(13:18)]
pdata3 <- pivot_longer(pdata3, cols = starts_with("Weight for"))
pdata3$name <- rep(c(
  "Watershed stress", "Community change", "Climate change",
  "Species at risk", "Species richness", "Species rarity"
), 8)
sumpdata3 <- pdata3 %>%
  group_by(name) %>%
  summarize(med = stats::median(value))

p3 <- ggplot() +
  geom_hline(aes(yintercept = 0), color = "black", linetype = 2) +
  geom_beeswarm(data = pdata3, aes(x = name, y = value), size = 2) +
  # geom_beeswarm(data=pdata3, aes(x=reorder(name, value, median),  y = value), size=2)+
  geom_point(data = sumpdata3, aes(x = name, y = med), color = "red", alpha = 0.7, size = 6) +
  scale_y_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-5.5, 5.5)) +
  ylab("Weight") +
  xlab("") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Species at risk management") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


pdata4 <- weights[, c(19:24)]
pdata4 <- pivot_longer(pdata4, cols = starts_with("Weight for"))
pdata4$name <- rep(c(
  "Watershed stress", "Community change", "Climate change",
  "Species at risk", "Species richness", "Species rarity"
), 8)
sumpdata4 <- pdata4 %>%
  group_by(name) %>%
  summarize(med = stats::median(value))

p4 <- ggplot() +
  geom_hline(aes(yintercept = 0), color = "black", linetype = 2) +
  geom_beeswarm(data = pdata4, aes(x = name, y = value), size = 2) +
  # geom_beeswarm(data=pdata4, aes(x=reorder(name, value, median),  y = value), size=2)+
  geom_point(data = sumpdata4, aes(x = name, y = med), color = "red", alpha = 0.7, size = 6) +
  scale_y_continuous(breaks = c(-4, -2, 0, 2, 4), limits = c(-5.5, 5.5)) +
  ylab("Weight") +
  xlab("") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Invasive species management") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

png("Figures/coauthor_weightings.png", width = 12, height = 8.5, units = "in", res = 100) # Open a new png file
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2) # Write the grid.arrange in the file
dev.off() # Close the file

###########

# Hydrobasin 6 index values
############

S2_A <- ggplot() +
  geom_sf(data = map6, aes(fill = WSI_n, col = WSI_n), alpha = 1) +
  scale_color_viridis(option = "viridis") +
  scale_fill_viridis(
    option = "viridis",
    breaks = c(0, 25, 50, 75, 100)
  ) +
  theme_minimal() +
  # ggtitle("Watershed stress")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )

S2_B <- ggplot() +
  geom_sf(data = map6, aes(fill = SARI_n, col = SARI_n), alpha = 1) +
  scale_color_viridis(option = "viridis") +
  scale_fill_viridis(
    option = "viridis",
    breaks = c(0, 25, 50, 75, 100)
  ) +
  theme_minimal() +
  # ggtitle("Fish species at risk richness")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )

S2_C <- ggplot() +
  geom_sf(data = map6, aes(fill = Fish_richness_n, col = Fish_richness_n), alpha = 1) +
  scale_color_viridis(option = "viridis") +
  scale_fill_viridis(
    option = "viridis",
    breaks = c(0, 25, 50, 75, 100)
  ) +
  theme_minimal() +
  # ggtitle("Fish species richness")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )

S2_D <- ggplot() +
  geom_sf(data = map6, aes(fill = Priority_n, col = Priority_n), alpha = 1) +
  scale_color_viridis(option = "viridis") +
  scale_fill_viridis(
    option = "viridis",
    breaks = c(0, 25, 50, 75, 100)
  ) +
  theme_minimal() +
  # ggtitle("Fish rarity")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )

S2_E <- ggplot() +
  geom_sf(data = map6, aes(fill = FBCI_n, col = FBCI_n), alpha = 1) +
  scale_color_viridis(option = "viridis") +
  scale_fill_viridis(
    option = "viridis",
    breaks = c(0, 25, 50, 75, 100)
  ) +
  theme_minimal() +
  # ggtitle("Fish biodiversity change")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )

S2_F <- ggplot() +
  geom_sf(data = map6, aes(fill = CCI_n, col = CCI_n), alpha = 1) +
  scale_color_viridis(option = "viridis") +
  scale_fill_viridis(
    option = "viridis",
    breaks = c(0, 25, 50, 75, 100)
  ) +
  theme_minimal() +
  # ggtitle("Climate change")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )


png("Figures/normalized_index_values.png", width = 7.5, height = 9, units = "in", res = 100) # Open a new png file
ggarrange(S2_A, S2_B,
  S2_C, S2_D,
  S2_E, S2_F,
  labels = c("Watershed stress", "Species at risk", "Species richness", "Species rarity", "Community change", "Climate change"),
  font.label = list(size = 10, face = "plain", color = "black"), hjust = -0.15, vjust = 1.8,
  ncol = 2, nrow = 3, common.legend = TRUE,
  legend = "bottom"
) # Write the grid.arrange in the file
dev.off() # Close the file
############

## Priorities within ecoregions
###############
data <- map6

#
A1 <- ggplot() +
  geom_sf(data = data, aes(fill = Prot_rank_feow_scaled, col = Prot_rank_feow_scaled), alpha = 1) +
  scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
  scale_fill_viridis(
    alpha = 1, end = 0.9, direction = -1, option = "magma",
    breaks = c(1, stats::median(data$Prot_rank_feow_scaled), max(data$Prot_rank_feow_scaled)),
    labels = c("High", "Medium", "Low")
  ) +
  # geom_sf(data=feow, fill = "transparent", color ="black", linewidth =0.1)+
  theme_minimal() +
  # ggtitle("Area-based protection")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )

#
B1 <- ggplot() +
  geom_sf(data = data, aes(fill = Rest_rank_feow_scaled, col = Rest_rank_feow_scaled), alpha = 1) +
  scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
  scale_fill_viridis(
    alpha = 1, end = 0.9, direction = -1, option = "magma",
    breaks = c(1, stats::median(data$Rest_rank_feow_scaled), max(data$Rest_rank_feow_scaled)),
    labels = c("High", "Medium", "Low")
  ) +
  # geom_sf(data=feow, fill = "transparent", color ="black", linewidth =0.1)+
  theme_minimal() +
  # ggtitle("Area-based protection")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )

#
C1 <- ggplot() +
  geom_sf(data = data, aes(fill = SAR_rank_feow_scaled, col = SAR_rank_feow_scaled), alpha = 1) +
  scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
  scale_fill_viridis(
    alpha = 1, end = 0.9, direction = -1, option = "magma",
    breaks = c(1, stats::median(data$SAR_rank_feow_scaled), max(data$SAR_rank_feow_scaled)),
    labels = c("High", "Medium", "Low")
  ) +
  # geom_sf(data=feow, fill = "transparent", color ="black", linewidth =0.1)+
  theme_minimal() +
  # ggtitle("Area-based protection")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )

#
D1 <- ggplot() +
  geom_sf(data = data, aes(fill = AIS_rank_feow_scaled, col = AIS_rank_feow_scaled), alpha = 1) +
  scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
  scale_fill_viridis(
    alpha = 1, end = 0.9, direction = -1, option = "magma",
    breaks = c(1, stats::median(data$AIS_rank_feow_scaled), max(data$AIS_rank_feow_scaled)),
    labels = c("High", "Medium", "Low")
  ) +
  # geom_sf(data=feow, fill = "transparent", color ="black", linewidth =0.1)+
  theme_minimal() +
  # ggtitle("Area-based protection")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )

png("Figures/priorities_by_feow.png", width = 8.5, height = 6, units = "in", res = 300) # Open a new png file
ggarrange(A1, B1, C1, D1,
  ncol = 2, nrow = 2, common.legend = TRUE,
  labels = c("Area-based protection", "Habitat restoration", "Species at risk\n management", "Invasive species\n management"),
  font.label = list(size = 10, face = "plain", color = "black"),
  hjust = -0.25, vjust = 1.8,
  legend = "bottom"
) # Write the grid.arrange in the file
dev.off() # Close the file


# Analysis of priorities within FEOW
##################

map6 %>%
  st_drop_geometry() %>%
  ungroup() %>%
  group_by(FEOW_ID) %>%
  summarise(SAR_p = max(SARI_n)) %>%
  print(n = Inf)
# two ecoregions with no SAR (111, 112)

map6 %>%
  st_drop_geometry() %>%
  ungroup() %>%
  group_by(FEOW_ID) %>%
  summarise(SAR_p = max(FBCI_n)) %>%
  print(n = Inf)
# five ecoregions with no FBCI (101, 102, 112, 113, 114)

# Relationship with size? coastal? latitude?

# ggplot(data)+
#   geom_boxplot(aes(x=COAST, group=COAST, y=Prot_rank_feow_scaled))
# lm(Prot_rank_feow_scaled~SUB_AREA, data = data)
# lm(Rest_rank_feow_scaled~SUB_AREA, data = data)
# summary(lm(SAR_rank_feow_scaled~SUB_AREA, data = data))

###################

# Hydrobasin 6 national priorities
#################

data <- map6
#
data$rank <- rank(-data$protection_score)
A <- ggplot() +
  geom_sf(data = data, aes(fill = rank, col = rank), alpha = 1) +
  scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
  scale_fill_viridis(
    alpha = 1, end = 0.9, direction = -1, option = "magma",
    breaks = c(1, stats::median(data$rank), max(data$rank)),
    labels = c("High", "Medium", "Low")
  ) +
  theme_minimal() +
  # ggtitle("Area-based protection")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )
#
data$rank <- rank(-data$restoration_score)
B <- ggplot() +
  geom_sf(data = data, aes(fill = rank, col = rank), alpha = 1) +
  scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
  scale_fill_viridis(
    alpha = 1, end = 0.9, direction = -1, option = "magma",
    breaks = c(1, stats::median(data$rank), max(data$rank)),
    labels = c("High", "Medium", "Low")
  ) +
  theme_minimal() +
  # ggtitle("Restoration")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )
#
data$rank <- rank(-data$SAR_score)
C <- ggplot() +
  geom_sf(data = data, aes(fill = rank, col = rank), alpha = 1) +
  scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
  scale_fill_viridis(
    alpha = 1, end = 0.9, direction = -1, option = "magma",
    breaks = c(1, stats::median(data$rank), max(data$rank)),
    labels = c("High", "Medium", "Low")
  ) +
  theme_minimal() +
  #  ggtitle("Species at risk management")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )
#
data$rank <- rank(-data$AIS_score)
D <- ggplot() +
  geom_sf(data = data, aes(fill = rank, col = rank), alpha = 1) +
  scale_color_viridis(guide = "none", end = 0.9, direction = -1, option = "magma") +
  scale_fill_viridis(
    alpha = 1, end = 0.9, direction = -1, option = "magma",
    breaks = c(1, stats::median(data$rank), max(data$rank)),
    labels = c("High", "Medium", "Low")
  ) +
  theme_minimal() +
  # ggtitle("Invasive species management")+
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )

png("Figures/national_priorities.png", width = 8.5, height = 6, units = "in", res = 100) # Open a new png file
ggarrange(A, B, C, D,
  ncol = 2, nrow = 2,
  common.legend = TRUE,
  labels = c("Area-based protection", "Habitat restoration", "Species at risk\n management", "Invasive species\n management"),
  font.label = list(size = 10, face = "plain", color = "black"),
  hjust = -0.25, vjust = 1.8,
  legend = "bottom"
) # Write the grid.arrange in the file

dev.off() # Close the file
##############

### Comparison plots
##############
pal <- c(
  viridis_pal(option = "magma")(20)[20],
  viridis_pal(option = "viridis")(20)[17],
  viridis_pal(option = "magma")(20)[17],
  "grey90"
)

#
data <- map6

perc1 <- quantile(data$Prot_rank_feow_scaled)
perc2 <- quantile(data$Rest_rank_feow_scaled)

rects <- tibble(
  xmins = c(1, 1, perc1[2], perc1[2]),
  xmaxs = c(perc1[2], perc1[2], perc1[5], perc1[5]),
  ymins = c(1, perc2[2], 1, perc2[2]),
  ymaxs = c(perc2[2], perc2[5], perc2[2], perc2[5]),
  fills = c("a", "b", "c", "d")
)

p1 <- ggplot() +
  geom_rect(data = rects, aes(xmin = xmins, ymin = ymins, xmax = xmaxs, ymax = ymaxs, fill = fills)) +
  geom_point(data = data, aes(
    x = Prot_rank_feow_scaled,
    y = Rest_rank_feow_scaled
  )) +
  scale_fill_manual(values = pal) +
  xlab("Priority for area-based protection") +
  ylab("Priority for habitat restoration") +
  scale_y_reverse(
    breaks = c(11, perc2[5] - 10), labels = c("high", "low")
  ) +
  scale_x_reverse(
    breaks = c(11, perc1[5] - 10), labels = c("high", "low")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.ticks = element_blank()
  )
p1

cor(data$Prot_rank_feow_scaled, data$Rest_rank_feow_scaled, method = "spearman")

data <- data %>%
  dplyr::mutate(wsh_fill = ifelse(
    Prot_rank_feow_scaled < perc1[2] & Rest_rank_feow_scaled < perc2[2], "a",
    ifelse(Prot_rank_feow_scaled < perc1[2], "b",
      ifelse(Rest_rank_feow_scaled < perc2[2], "c", "d")
    )
  ))

length(which(data$wsh_fill == "a")) / length(which(data$wsh_fill %in% c("a", "b", "c")))

m1 <- ggplot() +
  geom_sf(data = data, aes(fill = wsh_fill, col = wsh_fill), alpha = 1) +
  scale_color_manual(
    guide = "none",
    values = pal
  ) +
  scale_fill_manual(
    values = pal,
    guide = "none"
  ) +
  geom_sf(data = feow, fill = "transparent", color = "black", linewidth = 0.1) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )

png("Figures/protection_vs_restoration.png", width = 8.5, height = 4, units = "in", res = 100) # Open a new png file
ggarrange(p1, m1, ncol = 2, nrow = 1) # Write the grid.arrange in the file
dev.off() # Close the file

##
perc1 <- quantile(data$SAR_rank_feow_scaled)
perc2 <- quantile(data$AIS_rank_feow_scaled)

rects <- tibble(
  xmins = c(1, 1, perc1[2], perc1[2]),
  xmaxs = c(perc1[2], perc1[2], perc1[5], perc1[5]),
  ymins = c(1, perc2[2], 1, perc2[2]),
  ymaxs = c(perc2[2], perc2[5], perc2[2], perc2[5]),
  fills = c("a", "b", "c", "d")
)

cor(data$SAR_rank_feow_scaled, data$AIS_rank_feow_scaled, method = "spearman")

p2 <- ggplot() +
  geom_rect(data = rects, aes(xmin = xmins, ymin = ymins, xmax = xmaxs, ymax = ymaxs, fill = fills)) +
  geom_point(data = data, aes(
    x = SAR_rank_feow_scaled,
    y = AIS_rank_feow_scaled
  )) +
  scale_fill_manual(values = pal) +
  xlab("Priority for species at risk management") +
  ylab("Priority for invasive species management") +
  scale_y_reverse(
    breaks = c(11, perc2[5] - 10), labels = c("high", "low")
  ) +
  scale_x_reverse(
    breaks = c(11, perc1[5] - 10), labels = c("high", "low")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 90),
    axis.ticks = element_blank()
  )
p2

data <- data %>%
  dplyr::mutate(wsh_fill = ifelse(
    SAR_rank_feow_scaled < perc1[2] & AIS_rank_feow_scaled < perc2[2], "a",
    ifelse(SAR_rank_feow_scaled < perc1[2], "b",
      ifelse(AIS_rank_feow_scaled < perc2[2], "c", "d")
    )
  ))

length(which(data$wsh_fill == "a")) / length(which(data$wsh_fill %in% c("a", "b", "c")))


m2 <- ggplot() +
  geom_sf(data = data, aes(fill = wsh_fill, col = wsh_fill), alpha = 1) +
  scale_color_manual(
    guide = "none",
    values = pal
  ) +
  scale_fill_manual(
    values = pal,
    guide = "none"
  ) +
  geom_sf(data = feow, fill = "transparent", color = "black", linewidth = 0.1) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, -1, -0.2, -1), "lines")
  )

png("Figures/SAR_vs_AIS.png", width = 8.5, height = 4, units = "in", res = 100) # Open a new png file
ggarrange(p2, m2, ncol = 2, nrow = 1) # Write the grid.arrange in the file
dev.off() # Close the file
##############

## Comparison analysis
#######

newdata <- data %>%
  select(dplyr::ends_with("scaled")) %>%
  st_drop_geometry()
pca1 <- stats::prcomp(newdata, scale. = FALSE)
utils::head(pca1$rotation)
scores <- as_tibble(pca1$x)

ggplot() +
  geom_point(data = scores, aes(x = PC1, y = PC2))

###############


# Investigating scale
#######
d5 <- map5 %>%
  st_drop_geometry() %>%
  ungroup()

map6 <- map6 %>%
  ungroup() %>%
  dplyr::mutate(
    protection_score5 = d5$protection_score[match(map6$corresponding.HYBAS5, d5$HYBAS_ID)],
    restoration_score5 = d5$restoration_score[match(map6$corresponding.HYBAS5, d5$HYBAS_ID)],
    SAR_score5 = d5$SAR_score[match(map6$corresponding.HYBAS5, d5$HYBAS_ID)],
    AIS_score5 = d5$AIS_score[match(map6$corresponding.HYBAS5, d5$HYBAS_ID)],
  )

obj <- c("protection", "restoration", "SAR", "AIS")
threshold <- seq(0.05, 0.5, 0.01)
df <- tibble(objective = c(), threshold = c(), ratio = c())

for (i in 1:length(obj)) {
  for (j in 1:length(threshold)) {
    var <- paste0(obj[i], "_score")
    tmp <- map6 %>%
      ungroup() %>%
      group_by(FEOW_ID) %>%
      arrange(FEOW_ID, dplyr::desc(!!var)) %>%
      relocate(!!var, .after = last_col()) %>%
      top_frac(n = threshold[j])
    map6$level6 <- ifelse(map6$HYBAS_ID %in% tmp$HYBAS_ID, 1, 0)

    var2 <- paste0(obj[i], "_score5")
    tmp <- map6 %>%
      ungroup() %>%
      group_by(FEOW_ID) %>%
      arrange(FEOW_ID, dplyr::desc(!!var2)) %>%
      relocate(!!var2, .after = last_col()) %>%
      top_frac(n = threshold[j])
    map6$level5 <- ifelse(map6$HYBAS_ID %in% tmp$HYBAS_ID, 1, 0)

    ratio <- length(which(map6$level6 == 1 & map6$level5 == 0)) / length(which(map6$level6 == 1))

    to_add <- tibble(objective = obj[i], threshold = threshold[j], ratio = ratio)
    df <- bind_rows(df, to_add)
  }
}

mylabels <- df %>% filter(threshold == 0.5)
mylabels$ratio[which(mylabels$objective == "SAR")] <- 0.24 # nudge


s1 <- ggplot(data = df, aes(x = threshold * 100, y = 100 - (ratio * 100), group = objective, color = objective)) +
  geom_line(linewidth = 1.5) +
  theme_minimal() +
  ylab("Percentage of priority watersheds covered by\nprioritization at larger watershed scale") +
  scale_x_continuous(
    name = "Percentage of watersheds prioritized",
    limits = c(0, 59),
    breaks = c(10, 20, 30, 40, 50)
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  geom_text(
    data = mylabels,
    aes(
      x = 50.5,
      y = 100 - (ratio * 100),
      label = objective,
      color = objective
    ),
    hjust = 0
  ) +
  scale_color_viridis_d(option = "viridis")

png("Figures/scale_dependency.png", width = 5, height = 4, units = "in", res = 300) # Open a new png file
ggarrange(s1, ncol = 1, nrow = 1) # Write the grid.arrange in the file
dev.off() # Close the file

##
data5 <- map5
data5$watershed_rank <- data5$Rest_rank_feow
maxrank1 <- max(data5$watershed_rank - 1)
data5 <- data5 %>%
  group_by(FEOW_ID) %>%
  dplyr::mutate(watershed_rank = ((watershed_rank - 1) * maxrank1 / max(watershed_rank - 1)) + 1)
data5$watershed_rank[which(is.nan(data5$watershed_rank))] <- (maxrank1 + 1) / 2 # if there is only 1 watershed in feow

data6 <- map6
data6$watershed_rank <- data6$Rest_rank_feow
maxrank2 <- max(data6$watershed_rank - 1)
data6 <- data6 %>%
  group_by(FEOW_ID) %>%
  dplyr::mutate(watershed_rank = ((watershed_rank - 1) * maxrank2 / max(watershed_rank - 1)) + 1)
data6$watershed_rank[which(is.nan(data6$watershed_rank))] <- (maxrank2 + 1) / 2 # if there is only 1 watershed in feow

## identify top 25th percentile
data5 <- data5 %>% dplyr::mutate(priority_wsh = ifelse(watershed_rank < quantile(data5$watershed_rank)[2], "high", "low"))
data6 <- data6 %>% dplyr::mutate(priority_wsh = ifelse(watershed_rank < quantile(data6$watershed_rank)[2], "high", "low"))


###
sub_map5 <- data5 %>% filter(FEOW_ID == 103)
sub_map6 <- data6 %>% filter(corresponding.HYBAS5 %in% sub_map5$HYBAS_ID)

s_map <- ggplot() +
  geom_sf(data = sub_map6, aes(fill = priority_wsh)) +
  geom_sf(data = sub_map5, aes(color = priority_wsh), fill = "transparent", alpha = 0.5) +
  scale_color_manual(values = c("black", "white")) +
  scale_fill_manual(values = c("orange", "grey90")) +
  theme_void()
s_map
