best_model <- read_rds("data/created/models/best_mixed_model.rds")
best_model <- read_rds("data/created/models/rf_model.rds")

# Predict into hives
df_hives <- read.csv("data/created/tables/df_hives_with_distances_and_env.csv") |> 
  filter(Region == "Isen")
df_hives$Region <- as.factor(df_hives$Region)

df_hives$HI_nDNA <- predict(best_model, df_hives)
df_hives$HI_nDNA[df_hives$HI_nDNA < 0] <- 0
df_hives$HI_nDNA[df_hives$HI_nDNA >= 1] <- 1

hives_Isen <- vect("data/created/vectors/Isen/study_area_hive.shp")
hives_Isen$HI_nDNA <- df_hives$HI_nDNA
plot(hives_Isen, "HI_nDNA")
mapview::mapview(hives_Isen, zcol = "HI_nDNA")

df_real <- df[,c("HI_nDNA", "Dist_release_km",
                 "Grassland", "Artificial_land",
                 "Region")]
df_real$type <- "real"

df_hives <- df_hives[,c("HI_nDNA", "Dist_release_km",
                        "Grassland", "Artificial_land",
                        "Region")]
df_hives$type <- "hive"

df_both <- rbind(df_real, df_hives)

ggplot(df_both, aes(Dist_release_km, HI_nDNA, color = type)) +
  geom_point() +
  facet_wrap(~type)
