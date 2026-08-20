
library(tidyverse)
library(terra)
library(caret)
library(CAST)
library(tidytext)
library(ggh4x)
library(corrplot)

df <- read.csv("data/created/tables/landscape_genetics_data_with_env.csv")
df <- df |> distinct(Longitude, Latitude, .keep_all = T)

df$Region <- as.factor(df$Region)
predictors <- c(colnames(df)[c(-10:-9,-7:-1)])
predictors <- c(predictors, "Region")
predictors_corridors <- grep("_pop", predictors, value = TRUE, invert = TRUE)
predictors_no_sd <- grep("_sd", predictors, value = TRUE, invert = TRUE)
predictors_no_sd_no_pop <- grep("_pop", predictors_no_sd, value = TRUE, invert = TRUE)
predictors_pop <- c(predictors[c(grep("_pop", predictors))],
                    "Dist_release_km", "Time_release", "Region")

# regions <- unique(df$Region)
# 
# index <- lapply(regions, function(r) {
#   which(df$Region != r)
# })
# 
# indexOut <- lapply(regions, function(r) {
#   which(df$Region == r)
# })
# 
# names(index) <- regions
# names(indexOut) <- regions

coords_pop <- vect(df,
                   geom = c("Longitude", "Latitude"),
                   crs = "EPSG:4326")
dist_matrix <- distance(coords_pop)
dist_matrix_r <- as.matrix(dist_matrix)
hc <- hclust(as.dist(dist_matrix_r), method = "single")
groups <- cutree(hc, h = 1000) # 1 km
df$cluster <- groups
coords_pop <- vect(df,
                   geom = c("Longitude", "Latitude"),
                   crs = "EPSG:4326")

cluster <- unique(df$cluster)

index <- lapply(cluster, function(r) {
  which(df$cluster != r)
})

indexOut <- lapply(cluster, function(r) {
  which(df$cluster == r)
})

names(index) <- cluster
names(indexOut) <- cluster

region_weights <- 1 / table(df$Region)[df$Region]
region_weights <- as.numeric(region_weights / mean(region_weights))  # normalize

set.seed(1234)
rf_model_pop <- caret::train(df[,predictors_pop],
                         df[,"HI_nDNA"],
                         method = "ranger",
                         weights = region_weights,
                         tuneGrid = expand.grid(.mtry = 1:(length(predictors_pop)/3),
                                                .splitrule = "variance",
                                                .min.node.size = c(2:5)),
                         importance = "none",
                         always.split.variables = c("Dist_release_km"),
                         num.trees = 500,
                         preProcess = c("nzv"),
                         trControl = trainControl(method = "cv",
                                                  index = index,
                                                  indexOut = indexOut,
                                                  number = length(index),
                                                  savePredictions = TRUE,
                                                  preProcOptions = list(uniqueCut = 0.10)))

global_validation(rf_model_pop)

set.seed(1234)
rf_model_corridor <- caret::train(df[,predictors_corridors],
                             df[,"HI_nDNA"],
                             method = "ranger",
                             weights = region_weights,
                             tuneGrid = expand.grid(.mtry = 1:(length(predictors_corridors)/3),
                                                    .splitrule = "variance",
                                                    .min.node.size = c(2:5)),
                             importance = "none",
                             always.split.variables = c("Dist_release_km"),
                             num.trees = 500,
                             preProcess = c("nzv"),
                             trControl = trainControl(method = "cv",
                                                      index = index,
                                                      indexOut = indexOut,
                                                      number = length(index),
                                                      savePredictions = TRUE,
                                                      preProcOptions = list(uniqueCut = 0.10)))
global_validation(rf_model_corridor)

set.seed(1234)
rf_model_both <- caret::train(df[,predictors],
                                  df[,"HI_nDNA"],
                                  method = "ranger",
                                  weights = region_weights,
                                  tuneGrid = expand.grid(.mtry = 1:(length(predictors)/3),
                                                         .splitrule = "variance",
                                                         .min.node.size = c(2:5)),
                                  importance = "none",
                                  always.split.variables = c("Dist_release_km"),
                                  num.trees = 500,
                                  preProcess = c("nzv"),
                                  trControl = trainControl(method = "cv",
                                                           index = index,
                                                           indexOut = indexOut,
                                                           number = length(index),
                                                           savePredictions = TRUE,
                                                           preProcOptions = list(uniqueCut = 0.10)))

rf_model_predictors_no_sd_no_pop <- caret::train(df[,predictors_no_sd_no_pop],
                                                 df[,"HI_nDNA"],
                                                 method = "ranger",
                                                 weights = region_weights,
                                                 tuneGrid = expand.grid(.mtry = 1:(length(predictors_no_sd_no_pop)/3),
                                                                        .splitrule = "variance",
                                                                        .min.node.size = c(2:5)),
                                                 importance = "none",
                                                 always.split.variables = c("Dist_release_km"),
                                                 num.trees = 500,
                                                 preProcess = c("nzv"),
                                                 trControl = trainControl(method = "cv",
                                                                          index = index,
                                                                          indexOut = indexOut,
                                                                          number = length(index),
                                                                          savePredictions = TRUE,
                                                                          preProcOptions = list(uniqueCut = 0.10)))

global_validation(rf_model_pop)
global_validation(rf_model_corridor) # The
global_validation(rf_model_both)
global_validation(rf_model_predictors_no_sd_no_pop)

rf_model <- caret::train(df[,predictors_no_sd_no_pop],
                         df[,"HI_nDNA"],
                         method = "ranger",
                         weights = region_weights,
                         tuneGrid = expand.grid(.mtry = 1:(length(predictors_no_sd_no_pop)/3),
                                                .splitrule = "variance",
                                                .min.node.size = c(2,5,10,20)),
                         importance = "none",
                         always.split.variables = c("Dist_release_km"),
                         num.trees = 1000,
                         preProcess = c("nzv"),
                         trControl = trainControl(method = "cv",
                                                  index = index,
                                                  indexOut = indexOut,
                                                  number = length(index),
                                                  savePredictions = TRUE,
                                                  preProcOptions = list(uniqueCut = 0.10)))
global_validation(rf_model)

saveRDS(rf_model, "data/created/models/rf_model.rds")

rf_model
df_pred <- rf_model$pred |> 
  filter(mtry == 3 & min.node.size == 2) |>
  left_join(df |> dplyr::mutate(rowIndex = row_number()), by = "rowIndex")

df_pred |> 
  ggplot(aes(pred, obs, colour = Region)) +
  geom_point() +
  theme_minimal() +
  theme(aspect.ratio = 1) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(x = "Hybrid index (predicted)",
       y = "Hybrid index (observed)")
ggsave(paste0("output/figures/predictions_CV_RF_model.pdf"),
       width = 5, height = 5)

df_models_result <- NULL

removed_vars <- rf_model$preProcess$method$remove
all_vars <- colnames(rf_model$trainingData %>% dplyr::select(-matches(c(".outcome"))))
kept_vars <- all_vars[!(all_vars %in% removed_vars)]
pred_model <- iml::Predictor$new(rf_model,
                                 rf_model$trainingData %>%
                                   dplyr::select(matches(kept_vars)),
                                   y = rf_model$trainingData$.outcome)
  
eff_model <- iml::FeatureEffects$new(pred_model,
                                     grid.size = 25,
                                     features = kept_vars)
  
df_models <- eff_model$results %>%
  purrr::map(~ {
    .x$.borders <- as.character(.x$.borders)
    .x
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(
      x = .borders,
      y = .value,
      covariate = .feature
    ) %>%
    dplyr::as_tibble()
  
model_imp <- iml::FeatureImp$new(pred_model, loss = "rmse",
                                 compare = "ratio",
                                 n.repetitions = 100,
                                 features = kept_vars)
  
model_imp <- data.frame(model_imp$results)
colnames(model_imp)[1] <- "covariate"
  
df_models <- df_models %>%
    left_join(model_imp, by = "covariate")

# Plot variable importance
df_models %>%
  na.omit() %>%
  dplyr::group_by(covariate) %>% 
  distinct(covariate, .keep_all = TRUE) %>%
  dplyr::arrange(importance) %>%
  ggplot(aes(x = importance, y = reorder(covariate, importance))) +
  geom_point() +
  geom_errorbar(aes(xmin = importance.05, xmax = importance.95), width = 0.2) +
  theme_minimal() +
  scale_y_reordered() +
  labs(y = "Predictor variables",
       x = "Variable importance (loss: RMSE)",
       title = "Variable importance") +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches"))
ggsave(paste0("output/figures/variable_importance_HI_nDNA_RF_model.pdf"),
       width = 5, height = 5)

# Plot response curves
min_imp <- df_models[df_models$covariate == "Region", "importance"][1,1]

df_models %>%
  mutate(x = as.numeric(x),
         y = as.numeric(y)) %>% 
  filter(importance >= min_imp$importance) |> 
  mutate_if(is.character, as.factor) %>%
  filter(covariate != "Region") |> 
  ggplot(aes(x = x, y = y)) +
  facet_wrap2(~reorder(covariate, -importance),
              scales = "free_x",
              ncol = 4,
              strip = strip_nested()) +
  geom_line(alpha = 1, linewidth = 1) +
  geom_rug(data = df_models %>%
             mutate(x = as.numeric(x),
                    y = as.numeric(y)) %>%
             filter(covariate != "Region") |>
             filter(importance >= min_imp$importance) |> 
             mutate_if(is.character, as.factor),
           sides = "b", alpha = 0.75, linewidth = 0.5, outside = T) +
  coord_cartesian(clip = "off") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -2),
        panel.spacing = unit(1, "lines")) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.001)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches"),
        aspect.ratio = 1) +
  labs(x = "Variable mean extracted between pairs of populations",
       y = "Predicted change from mean hybrid index \n(higher values represent T. carnifex")
ggsave(paste0("output/figures/response_curves_HI_nDNA_RF_model.pdf"),
       width = 6, height = 5)

df_models %>%
  filter(importance >= min_imp$importance) |> 
  mutate_if(is.character, as.factor) %>%
  filter(covariate == "Region") |> 
  ggplot(aes(x = x, y = y)) +
  facet_wrap2(~reorder(covariate, -importance),
              scales = "free_x",
              ncol = 4,
              strip = strip_nested()) +
  geom_point(alpha = 1, linewidth = 1) +
  geom_rug(data = df_models %>%
             filter(covariate == "Region") |>
             filter(importance >= min_imp$importance) |> 
             mutate_if(is.character, as.factor),
           sides = "b", alpha = 0.75, linewidth = 0.5, outside = T) +
  coord_cartesian(clip = "off") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -2),
        panel.spacing = unit(1, "lines")) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.001)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "inches"),
        aspect.ratio = 1) +
  labs(x = "",
       y = "Predicted change from mean hybrid index \n(higher values represent T. carnifex")

ggsave(paste0("output/figures/response_curves_HI_nDNA_RF_model_cat.pdf"),
       width = 4, height = 4)

ggplot(df, aes(Dist_release_km, HI_nDNA)) +
  geom_point() +
  facet_wrap(~Region)

df$HI_nDNA_RF_model_prediction <- predict(rf_model, df)

df |> 
  ggplot(aes(HI_nDNA_RF_model_prediction, HI_nDNA, colour = Region)) +
  geom_point() +
  theme_minimal() +
  theme(aspect.ratio = 1) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(x = "Hybrid index (predicted)",
       y = "Hybrid index (observed)")
# ggsave(paste0("output/figures/predictions_RF_model.pdf"),
#        width = 5, height = 5)

# Save
write.table(df,
            "data/created/tables/landscape_genetics_data_with_env_RF_pred.csv",
            sep = ",",
            row.names = F)

# Resistance maps
for(i in unique(df$Region)){
  
  rst <- rast(paste0("data/created/rasters/",i,"/All_environmental_variables.tif"))
  study_area <- vect((paste0("data/created/vectors/",i,"/study_area.shp")))
  rst_pop <- rst
  names(rst_pop) <- paste0(names(rst_pop),"_pop")
  rst <- c(rst, rst_pop)
  rst_dist <- rst[[1]]
  rst_dist[] <- mean(df$Dist_release_km)
  names(rst_dist) <- "Dist_release_km"
  rst_region <- rst[[1]]
  rst_region[] <- i
  names(rst_region) <- "Region"
  rst_time <- rst[[1]]
  rst_time[] <- mean(df$Time_release)
  names(rst_time) <- "Time_release"
  rst <- c(rst_dist, rst_region, rst_time, rst)
  rst <- rst[[predictors]]
  
  rst_pred <- predict(rst, rf_model)
  rst_pred <- mask(rst_pred, rst[[1]])
  resistance <- 1 - rst_pred
  names(resistance) <- "RF_model_resistance"
  resistance <- mask(resistance, study_area)
  writeRaster(resistance, paste0("data/created/rasters/",i,"/RF_model_resistance_",i,".tif"), overwrite = T)
  plot(resistance, main = i)
  
}

# cor_matrix <- cor(df[, predictors[-22]], use = "pairwise.complete.obs")
# 
# corrplot(cor_matrix, method = "color", type = "upper", 
#          tl.cex = 0.7, tl.col = "black", 
#          addCoef.col = "black", number.cex = 0.5,
#          diag = FALSE)
