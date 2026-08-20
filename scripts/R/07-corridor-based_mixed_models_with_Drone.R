library(tidyverse)
library(lme4)
library(MuMIn)
library(ggeffects)
library(patchwork)
library(mgcv)

df_dist <- read.csv("data/created/tables/landscape_genetics_data_with_env.csv") |>
  distinct(Longitude, Latitude, .keep_all = T)
df_dist$Region <- as.factor(df_dist$Region)
df_dist <- df_dist[,c(1,2,3,4,25,26,29:44,59,60,64:77,8,79)]

df <- read.csv("data/raw/tables/DatabaseTriturusJBK_OnlyValid_foR.csv")
df <- df[,c(1,2,4,5,10,11:23,36:58)]
names(df)[1:5] <- c("Population_ID", "Region", "Latitude", "Longitude", "HI_nDNA")
df <- df |> distinct(Longitude, Latitude, .keep_all = T)
df <- df |> filter(!(is.na(HI_nDNA))) |> 
  filter(Region != "Weil am Rhein")

df <- left_join(
  df,
  df_dist %>% select(-any_of(setdiff(names(df_dist), "Population_ID")[
    setdiff(names(df_dist), "Population_ID") %in% names(df)
  ])),
  by = "Population_ID"
)

df <- df |> distinct(Longitude, Latitude, .keep_all = TRUE)

df[,c("Dist_release_km", "TER_COV_HERB", "POND_COV_WATER",
      "TER_MNDWI_MEAN")] <- scale(df[,c("Dist_release_km", "TER_COV_HERB", "POND_COV_WATER", "TER_MNDWI_MEAN")])

df$Region <- as.factor(df$Region)

car::vif(lm(HI_nDNA ~ Dist_release_km + TER_COV_HERB + POND_COV_WATER +
              TER_MNDWI_MEAN, data = df))

full_model <- lm(
  HI_nDNA ~ Dist_release_km + TER_COV_HERB + POND_COV_WATER +
    TER_MNDWI_MEAN + Region,
  data = df
)

# or GAM
full_model <- gam(
  HI_nDNA ~ s(Dist_release_km) + s(TER_COV_HERB) + s(POND_COV_WATER) +
    s(TER_MNDWI_MEAN) +
    Region,
  data = df,
  method = "ML"
)

options(na.action = "na.fail")
dredge_result <- dredge(full_model, fixed = c("s(Dist_release_km)", "Region"))
predictor_cols <- setdiff(names(dredge_result), 
                          c("(Intercept)", "s(Dist_release_km)", "Region",
                            "df", "logLik", "AICc", "delta", "weight")) # change lm and gam var name
n_predictors <- rowSums(!is.na(dredge_result[, predictor_cols]))
simplest_idx <- which.min(n_predictors)
dredge_result[simplest_idx,"delta"]
null_model <- dredge_result[simplest_idx, ]
null_model <- get.models(null_model, subset = 1)[[1]]
anova(null_model)

best_model <- get.models(dredge_result, subset = 1)[[1]]
anova(best_model)

predictors <- all.vars(formula(best_model))[-1]

plots <- lapply(predictors, function(t) plot(ggpredict(best_model, terms = t)))
wrap_plots(plots, ncol = 2)

# do cross validation for best model
for(i in 1:nrow(df)){
  
  df_train <- df[-i,]
  
  null_model <- gam(
    HI_nDNA ~ s(Dist_release_km) +
      Region,
    data = df_train,
    method = "ML"
  )
  
  best_model <- gam(
    HI_nDNA ~ s(Dist_release_km) + s(Grassland) + s(Artificial_land) +
      Region,
    data = df_train,
    method = "ML"
  )
  
  # null_model <- lm(
  #   HI_nDNA ~ Dist_release_km + Region,
  #   data = df
  # )
  # 
  # best_model <- lm(
  #   HI_nDNA ~ Dist_release_km + Grassland + Woodland_pop + Bare_land +
  #     Region,
  #   data = df
  # )
  
  
  df[i,"pred_null_cv"] <- predict(null_model, df[i,])
  df[i,"pred_best_cv"] <- predict(best_model, df[i,])
  
}

null_model <- gam(
  HI_nDNA ~ s(Dist_release_km) +
    Region,
  data = df,
  method = "ML"
)

best_model <- gam(
  HI_nDNA ~ s(Dist_release_km) + s(Grassland) + s(Artificial_land) +
    Region,
  data = df,
  method = "ML"
)

df[,"pred_null"] <- predict(null_model, df[,])
df[,"pred_best"] <- predict(best_model, df[,])

ggplot(df, aes(pred_null, pred_null_cv)) +
  geom_point() +
  facet_wrap(~Region)

ggplot(df, aes(pred_best, pred_best_cv)) +
  geom_point() +
  facet_wrap(~Region)

ggplot(df, aes(pred_null_cv, HI_nDNA)) +
  geom_point() +
  facet_wrap(~Region)

ggplot(df, aes(pred_best_cv, HI_nDNA)) +
  geom_point() +
  facet_wrap(~Region)

cor(df$HI_nDNA, df$pred_null)
cor(df$HI_nDNA, df$pred_best_cv)
