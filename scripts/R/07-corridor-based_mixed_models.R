library(tidyverse)
library(mgcv)

df <- read.csv("data/created/tables/landscape_genetics_data_with_env_RF_pred.csv")
df <- df |> distinct(Longitude, Latitude, .keep_all = TRUE)
df$Region <- as.factor(df$Region)

predictors <- c("Dist_release_km",
                "Tree",
                "Crops","Urban",
                "Grass", "Slope",
                "Elevation")

v <- usdm::vifstep(df[, predictors], th = 5)
predictors <- v@variables[!(v@variables %in% v@excluded)]
predictors <- paste0(predictors)

n <- nrow(df)
df$HI_nDNA_adj <- (df$HI_nDNA * (n - 1) + 0.5) / n

response <- "qlogis(HI_nDNA_adj)"
predictors_null <- c(predictors[1], "Region")
predictors_full <- c(predictors, "Region")

model_function <- function(response, predictors) {
  as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
}

null_model_formula <- model_function(response,
                                     predictors_null)

full_model_formula <- model_function(response,
                                     predictors_full)

full_model <- lm(full_model_formula,
                  data = df)

anova(full_model)

library(MuMIn)

options(na.action = "na.fail")
dredge_result <- dredge(full_model, fixed = c("Dist_release_km", "Region"))
predictor_cols <- setdiff(names(dredge_result), 
                          c("(Intercept)", "Dist_release_km", "Region",
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

library(ggeffects)
library(patchwork)

plots <- lapply(predictors, function(t) plot(ggpredict(best_model, terms = t)))
wrap_plots(plots, ncol = 2)

# do cross validation for best model
for(i in 1:nrow(df)){
  
  df_train <- df[-i,]
  
  null_model <- lm(qlogis(HI_nDNA_adj) ~ Dist_release_km +
                      Region,
                    data = df_train)

  best_model <- lm(qlogis(HI_nDNA_adj) ~ Dist_release_km +
                      Grass +
                      Urban +
                     Slope + 
                      Region,
                    data = df_train)
  
  df[i,"pred_null_cv"] <- predict(null_model, df[i,])
  df[i,"pred_best_cv"] <- predict(best_model, df[i,])
  
}

null_model <- lm(qlogis(HI_nDNA_adj) ~ Dist_release_km +
                   Region,
                 data = df)

best_model <- lm(qlogis(HI_nDNA_adj) ~ Dist_release_km +
                   Grass +
                   Urban +
                   Slope +
                   Region,
                 data = df)

# df$pred_null_cv[df$pred_null_cv < 0] <- 0
# df$pred_null_cv[df$pred_null_cv >= 1] <- 1
# df$pred_best_cv[df$pred_best_cv < 0] <- 0
# df$pred_best_cv[df$pred_best_cv >= 1] <- 1

df[,"pred_null"] <- predict(null_model, df[,])
df[,"pred_best"] <- plogis(predict(best_model, df[,]))

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

cor(df$HI_nDNA, df$pred_null_cv)
cor(df$HI_nDNA, df$pred_best_cv)

anova(null_model, best_model)
AIC(null_model, best_model)

# Save best model
saveRDS(null_model, "data/created/models/null_mixed_model.rds")
saveRDS(best_model, "data/created/models/best_mixed_model.rds")

# Plots
predictors <- attr(terms(best_model), "term.labels")

plot_data <- map_dfr(predictors, function(term) {
  
  pred <- ggpredict(
    best_model,
    terms = term
  )
  
  as.data.frame(pred) %>%
    mutate(
      x = as.character(x),
      predictor = term
    )
})

df_plot_cont <- plot_data |> 
  filter(predictor != "Region") 
df_plot_cont$x <- as.numeric(df_plot_cont$x)

df_plot_cont |> 
  ggplot(aes(x, predicted)) +
  geom_point() +
  geom_line() +
  facet_wrap(~predictor, scales = "free_x") 

df_plot_cat <- plot_data |> 
  filter(predictor == "Region") 

df_plot_cat |> 
  ggplot(aes(x, predicted)) +
  geom_point() +
  facet_wrap(~predictor, scales = "free_x") 
