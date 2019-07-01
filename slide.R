## ----setup, include=FALSE------------------------------------------------
source(here::here("code.R"))
library(magrittr)
knitr::opts_chunk$set(cache = TRUE,
                      warning = FALSE, 
                      message = FALSE, 
                      error = FALSE)


## ---- eval = TRUE, echo = TRUE, results = "hide"-------------------------
library(sf)
library(tidyverse)


## ---- eval = TRUE, echo = FALSE------------------------------------------
df_weather_20180815 <- 
  read_rds(here::here("data-raw/weather.rds"))


## ---- eval = FALSE, echo = FALSE-----------------------------------------
## library(rnaturalearth)
## ne_jpn <-
##   ne_states(country = "Japan", returnclass = "sf") %>%
##   new_tibble(subclass = "sf")
## 
## ggplot() +
##   geom_sf(data = ne_jpn,
##           col = "gray20",
##           size = 0.4) +
##   geom_sf(data = df_weather_20180815,
##           aes(col = rainy),
##           size = 0.4,
##           alpha = 0.5,
##           show.legend = "point") +
##   coord_sf(datum = NA) +
##   theme_light(base_family = "IPAexGothic",
##               base_size = 8) +
##   theme(rect = element_rect(fill = "#fafafa"),
##         plot.background = element_rect(
##           fill = "#fafafa",
##                                         color = "#fafafa"
##         ),
##         panel.border = element_blank(),
##         panel.background = element_rect(fill = "#fafafa",
##                                         color = "#fafafa"),
##         legend.background = element_rect(color = "#fafafa"),
##         legend.key = element_rect(fill = "#fafafa", size = 4),
##         legend.position = c(0.2, 0.95),
##         legend.direction = "horizontal") +
##   ggtitle("2018年8月15日は降水日であったか",
##           "降水日の基準は一日の降水量が0.1mm以上")
## 
## ggsave(here::here("figures/rain.png"),
##        last_plot(),
##        bg = "#fafafa",
##        width  = 5,
##        height = 4,
##        dpi = 300)


## ---- eval = TRUE, echo = TRUE-------------------------------------------
df_train <- 
  df_weather_20180815 %>% 
  select(elevation, temperature_mean, rainy) %>% 
  st_set_geometry(NULL)

coords <- 
  df_weather_20180815 %>% 
  st_coordinates()


## ---- eval = TRUE, echo = TRUE-------------------------------------------
library(mlr)

spatial_task <- 
  makeClassifTask(target = "rainy", 
                  data = as.data.frame(df_train), 
                  coordinates = as.data.frame(coords),
                  positive = "TRUE")

learner_rf <- 
  makeLearner("classif.ranger", 
              predict.type = "prob")


## ---- eval = TRUE, echo = TRUE, results = "hide"-------------------------
# データがランダムに記録されていることを想定し、RepCV
resampling_cv <- makeResampleDesc(method = "RepCV", 
                   folds = 5, reps = 5)
set.seed(123)
cv_out <- 
  resample(learner    = learner_rf, 
           task       = spatial_task,
           resampling = resampling_cv, 
           measures   = list(auc))

mean(cv_out$measures.test$auc, na.rm = TRUE)
# [1] 0.8544815


## ---- eval = TRUE, echo = TRUE, results = "hide"-------------------------
resampling_sp <-
  makeResampleDesc("SpRepCV", folds = 5, reps = 5)
set.seed(123)
sp_cv_out <- 
  resample(learner    = learner_rf, 
           task       = spatial_task,
           resampling = resampling_sp, 
           measures   = list(auc))
mean(sp_cv_out$measures.test$auc, na.rm = TRUE)
# [1] 0.7891348


## ---- eval = FALSE, echo = FALSE-----------------------------------------
## plots <-
##   createSpatialResamplingPlots(
##     spatial_task,
##     list("RepCV"   = cv_out,
##          "SpRepCV" = sp_cv_out),
##     crs = 4326,
##     repetitions = 1)
## 
## library(cowplot)
## theme_set(theme_gray(base_size = 4, base_family = "sans"))
## p_out <-
##   plot_grid(plotlist = plots[["Plots"]],
##           ncol = 5,
##           nrow = 2,
##           labels = plots[["Labels"]],
##           label_size = 4)
## 
## ggsave(here::here("figures/Rplot.png"),
##        p_out,
##        width = 8,
##        height = 4,
##        dpi = "retina")


## ---- eval = TRUE, echo = TRUE-------------------------------------------
library(CAST)
library(caret)


## ------------------------------------------------------------------------
df_train <- 
  df_weather_20180815 %>% 
  select(station_no,
                elevation, 
                temperature_mean,
                precipitation_sum) %>% 
  st_set_geometry(NULL)


## ------------------------------------------------------------------------
set.seed(123)
model <- 
  train(df_train[, c("elevation", "temperature_mean")],
        df_train$precipitation_sum,
      method　   = "rf",
      tuneLength = 1,
      importance = TRUE,
      trControl  = trainControl(method = "cv",
                                number = 5))


## ---- eval = FALSE, echo = TRUE------------------------------------------
## model$results


## ---- eval = TRUE, echo = FALSE------------------------------------------
model$results %>% 
  knitr::kable()


## ------------------------------------------------------------------------
indices <- 
  CreateSpacetimeFolds(
    df_train,
    spacevar = "station_no",
    k        = 10,
    seed     = 123)


## ------------------------------------------------------------------------
set.seed(123)

model_LLO <- 
  train(
    df_train[, c("elevation", "temperature_mean")],
    df_train$precipitation_sum,
    method     = "rf",
    tuneLength = 1,
    importance = TRUE,
    trControl  = trainControl(method = "cv",
                              index = indices$index))


## ------------------------------------------------------------------------
mean(model$results$RMSE)
mean(model_LLO$results$RMSE)

