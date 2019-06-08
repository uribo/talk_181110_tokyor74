## ----setup, include=FALSE------------------------------------------------
library(magrittr)
knitr::opts_chunk$set(cache = TRUE,
                      warning = FALSE, 
                      message = FALSE, 
                      error = FALSE)

## ---- eval = TRUE, echo = TRUE, results = "hide"-------------------------
library(sf)
library(dplyr)

## ---- eval = TRUE, echo = FALSE------------------------------------------
df_weather_20180815 <- 
  readr::read_rds(here::here("data-raw/weather.rds"))

## ---- eval = FALSE, echo = FALSE-----------------------------------------
## library(ggplot2)
## library(rnaturalearth)
## ne_jpn <-
##   ne_states(country = "Japan", returnclass = "sf") %>%
##   tibble::new_tibble(subclass = "sf")
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
## ggsave(here::here("figures", "rain.png"),
##        last_plot(),
##        bg = "#fafafa",
##        width  = 5,
##        height = 4,
##        dpi = 300)

## ---- eval = TRUE, echo = TRUE-------------------------------------------
df_train <- 
  df_weather_20180815 %>% 
  select(elevation, temperature_mean, rainy) %>% 
  st_set_geometry(NULL) %>% 
  as.data.frame()

coords <- 
  df_weather_20180815 %>% 
  st_coordinates() %>% 
  as.data.frame()

## ---- eval = TRUE, echo = TRUE-------------------------------------------
library(mlr)

spatial.task <- 
  makeClassifTask(target = "rainy", 
                  data = df_train, 
                  coordinates = coords,
                  positive = "TRUE")

learner.rf <- 
  makeLearner("classif.ranger", 
              predict.type = "prob")

## ---- eval = TRUE, echo = TRUE, results = "hide"-------------------------
resampling_cv <- 
  makeResampleDesc(method = "RepCV", 
                   fold = 5, reps = 5)

set.seed(123)
cv_out <- 
  resample(learner    = learner.rf, 
           task       = spatial.task,
           resampling = resampling_cv, 
           measures   = list(auc))

mean(cv_out$measures.test$auc, na.rm = TRUE)
# [1] 0.8568344

## ---- eval = TRUE, echo = TRUE, results = "hide"-------------------------
resampling_sp <-
  makeResampleDesc("SpRepCV", 
                   fold = 5, reps = 5)

set.seed(123)
sp_cv_out <- 
  resample(learner    = learner.rf, 
           task       = spatial.task,
           resampling = resampling_sp, 
           measures   = list(auc))
mean(sp_cv_out$measures.test$auc, na.rm = TRUE)
# [1] 0.7624839

## ---- eval = FALSE, echo = FALSE-----------------------------------------
## plots <-
##   createSpatialResamplingPlots(
##     spatial.task,
##     list("RepCV"   = cv_out,
##          "SpRepCV" = sp_cv_out),
##     crs = 4326,
##     repetitions = 1)
## 
## library(cowplot)
## theme_set(theme_gray(base_size = 8, base_family = "sans"))
## p_out <-
##   plot_grid(plotlist = plots[["Plots"]],
##           ncol = 5,
##           nrow = 2,
##           labels = plots[["Labels"]],
##           label_size = 4)
## 
## ggsave(here::here("figures", "Rplot.png"),
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
  dplyr::select(station_no,
                elevation, 
                temperature_mean,
                precipitation_sum) %>% 
  st_set_geometry(NULL) %>% 
  as.data.frame()

## ------------------------------------------------------------------------
set.seed(123)
model <- 
  caret::train(df_train[, c("elevation", "temperature_mean")],
        df_train$precipitation_sum,
      method　   = "rf",
      tuneLength = 1,
      importance = TRUE,
      trControl  = trainControl(method = "cv",
                                number = 5))

## ------------------------------------------------------------------------
model$results

## ------------------------------------------------------------------------
indices <- 
  CAST::CreateSpacetimeFolds(
    df_train,
    spacevar = "station_no",
    k        = 5,
    seed     = 123)

## ------------------------------------------------------------------------
set.seed(123)

model_LLO <- 
  caret::train(
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

