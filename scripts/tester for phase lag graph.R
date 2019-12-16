##noaa and usgs phase lag

library(dplyr)
library(stringr)
library(tidyverse)
library(here)
library(ggplot2)

output <- here("input/Dam_Purpose_LocFiles")

setwd("~/HotDam3")

df <- read.csv("Thermal_metrics_v2_PCA_cut.csv", header = TRUE, stringsAsFactors = FALSE)
df_NWIS <- str_split(df$STAID_edt, pattern = ",")
df_NWIS <- str_split(str_sub(df_NWIS, start = 6), pattern = ",")
df_NOAA <- str_split(df$NOAA_ID, pattern = ",")
df_NOAA <- str_split(str_sub(df_NOAA, start = 4), pattern = ",")

for (h in 1:length(df)){
  usgs <- df$STAID_edt
  usgs <- str_split(str_sub(usgs, start = 6) , pattern = ",")
  usgs <- paste0(usgs, "_tr", ".csv")
  noaa <- df$NOAA_ID
  noaa <- paste0(noaa, ".csv")
  tryCatch(
    expr = {
      setwd("~/HotDam3/input/Dam_thermal_regimes_post_v2")
      dam_data <- read.csv(usgs[h], header = TRUE, stringsAsFactors = FALSE)
      setwd("~/HotDam3/input/NOAA_thermal_regimes")
      noaa_data <- read.csv(noaa[h], header = TRUE, stringsAsFactors = FALSE)
      phase_plot <- ggplot(dam_data, aes(x=dam_data$X, y=dam_data$y)) + 
        geom_line(color = "blue") +
        geom_line(data = noaa_data, aes(x = noaa_data$X, y = noaa_data$y), color = "red") +
        labs(title = paste0(usgs[h]," , ", noaa[h]))
      
      setwd("~/HotDam3/results/figures/phase_lag")
      ggsave(paste0(usgs[h]," , ", noaa[h], "_phase", ".png"))
    },
    error = function(cond){
      message(paste0(usgs[h], ",", noaa[h]))
      return(NA)
    }
  )
}

dam_data <- read.csv(usgs[h], header = TRUE, stringsAsFactors = FALSE)
noaa_data <- read.csv(noaa[h], header = TRUE, stringsAsFactors = FALSE)
ggplot(dam_data, aes(x=dam_data$X, y=dam_data$y)) + 
  geom_line() +
  geom_line(data = noaa_data, aes(x = noaa_data$X, y = noaa_data$y))

setwd("~/HotDam3/input/Dam_thermal_regimes")

dam_files <- dir(path = ".", pattern = "*.csv")
dam_files <- as.list(dam_files)
for (i in 1:length(dam_files)){
  USGS_num <- str_sub(dam_files, end = -8)
}
USGS_num <- as.list(USGS_num)

setwd("~/HotDam3/input/NOAA_thermal_regimes")

weather_files <- dir(path = ".", pattern = "*.csv")
weather_files <- as.list(weather_files)
for (i in 1:length(weather_files)){
  NOAA_num <- str_sub(weather_files, start = 4, end = -8)
}
NOAA_num <- as.list(NOAA_num)

NWIS_list <- vector('list')
NOAA_list <- vector('list')
dam_file_list <- vector('list')
weather_file_list <- vector('list')
i <- 1
k <- 1

for (n in 1:length(df_NWIS)){
  if (df_NWIS[n] %in% USGS_num == TRUE){
    NWIS_list[i] <- df_NWIS[n]
    for (m in 1:length(NWIS_list)){
      dam_file_list[i] <- paste0(NWIS_list[m], "_tr", ".csv")
    }
    i <- i+1
  }
}

for (x in 1:length(df_NOAA)){
  if (df_NOAA[x] %in% NOAA_num == TRUE){
    NOAA_list[k] <- df_NOAA[x]
    for (y in 1:length(NOAA_list)){
      weather_file_list[k] <- paste0(NOAA_list[y], "_tr", ".csv")
    }
    k <- k+1
  }
}

