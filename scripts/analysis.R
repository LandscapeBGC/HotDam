library(dplyr)
library(ggplot2)
library(broom)
library(stats)
library(tidyverse)
library(data.table)

setwd("~/HotDam/input/Dam_thermal_regimes")
Dam_list <- list.files("~/HotDam/input/Dam_thermal_regimes")
USGs_station_num <- str_sub(Dam_list, end = -8)

setwd("~/HotDam/")
Main_df <- read.csv("NWIS_NID_NOAA_clean.csv", header = T, stringsAsFactors = F)
Main_df$tmax <- NaN
Main_df$tmin <- NaN

NWIS_Str<- str_split(Main_df$NWIS_ID, pattern = ",")
NWIS_Str <- paste0("0", NWIS_Str)

for (n in 1:length(NWIS_Str)){
  if (NWIS_Str[n] %in% USGs_station_num == TRUE) {
    x <- c('~/HotDam/input/Dam_thermal_regimes/thy_tr.csv')
    NWISsubstitution <- gsub("thy",toString(NWIS_Str[n]), x)  
    analysis.df <- read.csv(NWISsubstitution)
    tmax <- max(analysis.df$y)
    tmin <- min(analysis.df$y)
    #print(tmax)
    for (x in 1:length(Main_df)){
      if (NWIS_Str[n] == paste0("0",toString(Main_df$NWIS_ID[x]))){
        print('hazzah!')
        Main_df$tmax[x] <- tmax
        Main_df$tmin[x] <- tmin}}}}
#if NWIS_Str[n] %in% Main_df
#write.csv(Main_df, Main_edit)