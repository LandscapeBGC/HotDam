#install.packages("here")
library(here)
library(dplyr)
library(ggplot2)
library(broom)
library(stats)
library(tidyverse)
library(data.table)

#######################################
start_time <- Sys.time()
#######################################

path1 <- here()
Orig_file <- read.csv(file.path(path1,'NWIS_NOAA_NID_Eco_clean.csv'))

path2 <- file.path(here(), 'Thermal_metrics.csv') # make copy of NWIS_NOAA_NID_Eco_clean.csv that can be edited
write.csv(Orig_file, file = path2)

Main_df <- read.csv(file.path(path1,'Thermal_metrics.csv'))

#######################################
#### Create Thermal Metric Columns #### 
#######################################

Main_df$pstRcrdCnt <- NaN
Main_df$pstTmax <- NaN
Main_df$pstTmin <- NaN
Main_df$pstTmaxT <- NaN
Main_df$pstTminT <- NaN

Main_df$preRcrdCnt <- NaN
Main_df$preTmax <- NaN
Main_df$preTmin <- NaN
Main_df$preTmaxT <- NaN
Main_df$preTminT <- NaN

Main_df$NOAA_TmaxT <- NaN
Main_df$NOAA_TminT <- NaN
Main_df$Lag_TmaxT <- NaN
Main_df$Lag_TminT <- NaN
Main_df$NOAARcrdCn <- NaN


#########################################################
#### Create Path + List of Post-Construction Records ####
#########################################################

path3 <- here('input/Dam_thermal_regimes_post')
pstDam <- dir(path = path3, pattern = "*.csv")
pstDam_lst <- str_sub(pstDam, end = -8)
pstUSGs_St_num <- as.list(pstDam_lst) # create an iterable list of USGS gauges with post construction temperature data

##########################################################################################
#### Calcuate Thermal Metrics of Post-Contruction Records and populate main dataframe ####
##########################################################################################

NWIS_Str<- str_split(Main_df$STAID_edt, pattern = ",")
NWIS_Str <- str_split(str_sub(NWIS_Str, start = 6), pattern = ",")

for (n in 1:length(NWIS_Str)){
  if (NWIS_Str[n] %in% pstUSGs_St_num == TRUE) {
    tryCatch(
      expr = {
        x <- c(file.path(here('input/Dam_thermal_regimes_post/xyxyxyxy_tr.csv')))
        NWISsubstitution <- gsub("xyxyxyxy",NWIS_Str[n], x)
        print(NWISsubstitution)
        analysis.df <- read.csv(NWISsubstitution)
        tmax <- max(analysis.df$y)
        print(tmax)
        tmin <- min(analysis.df$y)
        tmax_timing <- analysis.df$x[which.max(analysis.df$y)]
        tmin_timing <- analysis.df$x[which.min(analysis.df$y)]
        RcrdCnt <- analysis.df$RcrdCnt[1]
        print(RcrdCnt)
        for (z in 1:nrow(Main_df)){
          if (NWIS_Str[n] == str_sub(Main_df$STAID_edt[z], start = 6)){
            Main_df$pstRcrdCnt
            Main_df$pstTmax[z] <- tmax
            Main_df$pstTmin[z] <- tmin
            Main_df$pstTmaxT[z] <- tmax_timing
            Main_df$pstTminT[z] <- tmin_timing
            Main_df$pstRcrdCnt[z] <- RcrdCnt
          }}},error = function(cond){
          message(paste0(NWIS_Str[n]))
          return(NA)})}}

########################################################
#### Create Path + List of Pre-Construction Records ####
########################################################

path4 <- here('input/Dam_thermal_regimes_pre')
preDam <- dir(path = path4, pattern = "*.csv")
preDam_lst <- str_sub(preDam, end = -8)
preUSGs_St_num <- as.list(preDam_lst) # create an iterable list of USGS gauges with post construction temperature data

#########################################################################################
#### Calcuate Thermal Metrics of Pre-Contruction Records and populate main dataframe ####
#########################################################################################

NWIS_Str<- str_split(Main_df$STAID_edt, pattern = ",")
NWIS_Str <- str_split(str_sub(NWIS_Str, start = 6), pattern = ",")

for (n in 1:length(NWIS_Str)){
  if (NWIS_Str[n] %in% preUSGs_St_num == TRUE) {
    tryCatch(
      expr = {
        x <- c(file.path(here('input/Dam_thermal_regimes_pre/xyxyxyxy_tr.csv')))
        NWISsubstitution <- gsub("xyxyxyxy",NWIS_Str[n], x)
        print(NWISsubstitution)
        analysis.df <- read.csv(NWISsubstitution)
        tmax <- max(analysis.df$y)
        print(tmax)
        tmin <- min(analysis.df$y)
        tmean <- mean(analysis.df$y)
        tmax_timing <- analysis.df$x[which.max(analysis.df$y)]
        tmin_timing <- analysis.df$x[which.min(analysis.df$y)]
        RcrdCnt <- analysis.df$RcrdCnt[1]
        print(RcrdCnt)
        for (z in 1:nrow(Main_df)){
          if (NWIS_Str[n] == str_sub(Main_df$STAID_edt[z], start = 6)){
            Main_df$preTmax[z] <- tmax
            Main_df$preTmin[z] <- tmin
            Main_df$preTmaxT[z] <- tmax_timing
            Main_df$preTminT[z] <- tmin_timing
            Main_df$preRcrdCnt[z] <- RcrdCnt
          }}},error = function(cond){
            message(paste0(NWIS_Str[n]))
            return(NA)})}}

########################################################
#### Create Path + List of Pre-Construction Records ####
########################################################

path5 <- here('input/NOAA_thermal_regimes')
NOAA <- dir(path = path5, pattern = "*.csv")
NOAA_lst <- str_sub(NOAA, end = -5)
NOAA_St_num <- as.list(NOAA_lst) # create an iterable list of USGS gauges with post construction temperature data

##############################################################
#### Calcuate Julian Date of NOAA populate main dataframe ####
##############################################################

NOAA_Str<- str_split(Main_df$NOAA_ID, pattern = ",")
NOAA_Str <- str_split(str_sub(NOAA_Str), pattern = ",")

tryCatch(
  expre = {
    for (n in 1:length(NOAA_Str)) {
      for (y in 1:length(NOAA_St_num)){
        tryCatch(
          expr = {
            if (NOAA_Str[[n]] == NOAA_St_num[[y]]){
              tryCatch(
                expr = {
                  x <- c(file.path(here('input/NOAA_thermal_regimes/xyxyxyxy.csv')))
                  NOAAsubstitution <- gsub("xyxyxyxy",NOAA_St_num[y], x)
                  print(NOAAsubstitution)
                  analysis.df <- read.csv(NOAAsubstitution)
                  tmax <- max(analysis.df$y)
                  tmin <- min(analysis.df$y)
                  tmax_timing <- analysis.df$x[which.max(analysis.df$y)]
                  tmin_timing <- analysis.df$x[which.min(analysis.df$y)]
                  RcrdCnt <- analysis.df$pstRcrdCnt[1]
                  Main_df$NOAA_TmaxT[n] <- tmax_timing
                  Main_df$NOAA_TminT[n] <- tmin_timing
                  Main_df$Lag_TmaxT[n] <-  Main_df$pstTmaxT[n] - tmax_timing
                  Main_df$Lag_TminT[n] <- Main_df$pstTminT[n] - tmax_timing
                  Main_df$NOAARcrdCn[n] <- RcrdCnt
                }
                ,error = function(cond){
                      message(paste0(NOAA_St_num[[y]]))
                      return(NA)
                  }
                )
              }
            }
          ,error = function(cond){
            message(paste0(NOAA_Str[[n]]))
            return(NA)
            }
          )
        }
      }
    }
  ,error = function(cond){
            message(paste0(NOAA_Str[[n]]))
            return(NA)
    }
  )

###################################################################################
#### Calcuate Thermal Metrics of Reference Records and populate main dataframe ####
###################################################################################



#######################################
write.csv(Main_df, path2)
#######################################
end_time <- Sys.time()
time <- end_time - start_time
print(time)
print("Script Complete")
#######################################