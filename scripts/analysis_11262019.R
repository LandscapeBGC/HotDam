#install.packages("here")
#install.packages('data.table')
#install.packages('tidyverse')
#install.packages('broom')
#install.packages('dplyr')
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

path2 <- file.path(here(), 'Thermal_metrics_12022019.csv') # make copy of NWIS_NOAA_NID_Eco_clean.csv that can be edited
write.csv(Orig_file, file = path2)

Main_df <- read.csv(file.path(path1,'Thermal_metrics_12022019.csv'))

##################################################
#### Danielle's Cooling/Warming Rate function ####
##################################################

annual_rate <- function(df){
  start <- 58 #end of feburary as to remove freeze period
  max <- which.max(df$y) #row number of max value
  end <- 365
  # create df for each warming/cooling segement
  warm_seg <- df[start:max,]
  cool_seg <- df[max:end,]
  
  #linear fit to warming annual trend
  warm_fit <- lm(warm_seg$y ~warm_seg$X)
  warm_rate <- warm_fit[["coefficients"]][["warm_seg$X"]]
  
  #linear fit to cooling annual trend
  cool_fit <- lm(cool_seg$y ~ cool_seg$X)
  cool_rate <- cool_fit[["coefficients"]][["cool_seg$X"]]
  
  result  <- list(warm_rate_degpd = warm_rate, cool_rate_degpd = cool_rate)
  
  return(result)
}

#######################################
#### Create Thermal Metric Columns #### 
#######################################

Main_df$PstSplnCnt <- NA
Main_df$pstRcrdCnt <- NA
Main_df$pstP90 <- NA
Main_df$pstP90Cnt <- NA
Main_df$pstP50 <- NA
Main_df$pstP10 <- NA
Main_df$pstP10Cnt <- NA
Main_df$pstTmax <- NA
Main_df$pstTmin <- NA
Main_df$pstTmean <- NA
Main_df$pstTmaxT <- NA
Main_df$pstTminT <- NA
Main_df$pstCoolRt <- NA
Main_df$pstWarmRt <- NA
Main_df$pstCffcntVrtn <- NA
Main_df$pstP25cnt <- NA
Main_df$pstP75cnt <- NA

Main_df$PreSplnCnt <- NA
Main_df$preRcrdCnt <- NA
Main_df$preTmax <- NA
Main_df$preTmin <- NA
Main_df$preTmean <- NA
Main_df$preTmaxT <- NA
Main_df$preTminT <- NA

Main_df$NOAA_SplnCnt <- NA
Main_df$NOAA_TmaxT <- NA
Main_df$NOAA_TminT <- NA
Main_df$Lag_TmaxT <- NA
Main_df$Lag_TminT <- NA
Main_df$NOAARcrdCn <- NA

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
        tmean <- mean(analysis.df$y)
        tmax_timing <- analysis.df$x[which.max(analysis.df$y)]
        tmin_timing <- analysis.df$x[which.min(analysis.df$y)]
        P90 <-qnorm(0.90,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P50 <- qnorm(0.50,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P10 <- qnorm(0.10,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P75 <- qnorm(0.75,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P25 <- qnorm(0.25,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P10Cnt <- sum(analysis.df$y < P10)
        P90Cnt <- sum(analysis.df$y > P90)
        P25Cnt <- sum(analysis.df$y < P25)
        P75Cnt <- sum(analysis.df$y > P75)
        RcrdCnt <- analysis.df$RcrdCnt[1]
        SplineCnt <- length(unique(analysis.df$x))
        CoefficientVariation <- sd(analysis.df$y, na.rm = TRUE)/mean(analysis.df$y, na.rm = TRUE)
        for (z in 1:nrow(Main_df)){
          if (NWIS_Str[n] == str_sub(Main_df$STAID_edt[z], start = 6)){
            Main_df$pstTmax[z] <- tmax
            Main_df$pstTmin[z] <- tmin
            Main_df$pstTmean[z] <- tmean
            Main_df$pstTmaxT[z] <- tmax_timing
            Main_df$pstTminT[z] <- tmin_timing
            Main_df$pstP90[z] <- P90
            Main_df$pstP90Cnt[z] <- P90Cnt
            Main_df$pstP50[z] <- P50
            Main_df$pstP10[z] <- P10
            Main_df$pstP10Cnt[z] <- P10Cnt
            Main_df$pstRcrdCnt[z] <- RcrdCnt
            df <- analysis.df
            Main_df$pstCoolRt[z] <- annual_rate(df)[2]
            Main_df$pstWarmRt[z] <- annual_rate(df)[1]
            Main_df$pstCffcntVrtn[z] <- CoefficientVariation 
            Main_df$pstP25cnt[z] <- P25Cnt
            Main_df$pstP75cnt[z] <- P75Cnt
            Main_df$PstSplnCnt[z] <- SplineCnt
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
        SplineCnt <- length(unique(analysis.df$x))
        for (z in 1:nrow(Main_df)){
          if (NWIS_Str[n] == str_sub(Main_df$STAID_edt[z], start = 6)){
            Main_df$preTmax[z] <- tmax
            Main_df$preTmin[z] <- tmin
            Main_df$preTmean[z] <- tmean
            Main_df$preTmaxT[z] <- tmax_timing
            Main_df$preTminT[z] <- tmin_timing
            Main_df$preRcrdCnt[z] <- RcrdCnt
            Main_df$PreSplnCnt[z] <- SplineCnt
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
                  Main_df$NOAASplnCnt[n] <- length(unique(analysis.df$x))
                  RcrdCnt <- analysis.df$pstRcrdCnt[1]
                  Main_df$NOAA_TmaxT[n] <- tmax_timing
                  Main_df$NOAA_TminT[n] <- tmin_timing
                  
                  tmax_tm_diff <- abs(Main_df$pstTmaxT[n] - tmax_timing)
                  if (tmax_tm_diff > 180){
                    if (tmax_timing < Main_df$pstTmaxT[n]){
                      tmax_timing = tmax_timing + 365
                    }
                    else {
                      tmax_timing = tmax_timing - 365
                    }
                  }
                  Main_df$Lag_TmaxT[n] <-  abs(Main_df$pstTmaxT[n] - tmax_timing)
                  
                  tmin_tm_diff <- abs(Main_df$pstTminT[n] - tmin_timing)
                  if (tmin_tm_diff > 180){
                    if (tmin_timing < Main_df$pstTminT[n]){
                      tmin_timing = tmin_timing + 365
                    }
                    else {
                      tmin_timing = tmin_timing - 365
                    }
                  }
                  Main_df$Lag_TminT[n] <- abs(Main_df$pstTminT[n] - tmin_timing)
                  
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

#################################################
#### Create Path + List of Reference Records ####
#################################################

path6 <- here('input/Ref_thermal_regimes')
Rfrnc <- dir(path = path6, pattern = "*.csv")
Rfrnc_lst <- str_sub(Rfrnc, end = -8)
Rfrnc_St_num <- as.list(Rfrnc_lst) 

###################################################################################
#### Calcuate Thermal Metrics of Reference Records and populate main dataframe ####
###################################################################################

NWIS_Str<- str_split(Main_df$STAID_edt, pattern = ",")
NWIS_Str <- str_split(str_sub(NWIS_Str, start = 6), pattern = ",")

for (n in 1:length(NWIS_Str)){
  if (NWIS_Str[n] %in% Rfrnc_St_num == TRUE) {
    tryCatch(
      expr = {
        x <- c(file.path(here('input/Ref_thermal_regimes/xyxyxyxy_tr.csv')))
        NWISsubstitution <- gsub("xyxyxyxy",NWIS_Str[n], x)
        print(NWISsubstitution)
        analysis.df <- read.csv(NWISsubstitution)
        tmax <- max(analysis.df$y)
        tmin <- min(analysis.df$y)
        tmean <- mean(analysis.df$y)
        tmax_timing <- analysis.df$x[which.max(analysis.df$y)]
        tmin_timing <- analysis.df$x[which.min(analysis.df$y)]
        P90 <- qnorm(0.90,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P50 <- qnorm(0.50,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P10 <- qnorm(0.10,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P25 <- qnorm(0.25,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P75 <- qnorm(0.75,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P10Cnt <- sum(analysis.df$y < P10)
        P90Cnt <- sum(analysis.df$y > P90)
        P25Cnt <- sum(analysis.df$y < P25)
        P75Cnt <- sum(analysis.df$y > P75)
        RcrdCnt <- analysis.df$pstRcrdCnt[1]
        SplineCnt <- length(unique(analysis.df$x))
        CoefficientVariation <- sd(analysis.df$y, na.rm = TRUE)/mean(analysis.df$y, na.rm = TRUE)
        for (z in 1:nrow(Main_df)){
          if (NWIS_Str[n] == str_sub(Main_df$STAID_edt[z], start = 6)){
            Main_df$Ref[z] <- '1'
            Main_df$pstTmax[z] <- tmax
            Main_df$pstTmin[z] <- tmin
            Main_df$pstTmaxT[z] <- tmax_timing
            Main_df$pstTminT[z] <- tmin_timing
            Main_df$pstTmean[z] <- tmean
            Main_df$pstP90[z] <- P90
            Main_df$pstP90Cnt[z] <- P90Cnt
            Main_df$pstP50[z] <- P50
            Main_df$pstP10[z] <- P10
            Main_df$pstP10Cnt[z] <- P10Cnt
            Main_df$pstRcrdCnt[z] <- RcrdCnt
            df <- analysis.df
            Main_df$pstCoolRt[z] <- annual_rate(df)[2]
            Main_df$pstWarmRt[z] <- annual_rate(df)[1]
            Main_df$pstCffcntVrtn[z] <- CoefficientVariation
            Main_df$pstP25cnt[z] <- P25Cnt
            Main_df$pstP75cnt[z] <- P75Cnt
            Main_df$PstSplnCnt[z] <- SplineCnt
          }}},error = function(cond){
            message(paste0(NWIS_Str[n]))
            return(NA)})}
  else{
    Main_df$Ref[n] <- '0'
  }
}

Main_df2 <- vapply(Main_df, paste, collapse = ", ", character(1L))

df_Place2 = data.frame(lapply(Main_df, as.character), stringsAsFactors=FALSE)

#######################################
write.csv(df_Place2, path2)
#######################################
end_time <- Sys.time()
time <- end_time - start_time
print(time)
print("Script Complete")
#######################################