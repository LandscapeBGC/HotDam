#install.packages('ggplot')
library(here)
library(dplyr)
library(ggplot2)
library(broom)
library(stats)
library(tidyverse)
library(data.table)
library(stringr)
library(ggplot)

start_time <- Sys.time()

#### Establish Relavent File Paths ####

path1 <- here()
STAID <- read.csv(file.path(path1,'NWIS_NOAA_NID_Eco_clean.csv')) # Create data frame from csv linking ids

path2 <- here("input", "SW_T_Dam_v2") #removes confusion regarding setwd()
file <- dir(path = path2, pattern = "*.csv")
files <- as.list(file)

path3 <- here("input/Dam_thermal_regimes_post_v2")
dir.create(path3) 

path4 <- here("input/Dam_thermal_regimes_pre_v2")
dir.create(path4) 

input <- here("input/SW_T_Dam_v2")

#### This loop works for reading and writing files ####

for (i in 1:length(files)){
  station_num <- str_sub(files, end = -5) # extract station ids
  fn <- paste0(station_num[i], "_tr", ".csv") # create filename for clean station data
  fp3 <- file.path(path3, fn) # create filepath for clean station data
  fp4 <- file.path(path4, fn)
  
  for (x in 1:nrow(STAID)){
    if (substring(toString(STAID$STAID_edt[x]),6) == station_num[i]){ # for loop extracts relavent contruction date for USGS gauge and Dams
      CnstrDate <- STAID$YEAR_COMPL[x]
      if (STAID$YEAR_COMPL[x] != 0){ # disregard Dams that do not include Construction Date
        print(CnstrDate)
        tryCatch(
          expr = {
            USGSdata <- read.csv(file.path(input,files[i]), header = T, sep = ",")
            USGSdata <- na.omit(USGSdata, c(USGSdata$Wtemp, USGSdata$Date))
            USGSdata$julian <- NaN
            USGSdata$year <- NaN
            
            temp <- as.POSIXlt(USGSdata$Date, "%Y-%m-%d", tz = "")
            USGSdata$julian <- temp$yday
            USGSdata$year <- format(as.Date(USGSdata$Date), format = "%Y")
            
            ########################################
            #### Post Construction Calculations ####
            ########################################
            
            USGS_post <- USGSdata %>% filter(year>CnstrDate)
            USGS_post_fnl <- USGS_post %>% filter(year>2000)
            pstRcrdCnt_val <- length(unique(USGS_post_fnl[["year"]]))
            
            all_years_plot <- ggplot(USGS_post_fnl, aes(x = USGS_post_fnl$julian, y = USGS_post_fnl$Wtemp), group = USGS_post_fnl$Date) +
              geom_line(aes(colour = year)) +
              theme_classic() + 
              labs(title = "Dam Thermal Regime")
            
            smooth_vals <- predict(smooth.spline(USGS_post_fnl$Wtemp ~ USGS_post_fnl$julian))
            smooth_vals1 <- smooth_vals$x
            smooth_vals2 <- smooth_vals$y
            
            df_x <- data.frame(matrix(unlist(smooth_vals1), nrow = length(smooth_vals1), byrow = T))
            df_y <- data.frame(matrix(unlist(smooth_vals2), nrow = length(smooth_vals2), byrow = T))
            
            dt_x <- data.table(df_x)
            dt_y <- data.table(df_y)
            
            dt_x[, Index:= 1:.N]
            dt_y[, Index2:= 1:.N]
            
            dt_spline <- cbind(dt_x, dt_y)
            
            names(dt_spline)[1] <- "x"
            names(dt_spline)[3] <- "y"
            
            spline_plot <- ggplot(dt_spline,aes(x = dt_spline$x,y = dt_spline$y))+geom_point(color = "red")
            
            dt_spline$RcrdCnt <- pstRcrdCnt_val # Popluate column of the total number of annual records (Used in analysis.R)
            
            ggsave(all_years_plot, file = file.path(path3, paste0(station_num[i],"_All_years",".png")), width = 14 , height = 10, units = "cm")
            ggsave(spline_plot, file = file.path(path3, paste0(station_num[i],"_spline",".png")), width = 14 , height = 10, units = "cm")
            write.csv(dt_spline, file = fp3)
            
            #######################################
            #### Pre Construction Calculations ####
            #######################################
            
            USGS_pre <- USGSdata %>% filter(year<CnstrDate)
            preRcrdCnt_val <- length(unique(USGS_pre[["year"]]))
            
            all_years_plot <- ggplot(USGS_pre, aes(x = USGS_pre$julian, y = USGS_pre$Wtemp), group = USGS_pre$Date) +
              geom_line(aes(colour = year)) +
              theme_classic() + 
              labs(title = "Dam Thermal Regime")
            
            smooth_vals <- predict(smooth.spline(USGS_pre$Wtemp ~ USGS_pre$julian))
            smooth_vals1 <- smooth_vals$x
            smooth_vals2 <- smooth_vals$y
            
            df_x <- data.frame(matrix(unlist(smooth_vals1), nrow = length(smooth_vals1), byrow = T))
            df_y <- data.frame(matrix(unlist(smooth_vals2), nrow = length(smooth_vals2), byrow = T))
            
            dt_x <- data.table(df_x)
            dt_y <- data.table(df_y)
            
            dt_x[, Index:= 1:.N]
            dt_y[, Index2:= 1:.N]
            
            dt_spline <- cbind(dt_x, dt_y)
            
            names(dt_spline)[1] <- "x"
            names(dt_spline)[3] <- "y"
            
            spline_plot <- ggplot(dt_spline,aes(x = dt_spline$x,y = dt_spline$y))+geom_point(color = "red")
            
            dt_spline$RcrdCnt <- preRcrdCnt_val # Popluate column of the total number of annual records (Used in analysis.R)
            
            ggsave(all_years_plot, file = file.path(path4, paste0(station_num[i],"_All_years",".png")), width = 14 , height = 10, units = "cm")
            ggsave(spline_plot, file = file.path(path4, paste0(station_num[i],"_spline",".png")), width = 14 , height = 10, units = "cm")
            write.csv(dt_spline, file = fp4)
            
            break()
          },
      error = function(cond){
        message(paste0(station_num[i]))
        return(NA)}
    )}}}
  }

end_time <- Sys.time()

time <- end_time - start_time

print(time)
print("Script Complete")