library(here)
library(dplyr)
library(ggplot2)
library(broom)
library(stats)
library(tidyverse)
library(data.table)

start_time <- Sys.time()

input <- here("input/SW_T_Ref") #removes confusion regarding setwd()
file <- dir(path = input, pattern = "*.csv")
files <- as.list(file)

output <- here("input/Ref_thermal_regimes")
dir.create(output)

#####This loop works for reading and writing files###
for (i in 1:length(files)){
  station_num <- str_sub(files, end = -5)
  fn <- paste0(station_num[i], "_tr", ".csv")
  fp <- file.path(output, fn)
  for (x in 1:length(files)){
    if (file[x] == file[i]){
      tryCatch(
        expr = {
          print(files[i])
          USGSdata <- read.csv(file.path(input,files[i]), header = T, sep = ",")
          USGSdata <- na.omit(USGSdata, c(USGSdata$Wtemp, USGSdata$Date))
          USGSdata$julian <- NaN
          USGSdata$year <- NaN
          temp <- as.POSIXlt(USGSdata$Date, "%Y-%m-%d", tz = "")
          
          USGSdata$julian <- temp$yday
          USGSdata$year <- format(as.Date(USGSdata$Date), format = "%Y")
          
          USGSdata_filtered <- USGSdata %>% filter(year>2000)
          RcrdCnt_val <- length(unique(USGSdata_filtered[["year"]]))
          
          all_years_plot <- ggplot(USGSdata_filtered, aes(x = USGSdata_filtered$julian, y = USGSdata_filtered$Wtemp), group = USGSdata_filtered$Date) + 
            geom_line(aes(colour = year)) + 
            theme_classic() + 
            labs(title = "Reference Thermal Regime")
          
          smooth_vals <- predict(smooth.spline(USGSdata_filtered$Wtemp ~ USGSdata_filtered$julian))
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
          
          dt_spline$pstRcrdCnt <- RcrdCnt_val # Popluate column of the total number of annual records (Used in analysis.R)
          
          ggsave(all_years_plot, file = file.path(output, paste0(station_num[i],"_All_years",".png")), width = 14 , height = 10, units = "cm")
          ggsave(spline_plot, file = file.path(output, paste0(station_num[i],"_spline",".png")), width = 14 , height = 10, units = "cm")
          write.csv(dt_spline, file = fp)
          break()
        },
    
        error = function(cond){
          message(paste0(station_num[i]))
          return(NA)}
      )}
    }
  }

end_time <- Sys.time()

time <- end_time - start_time

print(time)
print("Script Complete")
