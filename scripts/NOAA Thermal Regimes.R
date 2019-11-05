library(dplyr)
library(ggplot2)
library(broom)
library(stats)
library(tidyverse)
library(data.table)

setwd("~/HotDam/input/NOAA_pullR")
output <- "~/HotDam/input/NOAA_thermal_regimes"
files <- dir(path = ".", pattern = "*.csv")

#####This loop works for reading and writing files#####
for (i in 1:length(files)){
  stations <- basename(files)
  station_num <- str_sub(stations, end = -5)
  #print(station_num[i])
  fn <- paste0(station_num[i], "_tr", ".csv")
  fp <- file.path(output, fn)
  tryCatch(
    expr = {
      NOAAdata <- read.csv(files[i], header = T, sep = ",")
      NOAAdata <- na.omit(NOAAdata, c(NOAAdata$tmax, NOAAdata$date))
      NOAAdata$julian <- NaN
      NOAAdata$year <- NaN
      for (n in 1:nrow(NOAAdata)){
        temp <- as.POSIXlt(NOAAdata$date, "%Y-%m-%d", tz = "")
        NOAAdata$julian <- temp$yday
        NOAAdata$year <- format(as.Date(NOAAdata$date), format = "%Y")
        
        all_years_plot <- ggplot(NOAAdata, aes(x = NOAAdata$julian, y = NOAAdata$tmax), group = NOAAdata$date) + 
          geom_line(aes(colour = year)) + 
          #geom_smooth(method = "loess", se = FALSE, span = .6, colour = "red") + 
          theme_classic() + #scale_color_gradient(colours) + 
          labs(title = "Air Thermal Regime")
        
        smooth_vals <- predict(smooth.spline(NOAAdata$tmax ~ NOAAdata$julian))
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
        
        ggsave(all_years_plot, file = paste0(station_num[i],"_All_years",".png"), width = 14 , height = 10, units = "cm")
        ggsave(spline_plot, file = paste0(station_num[i],"_spline",".png"), width = 14 , height = 10, units = "cm")
        write.csv(dt_spline, file = fp)
        break()
      }
    },
    error = function(cond){
      message(paste0(station_num[i]))
      return(NA)
    }
  )
}
