library(here)
library(dplyr)
library(ggplot2)
library(broom)
library(stats)
library(tidyverse)
library(data.table)

start_time <- Sys.time()

input <- here("input/NOAA_pullR") #removes confusion regarding setwd()
file <- dir(path = input, pattern = "*.csv")
files <- as.list(file)

output <- here("input/NOAA_thermal_regimes")
dir.create(output)

station_num <- str_sub(files, end = -5)
station_num <- as.list(station_num)

#####This loop works for reading and writing files#####
for (i in 1:length(files)){
  fn <- paste0(station_num[i], "_tr", ".csv")
  fp <- file.path(output, fn)
  print(fp) 
  for (x in 1:length(files)){
    if (file[x] == file[i]){
      tryCatch(
        expr = {
          print (file[i])
          NOAAdata <- read.csv(file.path(input,files[i]), header = T, sep = ",")
          NOAAdata <- na.omit(NOAAdata, c(NOAAdata$tmax, NOAAdata$date))
          NOAAdata$julian <- NaN
          NOAAdata$year <- NaN
          
          temp <- as.POSIXlt(NOAAdata$date, "%Y-%m-%d", tz = "")
          NOAAdata$julian <- temp$yday
          NOAAdata$year <- format(as.Date(NOAAdata$date), format = "%Y")
          
          NOAAdata_filtered <- NOAAdata %>% filter(year>2000)
          RcrdCnt_val <- length(unique(NOAAdata_filtered[["year"]]))
          
          all_years_plot <- ggplot(NOAAdata_filtered, aes(x = NOAAdata_filtered$julian, y = NOAAdata_filtered$tmax), group = NOAAdata_filtered$date) + 
            geom_line(aes(colour = year)) + 
            #geom_smooth(method = "loess", se = FALSE, span = .6, colour = "red") + 
            theme_classic() + #scale_color_gradient(colours) + 
            labs(title = "Air Thermal Regime")
          
          smooth_vals <- predict(smooth.spline(NOAAdata_filtered$tmax ~ NOAAdata_filtered$julian))
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
          return(NA)
        }
      )
    }
  }
}

end_time <- Sys.time()

time <- end_time - start_time

print(time)
print("Script Complete")
