install.packages('ggplot')

library(here)
library(dplyr)
library(ggplot2)
library(broom)
library(stats)
library(tidyverse)
library(data.table)
library(stringr)
library(ggplot)

here("input", "SW_T_Dam") #removes confusion regarding setwd()

file <- dir(path = ".", pattern = "*.csv")
files <- as.list(file)


output <- here("input/Dam_thermal_regimes")
dir.create(output) 
input <- here("/input/SW_T_Dam")

####This loop works for reading and writing files###

for (i in 1:length(files)){
  station_num <- str_sub(files, end = -5) # extract station ids
  fn <- paste0(station_num[i], "_tr", ".csv") # create filename for clean station data
  fp <- file.path(output, fn) # create filepath for clean station data
  here("")
  tryCatch(
    expr = {
      USGSdata <- read.csv(file.path(input,files[i]), header = T, sep = ",")
      USGSdata <- na.omit(USGSdata, c(USGSdata$Wtemp, USGSdata$Date))
      USGSdata$julian <- NaN
      USGSdata$year <- NaN
      for (n in 1:nrow(USGSdata)){
        temp <- as.POSIXlt(USGSdata$Date, "%Y-%m-%d", tz = "")
        USGSdata$julian <- temp$yday
        USGSdata$year <- format(as.Date(USGSdata$Date), format = "%Y")

        all_years_plot <- ggplot(USGSdata, aes(x = USGSdata$julian, y = USGSdata$Wtemp), group = USGSdata$Date) +
          geom_line(aes(colour = year)) +
          #geom_smooth(method = "loess", se = FALSE, span = .6, colour = "red") +
          theme_classic() + #scale_color_gradient(colours) +
          labs(title = "Dam Thermal Regime")

        smooth_vals <- predict(smooth.spline(USGSdata$Wtemp ~ USGSdata$julian))
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

        ggsave(all_years_plot, file = file.path(output, paste0(station_num[i],"_All_years",".png")), width = 14 , height = 10, units = "cm")
        ggsave(spline_plot, file = file.path(output, paste0(station_num[i],"_spline",".png")), width = 14 , height = 10, units = "cm")
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
