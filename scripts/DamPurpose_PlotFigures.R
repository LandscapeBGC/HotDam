### PLot Figures via Dam Purpose
## Danielle Hare
## October 2019
### Advanced Stream Ecology - Hot Dam
## for later use <- https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r


# DP - Dam Purpose
## Library
library(plyr) # for rbind.fill
library(ggplot2)

##List files that may need to be read
DP_files <- list.files("input/DamPurposes_LocFiles", recursive= TRUE, full.names =  TRUE)
SW_dam_files <- list.files("input/SW_T_Dam", recursive= TRUE, full.names =  FALSE)


  
for (DP in DP_files) {
  df_loc <-  read.csv(DP, 
                      colClasses=c("locname"="character"))
  DF <- data.frame() #set up empty data to add data to 
  
  for (n in 1:nrow(df_loc)){
    
    SW_id <- as.character(df_loc$locname[n])
    SW_id_csv <- paste0(SW_id, '.csv')
    fp <- file.path('input/SW_T_Dam',SW_id_csv)
    
    #if file exists combine in dataframe
    if (file.exists(fp)){
      df <- read.csv(fp, 
                     colClasses=c("locname"="character"))
      
      #combine surface water data together for each dam purpose
      DF <- rbind.fill(DF,df)
    }
  }
  ### --- Output Dam Purpose Data Frame
  output_csv <- basename(DP)
  fp <-  file.path('input/DamPurposes_Dataframes',output_csv)
  write.csv(DF, file = fp )


  
  ## --- Plot Image for each type *RAW*
  
  #make iterative file name
  output_png <- paste0((sub("^([^.]*).*", "\\1", output_csv)),".png")
  fp <-  file.path('input/DamPurposes_Dataframes',output_png)
  
  # plot
  try(
  plot.DP <- ggplot(DF, aes(Date, Wtemp, colour = site_no, group = site_no)) +
    facet_wrap( ~ site_no, scales = "free")+
    geom_point(size = 0.5) +
    geom_smooth(span = 0.3)  
  )
  try(
    ggsave(fp)
  )
  

}



  
  
  
  
