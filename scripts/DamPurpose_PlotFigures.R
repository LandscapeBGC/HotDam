### PLot Figures via Dam Purpose
## Danielle Hare
## October 2019
### Advanced Stream Ecology - Hot Dam

# DP - Dam Purpose
## Library
library(plyr) # for rbind.fill

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
}
  