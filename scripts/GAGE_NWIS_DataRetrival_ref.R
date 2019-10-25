## Hot Dam - Download Data
## Danielle Hare
## October 2019

##--- Pull Data (that could be) Relevant for Dams and Comparison to Reference Sites


##--- Library 
library(dataRetrieval) #Retreive NWIS datasetset
library(dplyr)#for filter
library(rnoaa)# Retreive NOAA data

# --- Input Parameters

file_list <- c("conterm_bas_classif_.txt","conterm_hydro.txt","conterm_hydromod_dams.txt")#list of 'extra' GAGEII datasets of interest
dam_dis_max <- 10 #max dam distance km
SW_out_dam <- "input/SW_T_Dam"
SW_out_ref <- "input/SW_T_Ref"
f_out <- "input/GAGE_NWIS_NOAA_LocData.csv"

##---- Read location files 

#initate dataframe with location information
GAGE_loc <- read.csv(list.files(pattern = "conterm_basinid.txt", recursive = TRUE, full.names = TRUE),
                     header=T,
                     stringsAsFactors=F,
                     colClasses=c("STAID"="character"))

for (f in file_list){
  df <- read.csv(list.files(pattern = f, recursive = TRUE, full.names = TRUE),
                     header=T,
                     stringsAsFactors=F,
                    colClasses=c("STAID"="character"))
  GAGE_loc <- merge(GAGE_loc, df, on = 'STAID')
}
rm(df)
# Column name "id" necessary to run NOAA part
GAGE_loc$id <- GAGE_loc$STAID


### --- Perform Relevant Dam Download
GAGE_loc$NWIS_Tavail <- NaN #Set column for SW Temperature records that are available for download

#make two relevant folders Dam within raw distance and ref locations
GAGE_Dam <- filter(GAGE_loc, (GAGE_loc$RAW_DIS_NEAREST_DAM < dam_dis_max & GAGE_loc$RAW_DIS_NEAREST_DAM >=0))
GAGE_Ref <- filter(GAGE_loc, GAGE_loc$CLASS == "Ref")

for (n in 1:nrow(GAGE_Ref)){
  ID <- GAGE_Ref$id[n]
  #ID <- "01463500" trial run
  temp_df<- renameNWISColumns(readNWISdv(ID, "00010"))#, #Temperature (C)
  df <- temp_df
  if (nrow(df)>(0.75*365)){# at least has 75% of one year worth of temp data
    GAGE_Ref$NWIS_Tavail[n] <-  1
    
    #--- also download discharge data if available
    try({
      discharge_df<- renameNWISColumns(readNWISdv(ID, "00060"))#, #Daily Discharge cubic feet per second
      #startDate = date_se[1],
      #endDate = date_se[2]))
      df <- merge(temp_df, discharge_df, on = "Date", how = 'left')
    }
    )
    
    fn <- sprintf('%s.csv', ID)
    fp <- file.path(SW_out_ref,fn)
    write.csv(df, file = fp)
    
    } else { #not enough data or no data available
      GAGE_Ref$NWIS_Tavail[n] <-  0
    } 
  
}

# --- Only keep rows with Temp Data
GAGE_Ref <- filter(GAGE_Ref, NWIS_Tavail== 1)
GAGE_Ref$id <- GAGE_Ref$locname

## Get NOAA DATA
#Get a list of all the NOAA stations currently available
#G_st <- ghcnd_stations() #Use first time, or if need of an update - takes a long time. 
load("input/NOAA_Stations_All.RData")#Already loaded all NOAA station data as G_st

# Determine the nearest NOAA station to each of the SW station in the list filter for only SW stations that meet the timeframe requirements
# Here we look for the NOAA station within 25 mi of station, no data returned for SW stations
# without any within that range. Limit = 1 only provide 1 per station (the closet one)
nearby_stations <- meteo_nearby_stations(lat_lon_df = GAGE_Ref, 
                                         lat_colname = "LAT_GAGE", # "dec_lat_va",#
                                         lon_colname = "LNG_GAGE", #" #"dec_long_va",
                                         station_data = G_st,
                                         #year_min = 2011,#This way we wont get air temp records with just 2010, but include 2010 in data pull- see below 
                                         #year_max = 2018,
                                         var = c("TMAX", "TMIN", "TAVG"),
                                         radius = 25,
                                         limit = 1)

# Merge the nearby station data to one dataframe
match_data = do.call(rbind, nearby_stations) # make output of meteo nearby stations to a usable df

# Make a list of the unique NOAA station ids (remove duplicates)
noaa_pull <- unique(match_data$id)
# remove NA values (if some stations do not have lat long this will occur)
noaa_pull <- noaa_pull[!is.na(noaa_pull)]

#put in question to pull air data
# Pull the data for each of the NOAA stations identified as closest stations to the input SW stations
for (i in noaa_pull) {
  fn <- sprintf('%s.csv', i)
  fp <- file.path('input/NOAA_pullR',fn)
  # If the NOAA station is not already downloaded, download it. This is so i dont have to manipulate input files for speed
  if(!file.exists(fp)){
    # If there is an error the script will continue to run
    tryCatch({
      # Pull the station temperature data
      df <- meteo_pull_monitors(monitors = i, 
                                var = c("TAVG", "TMIN", "TMAX"))
      rec <- colnames(df[3:length(df)])
      
      for (n in rec){
        df_temp <- df[n]/10
        df[n] <- df_temp
      }
      
      write.csv(df, file = fp)
    }, 
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

#change index/rownames to be a active column - locname is consistent for next step (2)
match_data$locname <- row.names(match_data)
#change original input siteno/id to locname for match (needed to be id for last package to work)
colnames(GAGE_Ref)[colnames(GAGE_Ref)=="id"] <- "locname"

#Change Column names to match input for step 2, as well as provide appropriate colnames for join with SW location data
colnames(match_data)[names(match_data) == "id"] <- "NOAA_ID"
colnames(match_data)[names(match_data) == "name"] <- "NOAA_NAME"
colnames(match_data)[names(match_data) == "latitude"] <- "NEAR_Y"
colnames(match_data)[names(match_data) == "longitude"] <- "NEAR_X"
colnames(match_data)[names(match_data) == "distance"] <- "NEAR_DIST"

# Merge the SW and Air Station data together (in the same format as the ArcGIS python script)
GAGE_Ref <- GAGE_Ref[, !duplicated(colnames(GAGE_Ref))]#remove duplicate column names locname
station_loc <- left_join(GAGE_Ref,match_data, by = "locname")

#Export this merge data set for next step (step 2 in python for annual signal)
fn <- sprintf('GAGE_NOAA_NWIS_Ref.csv', ID)
fp <- file.path(SW_out_ref,fn)
write.csv(df, file = fp)
write.csv(station_loc, file = f_out)



