## STEP 1 in Dams Thermal Regime Analysis 
## Danielle Hare
## October 2019

## This script differs from the other STEP1 by adding in the data retrival for USGS gauging stations (seperate download is not necessary)
## Only uses the station names provided

## Function Inputs
library(rnoaa)# noaa station search and download
library(dplyr)
library(tidyverse) # for extract geomtry to two columns
library(dataRetrieval)

#Set input parameters
DamDistanceMax <- 10 # Max km away a station is from a dam (raw straight-line)
f_out <- "DataOutput/NWISDamsLoc_NOAA_Match_.txt"
f_in <- "DataInput/GAGEII_NWIS_TempwDams.csv"
delim <- "," #  "\t",#
air_out <- "DataOutput/NOAA_pullR"
SW_out <- "DataInput/SWTemp_10km"
#Get a list of all the NOAA stations currently available
#G_st <- ghcnd_stations() #Use first time, or if need of an update - takes a long time. 
load("DataInput/NOAA_Stations_All.RData")#Already loaded all NOAA station data as G_st

# Read full SW location dataset
# Location data has to use column names Latitude and Longitude, or Northing and Easting if UTM
lat_lon_SW_df <- read.csv(f_in,
                          sep = delim, 
                          header=T,
                          stringsAsFactors=F,
                          colClasses=c("Sheet1_gag"="character"))# Makes sure ids that start with '0' are mantained

#change current, used column names to readable - hard code, need to change
colnames(lat_lon_SW_df)[colnames(lat_lon_SW_df)=="Sheet1_gag"] <- "id"
colnames(lat_lon_SW_df)[colnames(lat_lon_SW_df)=="Sheet1_g_4"] <- "latitude"
colnames(lat_lon_SW_df)[colnames(lat_lon_SW_df)=="Sheet1_g_5"] <- "longitude"
colnames(lat_lon_SW_df)[colnames(lat_lon_SW_df)=="Sheet1_g_6"] <- "state"
colnames(lat_lon_SW_df)[colnames(lat_lon_SW_df)=="RAW_DIS_NE"] <- "Dis_Crow_km"

#Pull data from NWIS, using any start date

lat_lon_SW_df$dataavailable <- NaN #Set column for SW Temperature records that are available for download

for (n in 1:nrow(lat_lon_SW_df)){
  ID <- lat_lon_SW_df$id[n]
  if ((nchar(as.character(ID)) >= 8) & lat_lon_SW_df$Dis_Crow_km[n] < DamDistanceMax ){ #only use usgs names (speeds up process!)
    try(
      station_df<- renameNWISColumns(readNWISdv(ID, "00010"))#, #Temperature (C)
                                              #startDate = date_se[1],
                                              #endDate = date_se[2]))
      )
      
# Create a new column in the SW location data table to indicate if there is data available for the time frame
# requested, if the whole list of SW is not run the remaining will be '0's as well. 
  
  if (nrow(station_df)> 0){
    lat_lon_SW_df$dataavailable[n] <-  1
    fn <- sprintf('%s.csv', ID)
    fp <- file.path(SW_out,fn)
    write.csv(station_df, file = fp)
  } else { #will this catch < 8 ID length
    lat_lon_SW_df$dataavailable[n] <-  0
  }
  }
}


# Filter SW station input data to only have the SW stations that meet the time requirements
lat_lon_SW_df <- filter(lat_lon_SW_df, dataavailable == 1)

# Determine the nearest NOAA station to each of the SW station in the list filter for only SW stations that meet the timeframe requirements
# Here we look for the NOAA station within 25 mi of station, no data returned for SW stations
# without any within that range. Limit = 1 only provide 1 per station (the closet one)
nearby_stations <- meteo_nearby_stations(lat_lon_df = lat_lon_SW_df, 
                              lat_colname = "latitude", # "dec_lat_va",#
                              lon_colname = "longitude", #" #"dec_long_va",
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
  fp <- file.path(air_out,fn)
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
colnames(lat_lon_SW_df)[colnames(lat_lon_SW_df)=="id"] <- "locname"

#Change Column names to match input for step 2, as well as provide appropriate colnames for join with SW location data
colnames(match_data)[names(match_data) == "id"] <- "NOAA_ID"
colnames(match_data)[names(match_data) == "name"] <- "NOAA_NAME"
colnames(match_data)[names(match_data) == "latitude"] <- "NEAR_Y"
colnames(match_data)[names(match_data) == "longitude"] <- "NEAR_X"
colnames(match_data)[names(match_data) == "distance"] <- "NEAR_DIST"

#remove geometry columns if converted from UTM
lat_lon_SW_df <- select(lat_lon_SW_df,locname, latitude, longitude)

# Merge the SW and Air Station data together (in the same format as the ArcGIS python script)
station_loc <- left_join(lat_lon_SW_df,match_data)

#Export this merge data set for next step (step 2 in python for annual signal)
write.csv(station_loc, file = f_out)

