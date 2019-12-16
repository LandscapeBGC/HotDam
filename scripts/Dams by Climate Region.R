### Dam type by region
### by Eric Moore
### for the HotDam project

###libraries
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(data.table)
library(stats)

###Set up Main_df
setwd("~/HotDam3")
Main_df <- read.csv('Thermal_metrics_v2_PCA_cut.csv',
                    header = T,
                    stringsAsFactors = F)
Main_df_NWIS <- str_split(Main_df$STAID_edt, pattern = ",")
Main_df_NWIS <- str_split(str_sub(Main_df_NWIS, start = 6), pattern = ",")
Main_df_NWIS <- paste0(Main_df_NWIS, "_tr", ".csv")
Main_df_NWIS <- as.list(Main_df_NWIS)


###Set up dam termal regimes
setwd('~/HotDam3/input/Dam_thermal_regimes_post_v2')
usgs_files <- list.files(getwd(), pattern = ".csv")
usgs_file_list <- as.list(usgs_files)

#region_list <- list("Northwest", "Northwest", "Northwest", "NorthNorthwest", "Northwest", "Northwest", "Northwesteast", "NorthwestNorthwest", "Northwest")

#empty list for ecoregion
Northwest <- vector('list')
i <- 1

 

###for loop to get ecoregion and add corresponding dam thermal regimes .csvs to a list
for (h in 1:length(Main_df_NWIS)){
  if (Main_df_NWIS[h] %in% usgs_file_list == TRUE){
    usgs <- Main_df$STAID_edt[[h]]
    usgs_split <- str_split(str_sub(usgs, start = 6) , pattern = ",")
    usgs_csv <- paste0(usgs_split, "_tr", ".csv")
    Clim_Reg <- Main_df$Clim_Reg[[h]]
    if (grepl("Northwest", Clim_Reg) == T){
      #print(usgs_csv)
      Northwest[i] <- usgs_csv
      i <- i+1
    }
  }
}


#make large data frame for all dams within thermal regime and plot on same graph
df2 <- map_df(Northwest, ~read.csv(.), .id = "origin")
SouthNorthwest_plot <- df2 %>% ggplot() +
  aes(df2$x, df2$y) +
  geom_point() +
  geom_smooth() +
  labs(title = paste0("Northwest")) +
  theme(plot.title = element_text(size = 24)) +
  theme(plot.title = element_text(hjust = .5))+
  theme(axis.text = element_text(size = 20))
setwd("~/HotDam3/results/figures")
ggsave("Northwest_plot.png", device = "png")
write.csv(df2, file = "Northwest.csv")

