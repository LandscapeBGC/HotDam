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
library(cowplot)
library(epitools)

setwd('~/HotDam3/results/figures')

files <- list.files(pattern = ".csv")

df <- map_df(files, ~read.csv(.), .id = "origin")
names(df)[1] <- "group"
df_group <- df %>% group_by(group)
names(df_group)[4] <- "Julian_date"
names(df_group)[7] <- "Temperature"

region_names <- c( '1' = "Midwest",
                   '2' = "Northeast",
                   '3' = "Northern Rockies",
                   '4' = "Northwest",
                   '5' = "Ohio Valley",
                   '6' = "South",
                   '7' = "Southeast",
                   '8' = "Southwest",
                   '9' = "West")


plot <- df_group %>% ggplot() +
  aes(df_group$Julian_date, df_group$Temperature)+
  geom_point()+
  geom_smooth()+
  #geom_line( aes(x = df$X.y, y = df$y.y), color = "green")
  facet_wrap(~ df_group$group, scales = "fixed", labeller = as_labeller(region_names)) +
  theme(strip.text.x = element_text(size = 15))+
  #theme(strip.text = element_text(size = 5)) +
  labs(title = paste0("Annual Thermal Regimes Across Climate Regions"), size = 60,
       x = paste0("Julian Day"), size = 15,
       y = paste0("Temperature"), size = 15) +
  theme(plot.title = element_text(hjust = .5))+
  theme_cowplot() +
  #annotate("text", label = "blah")




setwd("~/HotDam3/results/figures")
ggsave("Annual Thermal Regimes Across Climate Regions.png", device = "png", limitsize = F)

