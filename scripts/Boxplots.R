install.packages('ggpubr')
library(ggpubr)
library(here)
df <- read.csv('Thermal_Metrics_11262019.csv')

df[1:47] <- NULL
df[8:12] <- NULL


df$Ref[df$Ref==0] <- 'Dam Site'
df$Ref[df$Ref==1] <- 'Reference Site'

# function for number of observations 
# give.n <- function(x){
#   return(c(y = median(x)*2, label = length(x))) }


#### Tmax ####

p1<- ggplot(df, aes(x=df$NA_L1KEY, y=df$pstTmax, fill=df$Ref)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_grey() + theme_classic() + 
  coord_cartesian(ylim = c(5, 35))
p1 +  theme(axis.text.x = element_text(size = 6, angle = 60, hjust = 1)) + 
  ggtitle("Thermal Maximum ") +
  xlab("Ecoregion") + ylab("Absolute Thermal Maximum (C°)") + labs(fill = "") 

#### Tmin ####

p2<- ggplot(df, aes(x=df$NA_L1KEY, y=df$pstTmin, fill=df$Ref)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_grey() + theme_classic() + 
  coord_cartesian(ylim = c(0, 18))
p2 +  theme(axis.text.x = element_text(size = 6, angle = 60, hjust = 1)) + 
  ggtitle("Thermal Minimum ") +
  xlab("Ecoregion") + ylab("Absolute Thermal Minimum (C°)") + labs(fill = "") 


#### Tmean ####

p3<- ggplot(df, aes(x=df$NA_L1KEY, y=df$pstTmean, fill=df$Ref)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_grey() + theme_classic() + 
  coord_cartesian(ylim = c(0, 30))
p3 +  theme(axis.text.x = element_text(size = 6, angle = 60, hjust = 1)) + 
  ggtitle("Thermal Mean ") +
  xlab("Ecoregion") + ylab("Absolute Thermal Mean (C°)") + labs(fill = "") 

#### Lag_Tmax ####

p4<- ggplot(df, aes(x=df$NA_L1KEY, y=df$Lag_TmaxT, fill=df$Ref)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_grey() + theme_classic() + 
  coord_cartesian(ylim = c(0, 120))
p4 +  theme(axis.text.x = element_text(size = 6, angle = 60, hjust = 1)) + 
  ggtitle("Phase Lag (Thermal Maximum)") +
  xlab("Ecoregion") + ylab("Phase Lag (Difference in Julian Date)") + labs(fill = "") 

#### Lag_Tmin ####

p5<- ggplot(df, aes(x=df$NA_L1KEY, y=df$Lag_TminT, fill=df$Ref)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_grey() + theme_classic() + 
  coord_cartesian(ylim = c(0, 180))
p5 +  theme(axis.text.x = element_text(size = 6, angle = 60, hjust = 1)) + 
  ggtitle("Phase Lag (Thermal Mimimum) ") +
  xlab("Ecoregion") + ylab("Phase Lag (Difference in Julian Date)") + labs(fill = "") 

ggarrange(p1, p2, p3, p4, p5 + rremove("x.text"), 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)
