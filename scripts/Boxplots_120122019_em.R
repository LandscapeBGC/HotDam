#install.packages('ggpubr')
library(ggpubr)
library(here)

path1 <- here()

df <- read.csv('Thermal_Metrics_v2_PCA_cut.csv')

df[1:37] <- NULL #remove superfluous data
#df[11:12] <- NULL #remove julian data timing
#df[15:16] <- NULL # removes NOAA_timing
df[17:18] <- NULL # removes NOAA RcrdCnt and SplnCnt


df$Ref[df$Ref==0] <- 'Dam Site'
df$Ref[df$Ref==1] <- 'Reference Site'

#### Tmax ####


# p1<- #ggplot(df, aes(x=df$NA_L1KEY, y=df$pstTmax, fill=df$Ref)) + 
#   #geom_boxplot(color="red", alpha=0.2, outlier.shape = NA) + 
#   #scale_fill_grey() + theme_classic() + coord_cartesian(ylim = c(5, 35)) +
#   ggboxplot(df, x = "Clim_Reg", y = "pstTmax",
#             color = "Ref", palette = "Set1",
#             add = "jitter")+ 
#   stat_compare_means(aes(group = df$Ref), 
#                      label = "p.format")+
#   ylim(0, 35) #use with discretion
# p1

# names(df)[2] <- '90th Percentile of Mean Temperatures'
# names(df)[4] <- "50th Percentile of Mean Temperatures"
# names(df)[5] <- "10th Percentile of Mean Temperatures"
# names(df)[3] <- "Degree Days > 90th Percentile"
# names(df)[6] <- "Degree Days < 10th Percentile"
# names(df)[7] <- "Absolute Maximum Temperature"
# names(df)[8] <- "Absolute Minimum Temperature"
# names(df)[9] <- "Mean Temperature"
# names(df)[15] <- "Number of Cold Events < 25 Percentile"
# names(df)[16] <- "Number of warm Events > 75 Percentile"
# names(df)[10] <- "Julian Date of 1-day Maximum"
# names(df)[11] <- "Julidan Date of 1-day Minimum"
# names(df)[13] <- "Warming Rate"
# names(df)[12] <- "Cooling Rate"
# names(df)[19] <- "Absolute Maximum Phase Lag"
# names(df)[20] <- "Absolute Minimum Phase Lag"
# names(df)[14] <- "Coefficient of Variation of Mean Daily Temperatures"
names(df)[24] <- "Climate_Region"
names(df)[2] <- 'M4'
names(df)[4] <- "M5"
names(df)[5] <- "M6"
names(df)[3] <- "D1"
names(df)[6] <- "D2"
names(df)[7] <- "M1"
names(df)[8] <- "M2"
names(df)[9] <- "M3"
names(df)[15] <- "F1"
names(df)[16] <- "F2"
names(df)[10] <- "T1"
names(df)[11] <- "T2"
names(df)[13] <- "R2"
names(df)[12] <- "R1"
names(df)[19] <- "R4 max Temp"
names(df)[20] <- "R4 min Temp"
names(df)[14] <- "D3"


coln <- colnames(df[2:18])

for (metric in coln){
  ggboxplot(df, x = "Climate_Region", y = metric,
            color = "Ref", palette = "Set1",
            add = "jitter") + 
    
    stat_compare_means(mapping = aes(group = df$Ref), 
                       method = "wilcox.test",
                       label = "p.format",
                       size = 3) +
    
    ggtitle(metric) +
    
    theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
          axis.text = element_text(angle = 30, hjust = 1))
  
  fn <- paste0(metric, "_wilcoxon", ".png")
  ggsave(fn, path = file.path(path1,'results/figures'), device = "png")
  
}



wilcox.test(df$pstTmean ~ df$Ref, data = df)
t.test(df$pstTmean ~ df$Ref, data = df)


ggplot (df, aes(df$Ref, df$pstTmean)) +
  geom_boxplot()
#### Tmin ####

p2<- ggplot(df, aes(x=df$NA_L1KEY, y=df$pstTmin, fill=df$Ref)) + 
  geom_boxplot(color="red", alpha=0.2, outlier.shape = NA) + scale_fill_grey() + theme_classic() + 
  coord_cartesian(ylim = c(0, 15))


#### Tmean ####

p3<- ggplot(df, aes(x=df$NA_L1KEY, y=df$pstTmean, fill=df$Ref)) + 
  geom_boxplot(color="red", alpha=0.2, outlier.shape = NA) + scale_fill_grey() + theme_classic() + 
  coord_cartesian(ylim = c(5, 25))

#### pstP50 ####

p4<- ggplot(df, aes(x=df$NA_L1KEY, y=df$pstP50, fill=df$Ref)) + 
  geom_boxplot(color="red", alpha=0.2, outlier.shape = NA) + scale_fill_grey() + theme_classic() + 
  coord_cartesian(ylim = c(5, 25))

#### pstP90 ####

p5<- ggplot(df, aes(x=df$NA_L1KEY, y=df$pstP90, fill=df$Ref)) + 
  geom_boxplot(color="red", alpha=0.2, outlier.shape = NA) + scale_fill_grey() + theme_classic() + 
  coord_cartesian(ylim = c(5, 35))

#### pstP10 ####

p6<- ggplot(df, aes(x=df$NA_L1KEY, y=df$pstP10, fill=df$Ref)) + 
  geom_boxplot(color="red", alpha=0.2, outlier.shape = NA) + scale_fill_grey() + theme_classic() + 
  coord_cartesian(ylim = c(-5, 20))

#theme(axis.text.x = element_text(size = 6, angle = 60, hjust = 1))
#+ labs(fill = "")
Magnitude_Plot <- ggarrange(p1 + rremove('x.title') + rremove("x.text") + rremove("legend") +
            ggtitle("Thermal Maximum ") + xlab("") + ylab("") + labs(fill = ""), 
          p2 + rremove('x.title') + rremove("x.text") + rremove("legend") +
            ggtitle("Thermal Minimum") + xlab("") + ylab("") + labs(fill = ""), 
          p3 + rremove('x.title') + rremove("x.text") +
            ggtitle("Thermal Mean") + xlab("") + ylab("") + labs(fill = ""), 
          p4 + rremove('x.title') + rremove("x.text") +rremove("legend") +
            ggtitle("Thermal Median") + xlab("") + ylab("") + labs(fill = ""), 
          p5 + rremove('x.title') + rremove("x.text") +rremove("legend") +
            ggtitle("90th Percentile of Mean Temperatures") + xlab("") + ylab(""), 
          p6 + rremove('x.title') + rremove("x.text") + rremove("legend") +
            theme(axis.text.x = element_text(size = 5, angle = 10, hjust = 0.5)) +
            ggtitle("10th Percentile of Mean Temperatures")  + xlab("") + ylab("") + labs(fill = ""), 
          labels = c("A", "B", "C", "D", "E", "F"), ncol = 1, nrow = 6, common.legend = TRUE, legend="right")
annotate_figure(Magnitude_Plot,
                top = text_grob("Thermal Component: Magnitude", color = "black", face = "bold", size = 14),
                left = text_grob("Temperature (°C)", color = "black", rot = 90))