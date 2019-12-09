#install.packages('ggpubr')
library(ggpubr)
library(here)
df <- read.csv('Thermal_Metrics_v2_PCA_cut.csv')

df[1:37] <- NULL
#df[18:25] <- NULL

df$Ref[df$Ref==0] <- 'Dam Site'
df$Ref[df$Ref==1] <- 'Reference Site'

#### Tmax ####


p1<- #ggplot(df, aes(x=df$NA_L1KEY, y=df$pstTmax, fill=df$Ref)) + 
  #geom_boxplot(color="red", alpha=0.2, outlier.shape = NA) + 
  #scale_fill_grey() + theme_classic() + coord_cartesian(ylim = c(5, 35)) +
  ggboxplot(df, x = "Clim_Reg", y = "pstTmax",
            color = "Ref", palette = "Set1",
            add = "jitter")+ 
  stat_compare_means(aes(group = df$Ref), 
                     label = "p.format")+
  ylim(0, 35) #use with discretion
p1
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