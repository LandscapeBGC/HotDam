#install.packages('ggpubr')
library(ggpubr)
library(here)
library(ggplot2)
library(magrittr)
path1 <- here()

df <- read.csv('Thermal_Metrics_v2_PCA_cut.csv')

df[1:37] <- NULL #remove superfluous data
df[11:12] <- NULL #remove julian data timing
df[15:16] <- NULL # removes NOAA_timing
df[17:18] <- NULL # removes NOAA RcrdCnt and SplnCnt
#df[18:25] <- NULL

df$Ref[df$Ref==0] <- 'Dam Site'
df$Ref[df$Ref==1] <- 'Reference Site'

coln <- colnames(df[2:16])

for (metric in coln){
  ggboxplot(df, x = "Clim_Reg", y = metric,
            color = "Ref", palette = "Set1",
            add = "jitter") +
    stat_compare_means(aes(group = df$Ref),
                     label = "p.format", size = 2.5) +
    ggtitle(metric) +
    theme(axis.text.x = element_text(color = "grey20",
                                     size = 10, angle = 15,
                                     hjust = .5, vjust = .5,
                                     face = "plain"))

  fn <- paste0(metric, ".png")
  ggsave(fn, path = file.path(path1,'results/figures'), device = "png")

}