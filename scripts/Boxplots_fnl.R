#install.packages('ggpubr')
library(ggpubr)
library(here)
library(ggplot2)
library(magrittr)
path1 <- here()

df <- read.csv('Thermal_Metrics_v2_PCA_cut.csv')

df[1:37] <- NULL #remove superfluous data
df[17:18] <- NULL # removes NOAA_timing
df[19:20] <- NULL # removes NOAA RcrdCnt and SplnCnt

df$Ref[df$Ref==0] <- 'Dam Site'
df$Ref[df$Ref==1] <- 'Reference Site'

x=1
for (min_timing in df$pstTminT){
  if (min_timing > 188){
    df$pstTminT[x] <- min_timing - 366
  }
  x=x+1
}

coln <- colnames(df[2:18])

for (metric in coln){
  ggboxplot(df, x = "Clim_Reg", y = metric,
            color = "Ref",
            add = "jitter") +
    scale_color_grey(start=0.2, end=0.8) + theme_classic() +
    stat_compare_means(aes(group = df$Ref, 
                           label = paste0(..method.., "\n", "p=", ..p.format.., "\n", ..p.signif..)),
                       size = 2.5) +
    ggtitle(metric) +
    xlab("" )+ylab("")+
    theme(axis.text.x = element_text(color = "grey20",
                                     size = 10, angle = 15,
                                     hjust = .5, vjust = .5,
                                     face = "plain"), 
          axis.title.x = element_text(angle = 15))

  fn <- paste0(metric, ".png")
  ggsave(fn, path = file.path(path1,'results/figures'), device = "png")
}