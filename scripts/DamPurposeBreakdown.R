## Dam Purpose determination
## Danielle Hare
## October 2019
## UConn Advanced Stream Ecology Course


## --- Input Packages
library(dplyr)

##-- Read in full join table

df <- read.csv(list.files(pattern = "NWIS_NID_NOAA_master.csv", recursive = TRUE, full.names = TRUE),
                     header=T,
                     stringsAsFactors=F,
                     colClasses=c("locname"="character"))

df$PURPOSES_NAME <- "None"

abbv <- list("I","H","C","N","S","R","P","F","D","T","G","O")
name <- list("Irrigation", "Hydroelectric","Flood Control","Navigation","Water Supply","Recreation","Fire Protection","Fish and Wildlife Pond","Debris Control","Tailings","Grade Stabilzation","other")
purpose <- do.call(rbind, Map(data.frame, Purp_abbv=abbv, Purp_name=name))

for (i in purpose$Purp_abbv){
  
  j <- which(purpose$Purp_abbv == i, arr.ind=TRUE)
  
  for (n in 1:nrow(df)){
    if (grepl(i,df$PURPOSES[n])){
      df$PURPOSES_NAME[n] <- purpose$Purp_name[j]
    }
  }
  
  csv <- df %>% filter(df$PURPOSES_NAME==j)
  fn <- paste0(purpose$Purp_name[j],'.csv')
  fp <-  file.path('input/DamPurposes_LocFiles',fn)
  write.csv(csv, file = fp )
}


  