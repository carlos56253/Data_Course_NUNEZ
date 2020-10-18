library(tidyverse)



replicates <- rep(1:3, each=60) 

ag_trt <- rep(c(rep(TRUE,3), rep(FALSE, 3)),30)

h2o_sat <- 
  rep(rep(c(10,20,30,40,50,60,70,80,90,100),each = 6),3)
plants <- rep(c("A", "B", "C"),60)

df <- data.frame(Replicate = replicates,
                 Agricultural_Treatment =ag_trt,
                 Water_Saturation = h2o_sat,
                 Plant_Species = plants,
                 Plant_Mass=NA
                 )
write.csv(df,"./Data/designed_experiment.csv",row.names = FALSE, quote = FALSE)


ggplot(df, aes(y=Plant_Mass, x=Water_Saturation, color=Plant_Species))+
  geom_point()+
  geom_smooth()
