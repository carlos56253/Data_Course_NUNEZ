df2 <- read.table("../../Data/ITS_mapping.csv", header = T, sep ="\t" )
library(ggplot2)

summary(df2$Ecosystem)

summary(df2$Lat)

summary(df2$Run)

summary(df2)

str(df2)
#Typical way to plot it...
plot(x=(as.factor(df2$Ecosystem)),y=df2$Lat, xlab = "Ecosystem", ylab = "Latitude")

#Using ggplot
ggplot(df2, aes(y=Lat, x=Ecosystem))+geom_boxplot()

png(filename = "./silly_boxplot.png")
ggplot(df2, aes(y=Lat, x=Ecosystem))+geom_boxplot()
dev.off()




