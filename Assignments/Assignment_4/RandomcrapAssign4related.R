#Question on the Assignment four for boxplot

#the plot way of doing this
plot(x=(as.factor(df2$Ecosystem)),y=df2$Lat, xlab = "Ecosystem", ylab = "Latitude")

#the ggplot way of doing it.
ggplot(df2, aes(y=Lat, x=Ecosystem))+geom_boxplot()


