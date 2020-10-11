library(tidyverse)
library(ggplot2)
library(ggimage)
library(dplyr)
library(grid)
library(png)

data("starwars")
names(starwars)

starwars$image <- "./jar.png"

starwars$image

str(starwars)

c <- ggplot(starwars,aes(x=birth_year,y=homeworld, image= image ))+
  geom_image()+
  labs(title = "Homeworld v Birth year", subtitle = "cool stuff from a galaxy far far away, in dis episode no one dies hurray! Dis is all about the amazing stories of jar jar, he goes on an adventure and lives....... the End ",
       x="yearBirth", y="homew", caption= "Data Organizers 101")+
  geom_smooth(method = "lm", color = "white")+
  theme(plot.title = element_text(size = 40, face="italic"), plot.subtitle = element_text(size = 6),
        panel.background = element_rect(fill = "gray"),
        plot.background = element_rect(fill="gray"))+
  coord_flip() 


ggsave(filename = "ugly_plot.png",c)
