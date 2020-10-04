library(tidyverse)
library(ggplot2)
library(patchwork)
#Load dataset #1
data("mtcars")
str(mtcars)
?mtcars

#subset mtcars #2
auto_transmissions <- mtcars$am %in% 0 
Asubset <- mtcars[auto_transmissions,]

Asubset
#save dataset as csv file #3
?write.csv
write.csv(Asubset,"automatic_mtcars.csv")

#plot effect of hp on mpg #4
#will he want color as mpg or cyl?
ggplot(Asubset, aes(x = hp, y =mpg ))+ 
  geom_point(aes(color= cyl)) +
  labs(title = "Miles per Gallon vs Horsepower",subtitle = "Effect of Horsepower on Miles per Gallon",
       x ="Horsepower", y= "Miles per Gallon")+
  geom_smooth(method = "lm", se= F, color = "firebrick")+
  theme(plot.title = element_text(size=27, face = "italic"))+
  theme_minimal()

#save it as an object 
p <- ggplot(Asubset, aes(x = hp, y =mpg ))+ 
  geom_point(aes(color= mpg)) +
  labs(title = "Miles per Gallon vs Horsepower",subtitle = "Effect of Horsepower on Miles per Gallon",
       x ="Horsepower", y= "Miles per Gallon")+
  geom_smooth(method = "lm", se= F, color = "firebrick")+
  theme(plot.title = element_text(size=27, face = "italic"))+
  theme_minimal()
#create png file #5
ggsave(filename = "mpg_vs_hp_atuo.png",p)
  

#plot effect of weight on mpg #6
  ggplot(Asubset, aes(x=wt, y=mpg))+ 
    geom_point(aes(color=mpg))+
    geom_smooth(method = "lm", se=F, color = "firebrick")+
    labs(title = "Miles per Gallon vs Weight",subtitle = "Effect of Weight on Miles per Gallon",
         x= "Weight", y="Miles per Gallon")
  
  #save as object
  c <-   ggplot(Asubset, aes(x=wt, y=mpg))+ 
    geom_point(aes(color=mpg))+
    geom_smooth(method = "lm", se=F, color = "firebrick")+
    labs(title = "Miles per Gallon vs Weight",subtitle = "Effect of Weight on Miles per Gallon",
         x= "Weight", y="Miles per Gallon")
  #save as tiff image #7
  ggsave(filename = "mpg_vs_wt_audio.tiff",c)
    
  #Subset the original data set #8 
  
  auto_displacement <- mtcars$disp >=200   
  Bsubset <- mtcars[auto_displacement,]

  #make csv file from subset #9
  
  write.csv(Bsubset, "mtcars_max200_displ.csv")

  #include max horsepower from all 3 data sets #10
  mtcars_origin <- max(mtcars$hp)
  mtcars_auto <- max(Asubset$hp)
  mtcars_max200 <- max(Bsubset$hp)

  #11 prints and saves as txt file
  print(c(mtcars_origin, mtcars_auto,mtcars_max200))
#txt file
  txt <- 
  " 
  #calculation for max hp of mtcars 
    mtcars_origin <- max(mtcars$hp)
  
  #Subset for automatic transmissions

    auto_transmissions <- mtcars$am %in% 0
  Asubset <- mtcars[auto_transmissions,]
  
  #calculation for max hp of automatic transmission subset
    mtcars_auto <- max(Asubset$hp)
  
  #Subset for displacement >= 200

    auto_displacement <- mtcars$disp >=200   
  Bsubset <- mtcars[auto_displacement,]
  
  #calculation for max hp of Displacement data 
    mtcars_max200 <- max(Bsubset$hp)
  
  #Print command for the three values obtained
    print(c(mtcars_origin, mtcars_auto,mtcars_max200))"
  
  writeLines(txt, "./hp_maximums.txt")
  
  #12 
  #effect of weight on mpg
  q1 <- ggplot(mtcars, aes(x=wt, y=mpg))+ 
    geom_point(aes(color=as.factor(cyl)))+
    geom_smooth(method = "lm", se=F, color = "firebrick")+
    labs(title = "Miles per Gallon vs Weight", subtitle = "Effect of Weight on Miles per Gallon",
         x= "Weight", y="Miles per Gallon",color = "Cylinders")+
    theme_minimal()
  
  # distributions of mpg for cars, separated/colored by number of cyl
  q2 <- ggplot(mtcars,aes(x=cyl, y=mpg))+
    geom_violin(aes(color = as.factor(cyl)))+
    labs(title = "Miles per Gallon vs Cylinders", 
         subtitle = "Distribution of MPG with values separated by number Cylinders",
         x = "Number of Cylinders", y="Miles per Gallon", color="Cylinders")+
    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(face = "italic"))+
    theme_minimal()
  
  #effect of hp on mpg
  q3 <- ggplot(mtcars, aes(x = hp, y =mpg ))+ 
    geom_point(aes(color=as.factor(cyl))) +
    labs(title = "Miles per Gallon vs Horsepower",subtitle = "Effect of Horsepower on Miles per Gallon", 
         caption = "Automotives Data", x= "Horsepower", y="Miles Per Gallon", color = "Cylinders")+
    geom_smooth(method = "lm", se= F, color= "firebrick")+
    theme(plot.title = element_text(size=27, face = "italic"))+
    theme_minimal()
#13 combine all 3 plots into one image
  q1+q2+q3  
#save as a single png image
  
  png(filename = "./combined_mtcars_plot.png",1500,500)
  q1+q2+q3 
  dev.off()
  
  