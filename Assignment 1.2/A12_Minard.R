#installing the necessary packages
install.packages("HistData")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("ggrepel")

#Loading the necessary libraries
library(HistData)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(dplyr)
library(ggrepel)

#Fetching the required Minard data (troops, cities and temp) available in HistData package
troops <- Minard.troops
cities <- Minard.cities
#cities <- cities[complete.cases(cities), ]
temp <- Minard.temp
#temp<-temp[complete.cases(temp), ]

#Analyzing the structure of the dataset
#The troops data consists of five variables that describes the movement of the troop which include: Latitude, Longitude, Direction of the movement, and Group. 
str(troops)
#The cities data consists of three variables which describes the cities visited by the troops which include: Latitude, Longitude, and City
str(cities)
#The temp data consists of four variables which describes the temperature on different dates during the movement: Longitude, Temperature, Days, and Date
str(temp)


#Building a basic plot for troops
#Since we have to show the path for the movement of troops hence we are using geom_path() function of ggplot2 library.
#Here we took troops dataset and on x axis we are displaying longitude and latitude on y axis. Here, we use group = group to display separate paths used by different groups
ggplot(data = troops, aes(x = long, y = lat, group = group)) +
  geom_path()

#Now, we add more configuration in the above graph by adding color of the path based on the direction of the movement of troops and size of the path based on the number of survivors.
#There were some big gaps between the individual paths therefore use lineend="round" to round each end segment of the path.
ggplot(data = troops, aes(x = long, y = lat)) +
  geom_path(aes(color = direction, size = survivors, group = group),lineend = "round")

#Now, in the above graph we add few more configurations which include: 
#Here, we first increased the size of the scale as ggplot automatically changing Survivor variable to three discrete categories. Therefore, to show more different categories we adjusted the scale.
#We also change colour of the path to Yellow for advancing path and Black for return path as shown in the assignment. 
#We also provided label on x and y axis and also the title to the plot.
#We also hide the survivor variable from the legend by using guide() function and setting size variable to none
ggplot(data = troops, aes(x = long, y = lat)) +
  geom_path(aes(color = direction, size = survivors, group = group),lineend = "round") +
  scale_size(range = c(0.5, 15)) +
  scale_colour_manual(values = c("#DFC17E", "#252523")) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Minard’s visualization of Napolean’s Russian Campaign")+
  guides(size = "none")

#Now, we have to show the cities visited by the troops during their movement.
#In the above plot, we have added the points on the path using geom_point() and mentioned the name of the cities using geom_text() on the path plotted above.
ggplot(data = troops, aes(x = long, y = lat)) +
  geom_path(aes(color = direction, size = survivors, group = group),lineend = "round") +
  geom_point(data = cities, aes(x = long, y = lat),color = "#DC5B44") +
  geom_text(data = cities, aes(x = long, y = lat, label = city),
            vjust = 1.5,color = "#DC5B44") +
  scale_size(range = c(0.5, 15)) +
  scale_colour_manual(values = c("#DFC17E", "#252523")) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Minard’s visualization of Napolean’s Russian Campaign")+
  guides(size = "none")


#Finally, in order to improve the above plot, we added the number of survivors on the path 
#Here, we have used geom_text_repel() and set the position so that it will not overlap with the name of the cities.
plot_troops<- ggplot(data = troops, aes(x = long, y = lat)) +
  geom_path(aes(group = group,
                color = direction, 
                size = survivors),
            lineend = "round", linejoin = "round") +
  geom_text(data = cities, aes(label = city), size = 2.5, 
            position = position_nudge(y=0.1,x=-0.1),
            color = "#DC5B44")+
  geom_point(data = cities, aes(x = long, y = lat),
             color = "#DC5B44") +
  geom_text_repel(data = troops, aes(label = survivors), 
                  size = 2.5, 
                  position = position_nudge(x=-0.1,y=-0.1),
                  color = "#DC5B44")+
  scale_size(range = c(0.5, 15)) +
  scale_colour_manual(values = c("#DFC17E", "#252523")) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Minard’s visualization of Napolean’s Russian Campaign")+
  guides(size = "none")

plot_troops

#We then set the theme for the plot where we took a simple black and white theme using theme_bw() function
#We also removed the some major and minor line from the background and also removed the border
#We also made x and y axis line to black colour
plot_troops <- plot_troops + theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) + 
  theme(axis.line = element_line(color = 'black'))

#Now, we plotted the second part of the Minard plot which include the temperature on different dates during the movement
#We first plot a simple graph using temperature data showing longitude on x axis and temperature on y axis
#Here, we also used geom_path() to show temperature during the movement
ggplot(data = temp, aes(x = long, y = temp)) + 
  geom_path(color="grey", size=1.5) +
  geom_point(size=2) 

#Now, to show the temperature on different days we added a new column temp_on_date which is a combination of two different columns temp and date. We used mutate() function of dplyr library to create a this column and we used paste0() function to combine these two columns without any space.
temp <- temp %>%
  mutate(temp_on_date = paste0(temp, "° ", date))
head(temp)

#To show the above new column on the plot we used geom_text_repel() where we set the label as temp_on_date
#We also set cartesian cordinates on x axis which is longitude from 24 to 38 to keep it same as troops plot.
plot_temp <- ggplot(data = temp, aes(x = long, y = temp)) +
  geom_path(color="grey", size=1.5) +
  geom_point(size=2) +
  geom_text_repel(aes(label=temp_on_date), size=2.5) +
  coord_cartesian(xlim = c(24, 38))+
  xlab("Longitude") + ylab("Temperature")

plot_temp

#We then set the theme for the plot where we took a simple black and white theme using theme_bw() function
#We also removed the some major and minor line from the background and also removed the border
#We also made x and y axis line to black colour
plot_temp <- plot_temp + theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) + 
  theme(axis.line = element_line(color = 'black'))

#To show both the plots together as one we used grid.arrange() function we passed both the plots.
#Since the troops movement plot is the major plot so we adjust the heights of both plots where we give height of 4 to troops movenment plot and 2 to temperature plot.
grid.arrange(plot_troops + guides(color="none"), 
             plot_temp, heights=c(4,2) )

