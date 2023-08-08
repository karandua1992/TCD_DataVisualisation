setwd('C:/Users/surya/Desktop/Karan/Sem1/Data Visualisation/Assignment 3')
getwd()
#installing the necessary packages
install.packages("plotly")
install.packages("ggplot2")
install.packages("dplyr")

#Loading the necessary libraries
library(plotly)
library(ggplot2)
library(dplyr)

#Read the data from csv and check the structure of the dataset
gapminder_data <- read.csv("gapminder.csv")
str(gapminder_data)
colnames(gapminder_data)[1] <- 'country'



#----Part-1: Visualize the evolution of life expectancy over the years----

#As per the assignment, we need to Encode year by x-position, and life-expectancy by y-position
ggplot(data = gapminder_data, aes(x = year, y =  lifeExp)) +
  geom_point()

#Part-1: Variant-1:- For this variant, I have used color as a channel.
#I have used geom_jitter() function to add adds a small amount of random variation of each point to make plot clearly visible to the reader.
ggplot(data = gapminder_data, aes(x = year, y =  lifeExp)) +
  geom_jitter(aes(color = continent)) +
  xlab("Year") + ylab("Life Expectancy") 

#Part-1: Variant-2:- For this variant, I have used shape as a channel.
ggplot(data = gapminder_data, aes(x = year, y =  lifeExp)) +
  geom_jitter(aes(shape = continent), color = "red") +
  xlab("Year") + ylab("Life Expectancy") 

#Part-1: Variant-3:- For this variant, I have used brightness as a channel.
ggplot(data = gapminder_data, aes(x = year, y =  lifeExp)) +
  geom_jitter(aes(alpha = continent), color = "dark blue") +
  xlab("Year") + ylab("Life Expectancy") 





#----Part-2: Visualize the correlation between wealth and health----

#As per the assignment, we need to Encode gdp by x-position and life expectancy by y-position
ggplot(data = gapminder_data, aes(x = gdpPercap, y =  lifeExp)) +
  geom_point()

#Part-2: Variant-1:- For this variant, I have used size as a channel.
ggplot(data = gapminder_data, 
       aes(x = gdpPercap, y =  lifeExp, 
           size = pop)) +
  geom_point(alpha=0.5, shape=21,color = "dark blue") +
  xlab("GDP per Capita") + ylab("Life Expectancy")
  

#Part-2: Variant-2:- For this variant, I have used color gradient as a channel.
ggplot(data = gapminder_data, 
       aes(x = gdpPercap, y =  lifeExp)) +
  geom_point(aes(color = pop),alpha=0.5) +
  xlab("GDP per Capita") + ylab("Life Expectancy") +
  scale_colour_gradient(low = "green", high = "red")


#Part-2: Variant-3:- For this variant, I have used brightness as a channel.
ggplot(data = gapminder_data, 
       aes(x = gdpPercap, y =  lifeExp)) +
  geom_point(aes(color = pop)) +
  xlab("GDP per Capita") + ylab("Life Expectancy") +
  scale_colour_gradient(low = "white", high = 'black')


#----Part-3:  free form----
#As per the assignment, we have to Create a single visualization that somehow, encodes all 6 attributes in one chart
#For this, I have used plotly library to highlight all the 6 attributes
#Here, I have used gdpPercap on x axis, lifeExp on y axis, color the points using continent, add an extra frame based on years, size of the points based on population and provided country name in the id

plot <- ggplot(gapminder_data, 
               aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent, 
                 frame = year, 
                 size = pop, 
                 ids = country)) +
  xlab("GDP per Capita") + ylab("Life Expectancy") +
  scale_x_log10()

#Here, I used highlight() function to highlight the attributes based on the results
ggplotly(plot) %>% 
  highlight()
