#install.packages("sf")
#install.packages("rvest")
#install.packages("ggplot2")
#install.packages("patchwork")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("shiny")
#install.packages("plotly")
#install.packages("flexdashboard")
#install.packages("shinythemes")

#Loading the necessary libraries.
library(shinythemes)
library(shiny)
library(plotly)
library(sf)
library(ggplot2)
library(patchwork)
library(scales)
library(plyr)
library(dplyr)
library(colorspace)

getwd()
setwd(getwd())

#Reading the shape file "COVID-19_HPSC_County_Statistics_Historic_Data.shp" which store geospatial data using st_read() function of sf library. We set quiet = TRUE in order to suppress the output.
data <- "COVID-19_HPSC_County_Statistics_Historic_Data.shp"
Counties_IRL <- st_read(data, quiet = TRUE)

#Data normalization. For this, I first divide the ConfirmedC column with Population column and then multiply the total by
#100,000. I then rounded the final value to 0 decimal places to give a meaningful value as the number of cases
#can not be in decimal numbers. I finally stored these values in a new column ConfirmedC_Norm by using
#mutate() function of dplyr library. I created this column because I have to use this in multiple sections of
#the assignment.
Counties_IRL <- Counties_IRL %>%
  mutate(ConfirmedC_Norm = round(ConfirmedC/Population * 100000,0))


#====================== Choropleth Map of Ireland ==================================

#A choropleth map is a colored map that is used to display the divided geographical areas or regions with
#color of the region related to a numeric variable. A light color is generally used to represent smaller values
#and a dark color is used to represent larger values

#Get total confirmed cases as of 31/12/2020
Cases_31Dec2020 <- Counties_IRL %>% filter(TimeStamp == "2020-12-31")

#To plot a Choropleth visualization we need to discretized the continuous data into intervals to use them to map with a set of discrete colors to show on a custom legend. 
#For the same, we need to identify the minimum and the maximum value of the intervals.
#We use min() and max() function to identify the minimum and maximum value of cumulative number of cases on 2020-12-31. I then used round_any() function of the plyr library to round it to the multiple of the next lower integer
scale_minimum20<-round_any(min(Cases_31Dec2020$ConfirmedC_Norm), 1000, f = floor)
scale_maximum20<- round_any(max(Cases_31Dec2020$ConfirmedC_Norm), 1000, f = ceiling)

#In order to make a continuous scale of sequence of numbers, I subtract 10000 from scale_minimum20 this is the minimum value of the scale which is 0 and add 1000 to scale_maximum20 this is the maximum value of the scale which is
#20000. I set the gap value of the scale as 2000.
breaks20<-seq(scale_minimum20-0,scale_maximum20+1000, by =2000)

#Then I used an in-built R function cut() to divide the above range of continuous values from 0-20000 with a gap of 2000 into intervals of same size and codes the value of ConfirmedC_Norm according to which interval it fall 
Cases_31Dec2020$Total_Confirmed_Cases_Per_100K_2020 <- cut(Cases_31Dec2020$ConfirmedC_Norm,breaks = breaks20,dig.lab = 5)


Cases_31Dec2021 <- Counties_IRL %>% filter(TimeStamp == "2021-12-31")
scale_minimum21<-round_any(min(Cases_31Dec2021$ConfirmedC_Norm), 1000, f = floor)
scale_maximum21<- round_any(max(Cases_31Dec2021$ConfirmedC_Norm), 1000, f = ceiling)
breaks21<-seq(scale_minimum21-10000,scale_maximum21+1000, by =2000)
Cases_31Dec2021$Total_Confirmed_Cases_Per_100K_2021 <- cut(Cases_31Dec2021$ConfirmedC_Norm,breaks = breaks21,dig.lab = 5)

#I used nlevels() function to check the number of levels of the above intervals. I need this value as I have to make a palette of that much number of colours so that each interval range have a separate colour to easily distinguished from others
nlevels21<- nlevels(Cases_31Dec2021$Total_Confirmed_Cases_Per_100K_2021)

Cases_12Dec2022 <- Counties_IRL %>% filter(TimeStamp == "2022-12-12")

Cases_12Dec2022 <- Cases_12Dec2022 %>% 
  mutate(MeanDiff = ConfirmedC_Norm - mean(ConfirmedC_Norm))

scale_minimum22<-round_any(min(Cases_12Dec2022$ConfirmedC_Norm), 1000, f = floor)
scale_maximum22<- round_any(max(Cases_12Dec2022$ConfirmedC_Norm), 1000, f = ceiling)
breaks22<-seq(scale_minimum22-0,scale_maximum22+1000, by =2000)
Cases_12Dec2022$Total_Confirmed_Cases_Per_100K_2022 <- cut(Cases_12Dec2022$ConfirmedC_Norm,breaks = breaks22,dig.lab = 5)

#To make a palette, I used hcl.colors() functions of colorspace library as this function provides a basic and lean implementation of the pre-specified palettes.
#I choose Plasma palette as the colors of this palette are easily distinguishable.
#I also set rev=TRUE to facilitate reversing the order of colors as this function returns dark colors followed by light colors but in my case I want light colors first to represent smaller interval range. 
pal21 <- hcl.colors(nlevels21, "Plasma", rev = TRUE)

#I also desaturate the colors to an amount of 0.2 by using desaturate() function to reduce the chroma value of the colors to make them less bright.
pal_desat21<-desaturate(pal21,amount = 0.2)
labs21 <- breaks21/1000
labs_plot21 <- paste0("(", labs21[1:nlevels21], "k-", labs21[1:nlevels21+1], "k]")

# Generating choropleth plot for date 2020-12-31
# I used ggplot() function along with geom_sf() to plot the choropleth. geom_sf() is an unusual geom used to draw different geometric objects based on the simple features present in the data. 
#In the aesthetics of this function I first set the color for each county based on the interval range we calculated above and stored in column ConfirmedC_Norm. 
#I then set each county boundary color to darkgrey with linetype = 1 which specifies a solid line and width of the line as 0.4
#In order to fill the colors for each county I used scale_fill_manual() function in which I specified the values as the palette I created above (pal21)
#I set drop = FALSE, to use all the levels, I set darkgrey color in case if there is any NA value and set the labels as the labels I created above (labs_plot21).

CP_20 <- ggplot(Cases_31Dec2020) + geom_sf(aes(fill = Total_Confirmed_Cases_Per_100K_2020, text = CountyName),color = "darkgrey",
                                           linetype = 1,lwd = 0.4) +
  labs(title = "Number of cases per 100K population as of 31 Dec 2020") +
  scale_fill_manual(values = pal_desat21, drop = FALSE, na.value = "grey80",
                    label = labs_plot21
  ) +
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size=10), 
        legend.key.height = grid::unit(1, "cm"),
        plot.caption = element_text(size = 7, face = "italic"),
        legend.position = "right",
        legend.direction = "vertical",
        axis.line=element_blank())

# Generating choropleth plot for date 2021-12-31
CP_21 <- ggplot(Cases_31Dec2021) + geom_sf(aes(fill = Total_Confirmed_Cases_Per_100K_2021, text = CountyName),color = "darkgrey",
                                           linetype = 1,lwd = 0.4) +
  labs(title = "Number of cases per 100K population as of 31 Dec 2021") +
  scale_fill_manual(values = pal_desat21, drop = FALSE, na.value = "grey80",label = labs_plot21) +
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size=10), 
        legend.key.height = grid::unit(1, "cm"),
        plot.caption = element_text(size = 7, face = "italic"),
        legend.position = "right",
        legend.direction = "vertical",
        axis.line=element_blank())

# Generating choropleth plot for date 2022-12-12
CP_22 <- ggplot(Cases_12Dec2022) + geom_sf(aes(fill = Total_Confirmed_Cases_Per_100K_2022, text = CountyName),color = "darkgrey",
                                           linetype = 1,lwd = 0.4) +
  labs(title = "Number of cases per 100K population as of 12 Dec 2022") +
  scale_fill_manual(values = pal_desat21, drop = FALSE, na.value = "grey80",
                    label = labs_plot21
  ) +
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size=10), 
        legend.key.height = grid::unit(1, "cm"),
        plot.caption = element_text(size = 7, face = "italic"),
        legend.position = "right",
        legend.direction = "vertical",
        axis.line=element_blank())

#CP_20 + CP_21 + CP_22

##ggplotly(CP_20)

#================================== END ==================================

#=========================================================================
#=========================================================================

#====================== Mean Difference ==================================

#Filtering the data to get cases until 31/12/2020
Cases_31Dec2020_AvgPlot <- Counties_IRL %>% filter(TimeStamp == "2020-12-31")

#Creating a column MeanDiff that stores this difference. 
Cases_31Dec2020_AvgPlot <- Cases_31Dec2020_AvgPlot %>% 
  mutate(MeanDiff = ConfirmedC_Norm - mean(ConfirmedC_Norm))

# Setting the color of the bars as red for positive differences and blue for negative differences.
color <- ifelse(Cases_31Dec2020_AvgPlot$MeanDiff < 0, "#0000FF", "#FF0000")

# Plotting a Diverging Bar Chart to show difference from mean for the year 2020.
# Here, I first set the aesthetics where on x axis I used reorder() function to order the county names based on the MeanDiff values.
# I then set y axis to MeanDiff values.
# I used coord_flip() function to flip the cartesian coordinates so that x-axis becomes y-axis, and y-axis becomes x-axis. 
# I used scale_y_continuous() function and in that I set axis breaks to make a continuous scale of sequence of numbers starting from the minimum value of the MeanDiff to the largest value of MeanDiff with a gap of 500.

avg_plot_2020 <- ggplot(Cases_31Dec2020_AvgPlot, aes(x = reorder(CountyName, MeanDiff), y = MeanDiff)) +
  geom_col(position = "identity", fill = color)  +
  coord_flip() + 
  scale_y_continuous(breaks= seq(round_any(min(Cases_31Dec2020_AvgPlot$MeanDiff), 500, f = floor),
                                 round_any(max(Cases_31Dec2020_AvgPlot$MeanDiff), 500, f = ceiling),
                                 by = 500))+
  ggtitle("Difference from mean cases in each county (per 100K population) as of 31 Dec 2020") +
  theme_classic()+
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.y = element_line(color = "grey", size = 2),
    axis.ticks.x = element_line(color = "grey", size = 2),
    axis.title.y = element_blank(),
    axis.title.x.bottom =  element_blank(),
    axis.text.x = element_text(size = 7),
    plot.title = element_text(size = 15),
    plot.margin = margin(3, 6, 3, 3),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid', colour = "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'dotted', colour = "grey"),
    panel.ontop = TRUE
  )

#ggplotly(avg_plot_2020)


# Plotting a Diverging Bar Chart to show difference from mean for the year 2021.
Cases_31Dec2021_AvgPlot <- Counties_IRL %>% filter(TimeStamp == "2021-12-31")
Cases_31Dec2021_AvgPlot <- Cases_31Dec2021_AvgPlot %>% 
  mutate(MeanDiff = ConfirmedC_Norm - mean(ConfirmedC_Norm))


color <- ifelse(Cases_31Dec2021_AvgPlot$MeanDiff < 0, "#0000FF", "#FF0000")

avg_plot_2021 <- ggplot(Cases_31Dec2021_AvgPlot, aes(x = reorder(CountyName, MeanDiff), y = MeanDiff)) +
  geom_col(position = "identity",
           show.legend = FALSE, fill = color)  +
  coord_flip() + 
  scale_y_continuous(breaks= seq(round_any(min(Cases_31Dec2021_AvgPlot$MeanDiff), 500, f = floor),
                                 round_any(max(Cases_31Dec2021_AvgPlot$MeanDiff), 500, f = ceiling),
                                 by = 500))+
  ggtitle("Difference from mean cases in each county (per 100K population) as of 31 Dec 2021") +
  theme_classic()+
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.y = element_line(color = "grey", size = 2),
    axis.ticks.x = element_line(color = "grey", size = 2),
    axis.title.y = element_blank(),
    axis.title.x.bottom =  element_blank(),
    axis.text.x = element_text(size = 7),
    plot.title = element_text(size = 15),
    plot.margin = margin(3, 6, 3, 3),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid', colour = "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'dotted', colour = "grey"),
    panel.ontop = TRUE
  )

##ggplotly(avg_plot_2021)


# Plotting a Diverging Bar Chart to show difference from mean for the year 2022.
Cases_12Dec2022_AvgPlot <- Counties_IRL %>% filter(TimeStamp == "2022-12-12")
Cases_12Dec2022_AvgPlot <- Cases_12Dec2022_AvgPlot %>% 
  mutate(MeanDiff = ConfirmedC_Norm - mean(ConfirmedC_Norm))


color <- ifelse(Cases_12Dec2022_AvgPlot$MeanDiff < 0, "#0000FF", "#FF0000")

avg_plot_2022 <- ggplot(Cases_12Dec2022_AvgPlot, aes(x = reorder(CountyName, MeanDiff), y = MeanDiff)) +
  geom_col(position = "identity",
           show.legend = FALSE, fill = color)  +
  coord_flip() + 
  scale_y_continuous(breaks= seq(round_any(min(Cases_12Dec2022_AvgPlot$MeanDiff), 500, f = floor),
                                 round_any(max(Cases_12Dec2022_AvgPlot$MeanDiff), 500, f = ceiling),
                                 by = 500))+
  ggtitle("Difference from mean cases in each county (per 100K population) as of 12 December 2022") +
  theme_classic()+
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.y = element_line(color = "grey", size = 2),
    axis.ticks.x = element_line(color = "grey", size = 2),
    axis.title.y = element_blank(),
    axis.title.x.bottom =  element_blank(),
    axis.text.x = element_text(size = 7),
    plot.title = element_text(size = 15),
    plot.margin = margin(3, 6, 3, 3),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, linetype = 'solid', colour = "white"),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'dotted', colour = "grey"),
    panel.ontop = TRUE
  )

#ggplotly(avg_plot_2022)

#================================== END ==================================

#=========================================================================
#=========================================================================

#====================== Time Series ======================================

#Subseting the data for the date 2020-12-31 available in the data set. 
MaxDate_Filter_2020 <- subset(Counties_IRL, TimeStamp == "2020-12-31")
data_2020 <- Counties_IRL %>% filter(TimeStamp <= '2020-12-31')

# Identifying the counties having minimum and maximum number of cumulative cases.
MinCounty_2020 <- subset(MaxDate_Filter_2020, ConfirmedC_Norm == min(ConfirmedC_Norm))$CountyName
MaxCounty_2020 <- subset(MaxDate_Filter_2020, ConfirmedC_Norm == max(ConfirmedC_Norm))$CountyName
Main_Counties_2020 <- c("Dublin", MinCounty_2020, MaxCounty_2020)

# Subsetting the data for the above 3 counties for the year 2020 to plot time series graph for these 3 counties.
Main_Counties_Data_2020 <- subset(data_2020, CountyName %in% Main_Counties_2020)

# Subsetting the above data to get the data for these 3 counties for the maximum date available for the year 2020 i.e. 31/12/2020.
#I need this data a to get the maximum y scale value for these 3 counties to put a break and display the county names in front of the lines on the secondary axis.
Main_Counties_Data_Max_TS_2020 <- filter(Main_Counties_Data_2020, TimeStamp == "2020-12-31")


# Plot a time series line graph for all the counties.
# I used geom_line() function to plot the time series line graph for all the counties. 
# In the aesthetics I set group = CountyName to map a different line for each county. I specify the size of the lines to a low value to make each line clearly visible as there are many counties. 
# I also set color of the line to a hexadecimal value for grey color with alpha=0.9 to make the lines transparent as we want the reader’s focus on the lines for the main 3 counties.

TS_IRL_Counties_2020 <- ggplot(data_2020, aes(x=TimeStamp, y=ConfirmedC_Norm)) + 
  scale_x_date(name = "Months", breaks = "2 month", labels=date_format("%b-%y")) +
  geom_line( aes(group = CountyName),size= 0.35, na.rm = TRUE, color="#c1c1c1", 
             alpha =0.9, show.legend = FALSE) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank())


# Plot a time series line graph for the main 3 counties. 
# I used scale_colour_manual() function to create my own custom palettes of color. Here, I used red color for the county with maximum cumulative number of cases, blue for the county with minimum cumulative number of cases, and green for Dublin county
# I made a secondary axis to provide the name of the 3 main counties after the end of each of the specific line. 
# For this, I used dup_axis() function in which I specified the break as the cumulative number of cases using Main_Counties_Data_Max_TS_2020 data

TS_IRL_Main_Counties_2020 <- TS_IRL_Counties_2020 +
  geom_line(data=Main_Counties_Data_2020, size =1, alpha=0.9, show.legend = TRUE, 
            (aes(x =TimeStamp, y=ConfirmedC_Norm, colour= CountyName, 
                 group = CountyName))) +
  scale_colour_manual(values = c("#00FF00","#FF0000" , "#0000FF")) +
  scale_y_continuous(labels = seq(from = 0, to =3500, by=500), 
                     breaks =seq(from = 0, to =3500, by=500),sec.axis = dup_axis(
                       breaks = Main_Counties_Data_Max_TS_2020$ConfirmedC_Norm,
                       labels = Main_Counties_Data_Max_TS_2020$CountyName) )+
  ggtitle("Time Series Plot showing covid cases per 100K population as of 31 December 2020") +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.title.y= element_blank(),
        axis.line = element_blank(), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(  vjust = .5),
        axis.text.y.right = element_text(colour="black", size =10),
        legend.position = "none",
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
        plot.margin = margin(14, 14, 8, 14)
  ) 

#ggplotly(TS_IRL_Main_Counties_2020)




MaxDate_Filter_2021 <- subset(Counties_IRL, TimeStamp == "2021-12-31")
data_2021 <- Counties_IRL %>% filter(TimeStamp >= '2021-01-01', TimeStamp <= '2021-12-31')
MinCounty_2021 <- subset(MaxDate_Filter_2021, ConfirmedC_Norm == min(ConfirmedC_Norm))$CountyName
MaxCounty_2021 <- subset(MaxDate_Filter_2021, ConfirmedC_Norm == max(ConfirmedC_Norm))$CountyName

Main_Counties_2021 <- c("Dublin", MinCounty_2021, MaxCounty_2021)

Main_Counties_Data_2021 <- subset(data_2021, CountyName %in% Main_Counties_2021)
Main_Counties_Data_Max_TS_2021 <- filter(Main_Counties_Data_2021, TimeStamp == "2021-12-31")

TS_IRL_Counties_2021 <- ggplot(data_2021, aes(x=TimeStamp, y=ConfirmedC_Norm)) + 
  scale_x_date(name = "Months", breaks = "2 month", labels=date_format("%b-%y")) +
  geom_line( aes(group = CountyName),size= 0.35, na.rm = TRUE, color="#c1c1c1", 
             alpha =0.9, show.legend = FALSE)


TS_IRL_Main_Counties_2021 <- TS_IRL_Counties_2021 +  
  geom_line(data=Main_Counties_Data_2021, size =1, alpha=0.9, show.legend = TRUE, 
            (aes(x =TimeStamp, y=ConfirmedC_Norm, colour= CountyName, 
                 group = CountyName))) +
  scale_colour_manual(values = c("#00FF00","#FF0000" , "#0000FF")) +
  scale_y_continuous(labels = seq(from = 0, to =50000, by=2000), 
                     breaks =seq(from = 0, to =50000, by=2000),sec.axis = dup_axis(
                       breaks = Main_Counties_Data_Max_TS_2021$ConfirmedC_Norm,
                       labels = Main_Counties_Data_Max_TS_2021$CountyName) )+
  
  
  ggtitle("Time Series Plot showing covid cases per 100K pouplation as of 31 December 2021") +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.title.y= element_blank(),
        axis.line = element_blank(), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(  vjust = .5),
        axis.text.y.right = element_text(colour="black", size =10),
        legend.position = "none",
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
        plot.margin = margin(14, 14, 8, 14)
  ) 

#ggplotly(TS_IRL_Main_Counties_2021)




MaxDate_Filter_2022 <- subset(Counties_IRL, TimeStamp == "2022-12-12")
data_2022 <- Counties_IRL %>% filter(TimeStamp >= '2022-01-01', TimeStamp <= '2022-12-12')

MinCounty_2022 <- subset(MaxDate_Filter_2022, ConfirmedC_Norm == min(ConfirmedC_Norm))$CountyName
MaxCounty_2022 <- subset(MaxDate_Filter_2022, ConfirmedC_Norm == max(ConfirmedC_Norm))$CountyName

Main_Counties_2022 <- c("Dublin", MinCounty_2022, MaxCounty_2022)

Main_Counties_Data_2022 <- subset(data_2022, CountyName %in% Main_Counties_2022)
Main_Counties_Data_Max_TS_2022 <- filter(Main_Counties_Data_2022, TimeStamp == "2022-12-12")

TS_IRL_Counties_2022 <- ggplot(data_2022, aes(x=TimeStamp, y=ConfirmedC_Norm)) + 
  scale_x_date(name = "Months", breaks = "2 month", labels=date_format("%b-%y")) +
  geom_line( aes(group = CountyName),size= 0.35, na.rm = TRUE, color="#c1c1c1", 
             alpha =0.9, show.legend = FALSE)

TS_IRL_Main_Counties_2022 <- TS_IRL_Counties_2022 +  
  geom_line(data=Main_Counties_Data_2022, size =1, alpha=0.9, show.legend = TRUE, 
            (aes(x =TimeStamp, y=ConfirmedC_Norm, colour= CountyName, 
                 group = CountyName))) +
  scale_colour_manual(values = c("#00FF00","#FF0000" , "#0000FF")) +
  scale_y_continuous(labels = seq(from = 0, to =50000, by=2000), 
                     breaks =seq(from = 0, to =50000, by=2000),sec.axis = dup_axis(
                       breaks = Main_Counties_Data_Max_TS_2022$ConfirmedC_Norm,
                       labels = Main_Counties_Data_Max_TS_2022$CountyName) )+
  ggtitle("Time Series Plot showing covid cases per 100K population  as of 12 December 2022") +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.title.y= element_blank(),
        axis.line = element_blank(), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(  vjust = .5),
        axis.text.y.right = element_text(colour="black", size =10),
        legend.position = "none",
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
        plot.margin = margin(14, 14, 8, 14)
  ) 

#ggplotly(TS_IRL_Main_Counties_2022)


#================================== END ==================================

#=========================================================================
#=========================================================================


#====================== UI ===============================================

#I have created a Dashboard UI using Shiny library from R. I have first selected theme
#for my Dashboard. I have split my dashboard into 2 rows. First row contained the main 
#visualization that the use wants to analyse and second row contains the remaining 2 plots
#for user to refer if user wants to during analysis of the main chart.
#The first row also contains a side panel where user can choose from the drop-downs the plot 
#and the year that user wants to analyse. 

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Ireland Covid Statistics(2020-22)"),
  
  fluidRow
  (
    sidebarLayout
    (
      sidebarPanel
      (
        width = 3,
        helpText("Create Covid Statistics plot with information from 2020 to 2022."),
        
        selectInput("selectedPlot", 
                    label = "Choose a stat to display",
                    choices = c("Choropleth Map of Ireland",
                                "Time Series Analysis", "Difference from Mean Confirmed Cases Analysis"),
                    selected = "Choropleth of Ireland"),
        
        selectInput("selectedYear", 
                    label = "Choose a year to display",
                    choices = c("2022", "2021", "2020"),
                    selected = "2022")
      ),
      mainPanel
      (
        width = 9,
        fluidRow
        (
          plotlyOutput("plot")
        )
      )
    )
    ,style='height:60vh;width:191vh;'
  ),
  
  fluidRow
  (
    column(plotOutput("plot1"), width = 6), 
    column(plotOutput("plot2"), width = 6)
    ,style='height:60vh;'
  )
)

#Filling the dashboard with the plots on the basis of user input
server <- shinyServer(function(input,output)
{ 
  output$plot <- renderPlotly({
    if (input$selectedPlot == "Choropleth Map of Ireland") 
    {
      if (input$selectedYear == "2020")
      {
        return(get("CP_20"))
      }
      else if (input$selectedYear == "2021") 
      {
        return(get("CP_21"))
      }
      else 
      {
        return(get("CP_22"))
      }
    }
    else if (input$selectedPlot == "Difference from Mean Confirmed Cases Analysis") 
    {
      if (input$selectedYear == "2020")
      {
        return(get("avg_plot_2020"))
      }
      else if (input$selectedYear == "2021") 
      {
        return(get("avg_plot_2021"))
      }
      else 
      {
        return(get("avg_plot_2022"))
      }
    }
    else if (input$selectedPlot == "Time Series Analysis") 
    {
      if (input$selectedYear == "2020")
      {
        return(get("TS_IRL_Main_Counties_2020"))
      }
      else if (input$selectedYear == "2021") 
      {
        return(get("TS_IRL_Main_Counties_2021"))
      }
      else 
      {
        return(get("TS_IRL_Main_Counties_2022"))
      }
    }
    
  })
  output$plot1 <- renderPlot({
    if (input$selectedPlot == "Choropleth Map of Ireland") 
    {
      if (input$selectedYear == "2020")
      {
        return(get("TS_IRL_Main_Counties_2020"))
      }
      else if (input$selectedYear == "2021") 
      {
        return(get("TS_IRL_Main_Counties_2021"))
      }
      else 
      {
        return(get("TS_IRL_Main_Counties_2022"))
      }
    }
    
    else if (input$selectedPlot == "Time Series Analysis") 
    {
      if (input$selectedYear == "2020")
      {
        return(get("CP_20"))
      }
      else if (input$selectedYear == "2021") 
      {
        return(get("CP_21"))
      }
      else 
      {
        return(get("CP_22"))
      }
    }
    
    else if (input$selectedPlot == "Difference from Mean Confirmed Cases Analysis") 
    {
      if (input$selectedYear == "2020")
      {
        return(get("CP_20"))
      }
      else if (input$selectedYear == "2021") 
      {
        return(get("CP_21"))
      }
      else 
      {
        return(get("CP_22"))
      }
    }
  })
  output$plot2 <- renderPlot({
    if (input$selectedPlot == "Choropleth Map of Ireland") 
    {
      if (input$selectedYear == "2020")
      {
        return(get("avg_plot_2020"))
      }
      else if (input$selectedYear == "2021") 
      {
        return(get("avg_plot_2021"))
      }
      else 
      {
        return(get("avg_plot_2022"))
      }
    }
    
    else if (input$selectedPlot == "Time Series Analysis") 
    {
      if (input$selectedYear == "2020")
      {
        return(get("avg_plot_2020"))
      }
      else if (input$selectedYear == "2021") 
      {
        return(get("avg_plot_2021"))
      }
      else 
      {
        return(get("avg_plot_2022"))
      }
    }
    
    else if (input$selectedPlot == "Difference from Mean Confirmed Cases Analysis") 
    {
      if (input$selectedYear == "2020")
      {
        return(get("TS_IRL_Main_Counties_2020"))
      }
      else if (input$selectedYear == "2021") 
      {
        return(get("TS_IRL_Main_Counties_2021"))
      }
      else 
      {
        return(get("TS_IRL_Main_Counties_2022"))
      }
    }
  })
})


#Starting the dashboard server.
shinyApp(ui,server)




