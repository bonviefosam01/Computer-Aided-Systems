#---LSN 32: Geospatial Analysis 
#-By: Bonvie Fosam
#-April 2020

#We will be using two new packages: ggmap & leaflet

#ggmap is essentially ggplot2...except you plot with maps!
#you can read more about ggmap here: https://github.com/dkahle/ggmap

#leaflet allows you to make interactive maps
#you can read more about leaflet here: https://rstudio.github.io/leaflet/ 


#install ggmap and leaflet if this is your first time using them:
#install.packages("ggmap")
#install.packages("leaflet")

#call the libraries you will need:
library(tidyverse)
library(leaflet)
library(ggmap)

#check/set your working directory!
getwd()


#First, let's import our data set. For the first exercise we'll focus on starbucks  
starbucks <- read_csv("Starbucks.csv")

#Let's narrow our focus to those starbucks in the state of NY and NJ
NY_SB_loc <- starbucks %>% 
  filter(starbucks$`State/Province` == "NY")


ggplot(data = NY_SB_loc, aes(x = Longitude, y = Latitude, color = 'Ownership Type'))+
  geom_point()

#Alright, now it's time to get our map

#In the past, using Google maps was easy and free to download maps. 
#Now, you are required to use an API and enable billing. 
#If you see DSCOE posts and tutorials online using  qmap  and  get_map , 
#they will not work without setting up an API.
#As such, we are going to use Stamen maps to get map backgrounds.

#First, we need to establish some boundaries for the map
height <- max(NY_SB_loc$Latitude) - min(NY_SB_loc$Latitude)
width <- max(NY_SB_loc$Longitude) - min(NY_SB_loc$Longitude)
boundaries <- c(left = min(NY_SB_loc$Longitude) - width * .15,
                right = max(NY_SB_loc$Longitude) + width * .15, 
                bottom = min(NY_SB_loc$Latitude) - height * .15,
                top = max(NY_SB_loc$Latitude) + height * .15)

stam_map <- get_stamenmap(bbox = boundaries, 
                              zoom = 6, 
                              maptype = "toner-lite")
#look at the map
ggmap(stam_map)


#try and different map
osm_map <- get_map(location = boundaries, zoom = 7, source = 'osm')

#look at the map
ggmap(osm_map)


#Okay, let's put our point data on the map
ggmap(stam_map) + 
  geom_point(data = NY_SB_loc, aes(x = Longitude, y = Latitude), color = 'green')

#We can change the formatting the same way we do with ggplot2
ggmap(osm_map) + 
  geom_point(data = NY_SB_loc, aes(x = Longitude, y = Latitude, colour = as.factor(NY_SB_loc$`Ownership Type`), 
             ), size = 3)+
  labs(color = "Owenership Type")

#Let's try a different type of map using the leaflet package. 
#This gives us more interative capabilities similar to google maps

leaflet() %>%
  setView(lng = -73.95, lat = 41.39, zoom =12) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addCircleMarkers(data = NY_SB_loc,
                   lng = ~ Longitude,
                   lat = ~ Latitude,
                   radius = 2)

#Sweet! Now let's make this a little more interactive by adding some popup features to our spatial data points
leaflet() %>%
  setView(lng = -73.95, lat = 41.39, zoom =10) %>%
  addProviderTiles("Esri.WorldTopoMap") %>% 
  addCircleMarkers(data = NY_SB_loc,
                   lng = ~ Longitude,
                   lat = ~ Latitude,
                   radius = 5, popup = paste("Location: ", 
                                             NY_SB_loc$City,
                                             #notice this wants html to do styling
                                             "<br>",
                                             "Phone: ", 
                                             NY_SB_loc$`Phone Number`))

#You can save maps to use outside RStudio, by clicking the "Export" dropdown on the "Plots" tab.
#Options: 1. Save as Image. 2. Save as PDF. 3. Copy to Clipboard
#To save a leaflet map:
#Click the "Export" dropdown on the "Viewer" tab.
#Click "Save as a Webpage". This will create an html file for the leaflet map.
#Name the html file. It will default to saving in your working directory.

#In-class exercise

#read in terrorism dataset
terror <- read.csv("terror_2016.csv")

#pick a country or region you are interested in 
terror2016_Iraq <- terror %>% 
  filter(country_txt == "Iraq" | country_txt == "Syria")

#Get a map of the area of interest, add popups to show attack type
leaflet() %>% 
  setView(lng = 44.366, lat = 33.315, zoom = 5) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>% 
  addCircleMarkers(data = terror2016_Iraq,
                   lng = ~ longitude,
                   lat = ~ latitude,
                   radius = 2,
                   popup = ~ attacktype1_txt)

#Look at only the suicide attacks
terror_suicide <- terror2016_Iraq %>% 
  filter(suicide == 1)

#plot with points
leaflet() %>% 
  setView(lng = 44.366, lat = 33.315, zoom = 5) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>% 
  addCircleMarkers(data = terror_suicide,
                   lng = ~ longitude,
                   lat = ~ latitude,
                   radius = 2,
                   popup = ~ attacktype1_txt)


#-challenge
#Make a new plot - something new. Try chaging the maptype, title/labels, or something else

terror_success <- terror %>% 
  filter(country_txt == "Somalia") %>% 
  filter(success == 1) %>% 
  filter(is.na(latitude) == FALSE) %>% 
  filter(is.na(longitude) == FALSE)

  

leaflet() %>% 
  setView(lng = mean(terror_success$longitude), lat = mean(terror_success$latitude) , zoom = 5) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>% 
  addCircleMarkers(data = terror_success,
                   lng = ~ longitude,
                   lat = ~ latitude,
                   radius = 2,
                   popup = ~ attacktype1_txt)



#-pro challenge
#build a map with layers for different kinds of terror attacks that I can select/deselect types 



