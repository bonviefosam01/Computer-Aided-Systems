#---Lesson 33: Geospatial Analysis 2
#-By: CPT Williams
#-April 2020

#Read in libraries
#Note that you may need to install leaflet.extras, rgdal, raster, and 
#sf before completing this exercise!
library(leaflet)
library(tidyverse)
library(ggmap)
library(leaflet.extras)
library(rgdal)
library(sf)
library(raster)

#Make sure to check and set your working directory!
getwd()

#Read in the Afghanistan Conflict data from the Armed Conflict Location and 
#Event Data Project. (source: https://www.acleddata.com/tag/afghanistan/)
afgcon <- read.csv('Afghanistan.csv', stringsAsFactors = FALSE)

#We'll start our plot with a brief review by building our geospatial analysis around a stemmap
#Step 1, set up our boundaries

height <- max(afgcon$latitude) - min(afgcon$latitude)
width <- max(afgcon$longitude) - min(afgcon$longitude)
afgboundries <- c(left = min(afgcon$longitude) - width * 0.05,
                  right = max(afgcon$longitude) + width * 0.05,
                  bottom = min(afgcon$latitude) - height * 0.05,
                  top = max(afgcon$latitude) + height * 0.05)

#Step 2, go get your map based on the boundaries you've established

afgmap <- get_stamenmap(bbox = afgboundries,
                        zoom = 6,
                        maptype = 'toner-lite')

ggmap(afgmap)

#Step 3, plot your point data on your map 
#(remember, we can format just like we do with ggplot2)

ggmap(afgmap) + geom_point(data = afgcon %>% 
                             filter(event_type == 'Battles'),
                           aes(x = longitude, y = latitude, 
                               colour = admin1), shape =10, size = 1)



#If we want to look at the general density of conflict throughout AFG, 
#we could use a heat map by calling the stat_desnity2d function
#the ..level.. goes through the latitudes and longitiudes and makes each different value a different level
#i guess this helps when doing the heat map to get the different range of colors

ggmap(afgmap) + stat_density2d(data = afgcon, 
                               aes(x = longitude, y = latitude, 
                                   fill = ..level.., alpha = ..level..),
                               geom = 'polygon', show.legend = FALSE)


#By utilizing our facet_wrap function, we can provide more fidelity on the 
#conflict density by event_type
ggmap(afgmap) + stat_density2d(data = afgcon, 
                               aes(x = longitude, y = latitude, 
                                   fill = ..level.., alpha = ..level..),
                               geom = 'polygon', show.legend = FALSE) + facet_wrap(~event_type)


#What if we want to increase the fildelity of conflict data by province throughout the country?
#We'll shift back to our leaflet maps for more interative functionality


#The first step is to get the polygon data for AFG, we'll use the raster package for this
afgprov <- raster::getData('GADM', country = "Afghanistan", level = 1)

#this gets us our polygons --- look at the class of the object adn what it plots
raster::plot(afgprov)

#make your points into a spdf
#create a df of only cordinates 
afg2 <- afgcon %>% dplyr::select(longitude, latitude)


#make Spatial Points Data Frame (SPDF)
afg3 <- SpatialPointsDataFrame(coords = afg2, data = afgcon, 
                               proj4string = CRS(proj4string(afgprov)))

# Set a unique identifier for both of the data frames
afgprov@data <- mutate(afgprov@data, poly_id = as.numeric(rownames(afgprov@data)))
afg3@data <- mutate(afg3@data, data_id = as.numeric(rownames(afg3@data)))

#gets data from polygon for each point
afg4 <- over(afg3, afgprov)


#the order didn't change so re add data_id to the new table
afg5 <- mutate(afg4, data_id = as.numeric(rownames(afg4)))

#Now join the data with the new poly data
afg6 <- left_join(afg3@data, afg5, by = c("data_id" = "data_id"))

## Now we can aggregate the data by poly_id for plotting
afg7 <- afg6 %>% 
  filter(event_type == 'Battles') %>% 
  group_by(poly_id) %>% 
  summarise(count = n()) %>% 
  arrange(count)


#add whatever attributes you've created for the polygons back into the original spdf
afgprov@data <- left_join(afgprov@data, afg7)


#create bins
bins <- c(0, 10, 50, 100, 150, 200, 500, 1000, Inf)

#create pallet
#Note: Yl0rRd is from http://colrd.com/palette/19079/. 
pal <- colorBin("YlOrRd", domain = afgprov$count, bins = bins)

#Ok, let's plot the point data using leaflet and our new raster polygons
leaflet() %>% 
  setView(lng = 65.73, lat = 31.63, zoom = 6) %>% 
  addPolygons(data = afgprov,
              fillColor = ~pal(count),
              weight = 1,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7)


#lets add labels
labels <- sprintf("<strong>%s</strong><br/>%g Battles",
                  afgprov$NAME_1,
                  afgprov$count) %>% 
  lapply(htmltools::HTML)

#replot w/ labels
leaflet() %>%
  setView(lng = 65.73, lat = 31.63, zoom = 6) %>%
  #Using THREE different tiles here -- NatGeoWorld, Toner, and TonerLite
  addProviderTiles("Esri.NatGeoWorldMap", group = "NatGeo") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  #Adding polygons
  addPolygons(data = afgprov,
              fillColor = ~pal(count),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7, 
              highlight = highlightOptions(
                weight = 5, 
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
              ) %>% 
              # Layers control, so the user can choose a tile
              addLayersControl(
                baseGroups = c("NatGeo", "Toner", "Toner Lite"),
                options = layersControlOptions(collapsed = FALSE)
              )
  

##############################
######Your turn
##############################

#####Ok, take this map and make it better by one feature and with 
#another data mapped (ie event_type, actor, something...)

afgactor <- afg6 %>% 
  filter(actor1 == 'Taliban') %>% 
  group_by(poly_id) %>% 
  summarise(count3 = n()) %>% 
  arrange(count3)

afgprov@data <- left_join(afgprov@data, afgactor)

#afgprov@data <- cbind(afgprov@data , afgsub$count)

pal <- colorBin("YlOrRd", domain = afgprov$count, bins = bins)

labels <- sprintf("<strong>%s</strong><br/>%g Records of Taliban",
                  afgprov$NAME_1,
                  afgprov$count3) %>% 
  lapply(htmltools::HTML)

leaflet() %>%
  setView(lng = 65.73, lat = 31.63, zoom = 6) %>%
  #Using THREE different tiles here -- NatGeoWorld, Toner, and TonerLite
  addProviderTiles("Esri.NatGeoWorldMap", group = "NatGeo") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  #Adding polygons
  addPolygons(data = afgprov,
              fillColor = ~pal(count2),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7, 
              highlight = highlightOptions(
                weight = 5, 
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
  ) %>% 
  # Layers control, so the user can choose a tile
  addLayersControl(
    baseGroups = c("NatGeo", "Toner", "Toner Lite"),
    options = layersControlOptions(collapsed = FALSE)
  )




######Things you might see in the "wild"
#But what if my data is not in lat, long format, how do I map it?

  
#You can mathematically convert between between Decimal Degrees and Degrees, Minutes, Seconds using the  conv_unit()  
#function in the  measurements  package.

#Install if needed
#install.packages("measurements")
library(measurements)

measurements::conv_unit(x = c("38.8897", "-77.0089"), 
                          from = "dec_deg", to = "deg_min_sec")

  
measurements::conv_unit(x = c("38 53 23", "-77 0 32"), 
                          from = "deg_min_sec", to = "dec_deg")

#For MGRS formatted point data, we'll use the mgrs package
#West point MGRS 18TWL87278248

#Install fron GitHub if needed
#devtools::install_github("hrbrmstr/mgrs")
library(mgrs)

#From MGRS to Lat/Lon
mgrs_to_latlng("18TWL87278248")

#Lat/Lon to MGRS
latlng_to_mgrs(41.38912, -73.95617)

 