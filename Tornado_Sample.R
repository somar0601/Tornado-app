#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(shinythemes)
library(shiny)
library(shinydashboard)
library(markdown)
library(ggplot2)
library(data.table)
library(dplyr)
library(lubridate)
library(jpeg)
library(DT)
library(grid)
library(rsconnect)
library(scales)
library(leaflet)
library(geosphere)
library(gridExtra)
library(maptools)
library(raster)
library(stringi)
library(htmltools)
library(ggthemes)
library(plotly)
#library(sqldf)



# DATA PRE-PROCESSING
#Read data
st=c("All", "MO","IL","OH","AR","TX","MS","LA","TN","OK","FL","AL","SC","KS","IA","SD","NE","WY","NC","GA","ND","MN","WI","IN","PA","N","CT","CO","WV","MD","KY","CA","VA","NJ","MI","MA","NH","OR","NY","MT","AZ","UT","ME","VT","ID","WA","DE","HI","PR","AK","NV","RI","DC")
data = fread("Tornado_Dataset/allTornadoes.csv")
fipsCountyMap = fread("Tornado_Dataset/fipsCountyMap.csv")
stateCoordinates = fread("Tornado_Dataset/stateCoordinates.csv")
fatalities_df=read.csv( file = "Tornado_Dataset/heat_fatalities.csv",header=TRUE)
injuries_df=read.csv(file = "Tornado_Dataset/heat_injuries.csv",header=TRUE)
states_data = fread("Tornado_Dataset/states.csv")
# us = fread("Tornado_Dataset/USdataGADM_lvl2.csv")
# save.image(us,file = "Tornado_Dataset/USdataGADM_lvl211.rds")
us<-getData('GADM', country='USA', level=2)
# us<-load('Tornado_Dataset/USdataGADM_lvl211.RData')
tornadoesByCounty=read.csv( file = "Tornado_Dataset/tornadospercounty.csv",header=TRUE,colClasses=c("NULL", NA, NA))[0:20,]
names(tornadoesByCounty)=c("County","#Tornados")
# write.csv(us,"USdataGADM_lvl2.csv")
ILall<-subset(us,NAME_1=="Illinois")   #One extra county in lake michigan
t1=ILall@data
ILall@polygons=ILall@polygons[-c(642-592)]
ILall@data=ILall@data[-c(642-592), ]
illinois=ILall@data
illinois$NAME_2=gsub(" ", "",gsub("\\.", " ", illinois$NAME_2))
illinois$county=illinois$NAME_2
illinois$county <- tolower(illinois$county)
illinois[illinois$county=="saintclair",]$county="stclair"

countydata=read.csv(file = "Tornado_Dataset/countydata.csv",header=TRUE)
ILall@data=countydata

#Change adjust loss data 
data[yr < 1996 & loss == 1, loss := 25/1000000]
data[yr < 1996 & loss == 2, loss := 275/1000000]
data[yr < 1996 & loss == 3, loss := 2750/1000000]
data[yr < 1996 & loss == 4, loss := 27500/1000000]
data[yr < 1996 & loss == 5, loss := 275000/1000000]
data[yr < 1996 & loss == 6, loss := 2750000/1000000]
data[yr < 1996 & loss == 7, loss := 27500000/1000000]
data[yr < 1996 & loss == 8, loss := 50000000/1000000]
data[yr == 2016, loss:= loss/1000000]

#Set names and datatype
newNames = c("tornadoNumber", "year", "month", "day", "date", "time", "timeZone", "state", "fips", 
             "stateNumber", "fscale","injuries", "fatalities", "loss", "cropLoss", "startLat", 
             "startLon", "endLat", "endLon", "length", "width", "numberOfStates", "stateNumber2", 
             "tornadoSegment","fips1st", "fips2nd", "fips3rd", "fips4th","fscale2")
setnames(data, newNames)
data$date = as.Date(data$date)
factor_list = c("tornadoNumber", "year", "month", "day", "timeZone", "state", "fips", 
                "stateNumber", "fscale","fips1st", "fips2nd", "fips3rd", "fips4th","fscale2")
data[, (factor_list) := lapply(.SD, factor), .SDcols=factor_list]

#Create tornado ID (unique to the state single track, NOT unique as a key)
data[, tornadoID := paste(year,tornadoNumber, sep = "")]
data[, tornadoID := factor(tornadoID)]

#REMOVE BAD DATA
data$fscale = factor(data$fscale)

# HELPER FUNCTIONS
normalize = function (x){
  (x-min(x))/(max(x)-min(x))
}

getGeoDist = function(startlat, startlon, endlat, endlon){
  rad = pi/180
  a1 = startlat * rad
  a2 = startlon * rad
  b1 = endlat * rad
  b2 = endlon * rad
  dlon = b2 - a2
  dlat = b1 - a1
  a = (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  R = 6378.145
  distance = R * c
  distance
}

states=states_data[,c("State")]
# Define new column
#data = data %>%
#  mutate(distance = distm(c(startLon, startLat),c(endLon, endLat), fun = distHaversine)) %>%
#  data.table()

#data[,distance:= distm(as.numeric(c(startLon, startLat)),as.numeric(c(endLon, endLat)), fun = distHaversine)]
#distm(c(1, 1),c(2, 2), fun = distHaversine)

# PLOT GENERATING FUNCTIONS

#PART C9  
#Define Units
units = c("", "people", "people", "miles", "miles", "million USD")
names(units) = c("fscale", "injuries", "fatalities", "length", "width", "loss")

#Define Imperial Units
units_metric = c("", "people", "people", "km", "km", "million USD")
names(units_metric) = c("fscale", "injuries", "fatalities", "length", "width", "loss")

getmaxwidth = function(){
  val<-max(data[,c('width')])
  val
}
getmaxlength=function(){
  val<-max(data[,c('length')])
  val}

map_track_state_year = function(year_var, state_var1, state_var2, frange = c(-9,1,2,3,4,5), wrange = c(0,5000), lrange = c(0,250), 
                                irange = c(0,1800), fatrange = c(0,160), map_markers="fscale", lossrange = c(0,3000), units_set){
  
  state_var = c(state_var1, state_var2)
  unit = units_set[[map_markers]]
  
  track_data = data[stateNumber2 == 1 & startLat > 0 & startLon < 0 & endLat > 0 & endLon <0 &
                      year == year_var & 
                      state %in% state_var &
                      fscale %in% frange &
                      width %between% wrange & 
                      length %between% lrange & 
                      injuries %between% irange &
                      fatalities %between% fatrange &
                      loss %between% lossrange,
                    c("tornadoID", "startLon", "startLat","endLat","endLon", map_markers), with = FALSE]
  setnames(track_data, map_markers, "map_marker")
  
  if (unit == "km"){
    track_data[,map_marker := map_marker*1.6]
  }
  
  #Normalize data - In order to visualize
  track_data[, ("map_marker_normal") := (normalize(as.numeric(map_marker))+1)*5]
  
  track_state_start = track_data[,c("tornadoID", "startLon", "startLat", "map_marker", "map_marker_normal"), with = FALSE]
  track_state_end = track_data[,c("tornadoID", "endLat","endLon", "map_marker", "map_marker_normal"), with = FALSE]
  setnames(track_state_start, c("startLon", "startLat"), c("lon","lat"))
  setnames(track_state_end, c("endLon", "endLat"), c("lon","lat"))
  track_state = rbind(track_state_start,track_state_end)
  
  pal_colors = colorRamp(c("#ff6767", "#400000"))
  
  if (is.factor(track_data$map_marker)){
    pal <- colorFactor(
      palette = pal_colors,
      domain = track_state$map_marker)
    # track_data[, ("map_marker") := as.numeric(as.character(map_marker))]
  }
  else{
    pal <- colorNumeric(
      palette = pal_colors, 
      domain = track_state$map_marker)
  }
  hurricane_icon = makeIcon("hurricane-icon.png",36,24,iconAnchorX = 10, iconAnchorY = 6)
  html_legend <- "<img src='hurricane-icon.png' style='width:10px;height:10px;'>Tornado End"
  
  
  m = leaflet(options = leafletOptions(minZoom = 5, maxZoom = 18)) %>% 
    addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
    addProviderTiles(providers$Stamen.Toner, group = "Dark") %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "City Classic") %>%
    addProviderTiles(providers$Stamen.Terrain, group = "Topological") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Sattelite") %>%
    setView(-87.987437, 41.913741, zoom = 6) %>%
    addLayersControl(baseGroups = c("Light","Dark", "City Classic", "Topological", "Sattelite"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend(title = paste(map_markers, " (", unit, ")", sep = ""), "bottomright", pal = pal, values = track_state$map_marker,
              opacity = 1, bins = 5, labFormat = labelFormat(digits = 1)) %>%
    addControl(html = html_legend, position = "bottomleft")
  for (i in unique(track_state$tornadoID)) {
    m <- m %>%
      addPolylines(data = track_state[tornadoID == i],
                   lng = ~lon,
                   lat = ~lat,
                   col = ~pal(map_marker),
                   weight = ~(map_marker_normal),
                   highlightOptions = highlightOptions(color = "white", weight = 5,
                                                       bringToFront = TRUE),
                   label = ~paste(map_markers, ":",map_marker, " ", unit),
                   labelOptions = labelOptions(noHide = F, textsize = "22px",direction = 'auto')) %>%
      addMarkers(data = track_state_end[tornadoID == i], 
                 icon = hurricane_icon,
                 lng = ~lon,
                 lat = ~lat)
  }
  
  
  return(m)
}

#Function for B4
map_track_top10 = function(state_choice){
  unit = units[["fatalities"]]
  track_data = data[stateNumber2 == 1 & startLat > 0 & startLon < 0 & endLat > 0 & endLon <0 &
                      state %in% state_choice,
                    c("tornadoID", "startLon", "startLat","endLat","endLon", "fatalities", "injuries","year"), with = FALSE]
  
  track_data = track_data[order(-fatalities, -injuries), head(.SD,10)]
  
  #Normalize data - In order to visualize
  track_data[, ("map_marker_normal") := (normalize(fatalities)+1)*5]
  
  track_state_start = track_data[,c("tornadoID", "startLon", "startLat", "fatalities", "injuries", "map_marker_normal","year"), with = FALSE]
  track_state_end = track_data[,c("tornadoID", "endLat","endLon", "fatalities", "injuries", "map_marker_normal","year"), with = FALSE]
  setnames(track_state_start, c("startLon", "startLat"), c("lon","lat"))
  setnames(track_state_end, c("endLon", "endLat"), c("lon","lat"))
  track_state = rbind(track_state_start,track_state_end)
  
  pal_colors = colorRamp(c("#ff6767", "#400000"))
  
  pal <- colorNumeric(
    palette = pal_colors, 
    domain = track_state$fatalities)
  
  hurricane_icon = makeIcon("hurricane-icon.png",32,20,iconAnchorX = 10, iconAnchorY = 5)
  html_legend <- "<img src='hurricane-icon.png' style='width:10px;height:10px;'>Tornado End showing Years"
  
  
  m = leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
    addProviderTiles(providers$Stamen.Toner, group = "Dark") %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "City Classic") %>%
    addProviderTiles(providers$Stamen.Terrain, group = "Topological") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Sattelite") %>%
    setView(-89, 40, zoom = 8) %>%
    addLayersControl(baseGroups = c("Light","Dark", "City Classic", "Topological", "Sattelite"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend(title = paste("Fatalities", " (", unit, ")", sep = ""),"bottomright", pal = pal, values = track_state$fatalities,
              opacity = 1, bins = 5, labFormat = labelFormat(digits = 1)) %>%
    addControl(html = html_legend, position = "bottomleft")
  
  for (i in unique(track_state$tornadoID)) {
    m <- m %>%
      addPolylines(data = track_state[tornadoID == i],
                   lng = ~lon,
                   lat = ~lat,
                   col = ~pal(fatalities),
                   weight = ~(map_marker_normal),
                   highlightOptions = highlightOptions(color = "white", weight = 3,
                                                       bringToFront = TRUE),
                   label = ~paste("Fatalities:",fatalities),
                   labelOptions = labelOptions(noHide = F, textsize = "22px")) %>%
      
      addMarkers(data = track_state_end[tornadoID == i], 
                 icon = hurricane_icon,
                 lng = ~lon,
                 lat = ~lat,
                 label=~paste0("Year:",year),
                 labelOptions = labelOptions( textsize = "18px",noHide = FALSE,riseOnHover=TRUE,riseOffset=10,direction = "auto")
                 
      )
  }
  
  
  return(m)
}

# Label Option 1
l1<- mapply(
  function(county, inj, fatal, losses) {
    htmltools::HTML(
      sprintf(
        "<div style='font-size:25px;height:125px;width:450px;float:left'>
        <span style='font-size:28px;font-weight:bold'>%s</span><br/>-------------------------------------------------------<br/>
        
        
        <div style='width:95%%'>
        <span style='float:left'>Injuries:</span>
        <span style='float:right'>%g</span>
        
        <br/>
        <span style='float:left'>Fatalities:</span>
        <span style='float:right'>%g</span>
        
        <br/>
        <span style='float:left'>Losses (Millions of $):</span>
        <span style='float:right'>%f</span>
        
        <br/>
        
        
        <br/>
        
        
        
        </div>
        
        </div>",
        
        
        county,
        inj,
        fatal,
        losses
      )
    )
  },
  countydata$county[1:102],
  ceiling(round(countydata$injured[1:102],2)),
  ceiling(round(countydata$fatalities[1:102],2)), 
  round(countydata$losses[1:102],2),
  SIMPLIFY = F) 


# Label Option 2
l2 <- mapply(
  function(county, f0, f1, f2,f3,f4,f5, f9) {
    htmltools::HTML(
      sprintf(
        "<div style='font-size:25px;height:220px;width:170px;float:left'>
        <span style='font-size:28px;font-weight:bold'>%s</span><br/>-----------------------<br/>
        
        
        <div style='width:95%%'>
        <span style='float:left'>F0 Scale:</span>
        <span style='float:right'>%d</span>
        
        <br/>
        <span style='float:left'>F1 Scale:</span>
        <span style='float:right'>%d</span>
        
        <br/>
        <span style='float:left'>F2 Scale:</span>
        <span style='float:right'>%d</span>
        
        
        <br/>
        <span style='float:left'>F3 Scale:</span>
        <span style='float:right'>%d</span>
        
        <br/>
        <span style='float:left'>F4 Scale:</span>
        <span style='float:right'>%d</span>
        
        
        <br/>
        <span style='float:left'>F5 Scale:</span>
        <span style='float:right'>%d</span>
        
        <br/>
        <span style='float:left'>Unknown:</span>
        <span style='float:right'>%d</span>
        
        
        <br/>
        
        
        <br/>
        
        
        <br/>
        
        
        <br/>
        
        
        
        
        
        <br/>
        
        
        <br/>
        
        
        
        </div>
        
        </div>",
        
        
        county,
        f0,
        f1,
        f2,
        f3,
        f4,
        f5,
        f9
      )
    )
  },
  countydata$county[1:102],
  countydata$f0[1:102],
  countydata$f1[1:102], 
  countydata$f2[1:102], 
  countydata$f3[1:102],
  countydata$f4[1:102],
  countydata$f5[1:102],
  countydata$f9[1:102],
  SIMPLIFY = F) 


#PART B1

shinyApp(
  ui = navbarPage(h1("You Spin me Round!"),
                  theme = shinytheme("superhero"),
                  tabPanel(h2("About"),
                           h3("You Spin Me Round is an application made by Pedro Borges, Megan Hauck, Shoaib Khan and Namaswi Chandarana for UIC's Spring 2018 CS 424, Visualization and Visual Analytics, and Professor Andy Johnson."),
                           h3("The application visualizes 62,000 tornadoes for the entire U.S. from 1950 to 2016, with about 2,500 tornadoes in Illinois alone. Several features allow users to navigate tornado size and loss data."),
                           h3("Run time may be slow, as the app was meant to be run on the EVL server"),
                           h3("This layout and appearance of the app has been  edited from the original app by Shoaib Khan"),
                           HTML("<br>"),
                           h3("Data Links:"),
                           
                           HTML("<h3>Data Source: <a href='http://www.spc.noaa.gov/wcm/index.html#data'>http://www.spc.noaa.gov/wcm/index.html#data</a>"),
                           HTML("<h3>Data Source Description: <a href='http://www.spc.noaa.gov/wcm/data/SPC_severe_database_description.pdf'>http://www.spc.noaa.gov/wcm/data/SPC_severe_database_description.pdf</a>"),
                           HTML("<h3>Current version Source Code: <a href='https://github.com/skhan230/Tornado-app'>https://github.com/skhan230/Tornado-app</a>"),
                           
                           HTML("<h3>Original app source code: <a href='https://github.com/mhauck3/TornadoP3'>https://github.com/mhauck3/TornadoP3#data</a>")
                           
                  ),
                  tabPanel(h2("Tornado Trajectories"),
                           sidebarLayout(
                             sidebarPanel(width = 2,
                                          
                                          
                                          radioButtons("units", h3("Units:"),
                                                       choices = list("Imperial" = 1, "Metric" = 2),selected = 1),
                                          h2("Filters"),
                                          #uiOutput("width_input"),
                                          sliderInput("width_input",label=h3("Width:"), min=0, max=max(data[,c('width')])*1.6, value = c(0, max(data[,c('width')]))*1.6,width="100%"),
                                          #uiOutput("length_input"),
                                          sliderInput("length_input",label=h3("Length:"), min=0, max=max(data[,c('length')])*1.6, value = c(0, max(data[,c('length')]))*1.6,width="100%"),
                                          sliderInput("injuries_input",label=h3("Injuries (no of people):"), min=0, max=max(data[,c('injuries')]), value = c(0, max(data[,c('injuries')])),width="100%"),
                                          sliderInput("fatalities_input",label=h3("Fatalities (no of people):"), min=0, max=max(data[,c('fatalities')]), value = c(0, max(data[,c('fatalities')])),width="100%"),
                                          sliderInput("loss_input",label=h3("Loss(in million USD):"), min=0, max=max(data[,c('loss')]), value = c(0, max(data[,c('loss')])),width="100%"),
                                          checkboxGroupInput("fscale_input", h3("F-scale:"),
                                                             #c("Unknown" = "-9",
                                                             # "0" = "0",
                                                             #"1" = "1",
                                                             #"2"="2",
                                                             #"3"="3","4"="4","5"="5"),
                                                             inline=T,selected=list("-9", "0", "1", "2","3","4","5"),
                                                             choiceNames=c(("Unknown"),("0"),("1"),("2"),("3"),("4"),("5")),
                                                             choiceValues = c("-9","0","1","2","3","4","5")), #Ugly range (just to include -9). Need to change this
                                          
                                          tags$style(HTML(".irs-grid-text { font-size: 0px; } .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
                                          tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red }")),
                                          tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: red}")),
                                          tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: red}")),
                                          tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: red}"))
                             ),
                             
                             mainPanel(
                               
                               fluidRow(width = 11,
                                        
                                        column(12,
                                               sliderInput("year_input",label=h4("Year:"), min=1950, max=2016, value = 1950,animate = TRUE,width="100%",step=1, sep = ""),
                                               
                                               tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: red} .irs-max {font-size: 20px;font-family: 'arial'; color: white;}
                                                               .irs-min {font-size: 20px;font-family: 'arial'; color: white;}")),
                                               tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: red}")),
                                               fixedRow(
                                                 column(12,
                                                        column(1,h3("Compare Illinois Data to")),
                                                        column(12,selectInput("state_select", "", states,selected="Wisconsin")),
                                                        column(7, radioButtons("radio", h4("View according to"),
                                                                               choices = list("F-scale" = "fscale", "Injuries" = "injuries",
                                                                                              "Losses" = "loss", "Length" = "length", 
                                                                                              "Width" = "width", "Fatalities" = "fatalities"),
                                                                               selected = "fscale",inline=T)),
                                                        tags$style(type='text/css', ".selectize-input { font-size: 28px; line-height: 28px;} .selectize-dropdown { font-size: 25px; line-height: 25px; } "),
                                                        
                                                        leafletOutput("map_track",height = "1200px"),
                                                        tags$style("input[type='radio']:checked+span{ 
                                                                   
                                                                   font-size: 24px;
                                                                   }
                                                                   input[type='radio']+span{ 
                                                                   
                                                                   font-size: 24px;
                                                                   }"),
                                                        tags$style("input[type='checkbox']:checked+span{ 
                                                                   
                                                                   font-size: 24px;
                                                        }
                                                                   input[type='checkbox']+span{ 
                                                                   
                                                                   font-size: 24px;
                                                                   }")
                                                        
                                                        
                                                        
                                                        )
                                                 
                                                 ),
                                               fixedRow("")
                                               
                                               )#,
                                        
                                        #column(2#,
                                               
                                           #    fluidRow("Heat Map"),
                                               
                                               
                                          #    radioButtons("heat_map_option", h3("Heat-Map Type:"),
                                                        #    choices = list("Based on Injuries " = 1, "Based on Fatalities" = 2),selected = 2,inline=T,width="1000px"),
                                           #    plotOutput("HeatMaps",width="375px",height="700px")#,
                                               
                                          #     radioButtons("hover_map_option", h3("On Hover Details:"),
                                          #                  choices = list("View amount of Injuries, Fatalities and Losses" = 1, "View frequency of Magnitudes" = 2),selected = 2,inline=T,width="1000px"),
                                         #      leafletOutput("Heat_Maps",width="1000px",height="700px")
                                               
                                        #)
                                               )
                                        )
                               )),
                  tabPanel(h2("Analysis"),
                           
                           
                           sidebarPanel(width = 2,
                                      
                                        
                                        radioButtons("hr", h3("Hour:"),
                                                     choices = list("24Hr" = 1, "12Hr" = 2),selected = 1),
                                        checkboxInput("showTables",label = h4("Show Tables"), value = FALSE),
                                        checkboxInput("showGraphs",label = h4("Show Graphs"), value = TRUE),
                                        h3("Select Unit:"),
                                        checkboxInput("imperial",label = h4("Imperial"), value = TRUE),
                                        checkboxInput("metric",label = h4("Metric"), value = FALSE),
                                        
                                        radioButtons("radioDamages", h4("View according to"),
                                                     choices = list("Injuries" = "Injuries",
                                                                    "Losses" = "Losses",
                                                                    "Fatalities" = "Fatalities"),
                                                     selected = "Injuries"),
                                        selectInput("state_analysis", "", st,selected="WI")
                                        # h2("Filters")
                                        # uiOutput("SliderWidget"),
                           ),
                           
                           mainPanel("Analysis Plots",
                                     fluidPage(width= 11,
                                               tabBox(title = "",
                                                      # id = "tab1",
                                                      width = "100%",
                                                      # height = "2000px",
                                                      id = "tabset2", 
                                                      
                                                      navbarMenu("Time-Distance Count",
                                                        tabPanel("Yearly Sum",
                                                                 conditionalPanel(condition = "input.showTables == true",dataTableOutput("c1table")),br(),
                                                                 conditionalPanel(condition = "input.showGraphs == true",plotlyOutput(width ="100%","c1count")),br(),
                                                                 conditionalPanel(condition = "input.showGraphs == true",plotlyOutput(width ="100%","c1perc"))),
                                                        tabPanel("Monthly Sum",
                                                                 conditionalPanel(condition = "input.showTables == true",dataTableOutput("c2table")),br(),
                                                                 conditionalPanel(condition = "input.showGraphs == true",plotlyOutput(width ="100%","c2count")),br(),
                                                                 conditionalPanel(condition = "input.showGraphs == true",plotlyOutput(width ="100%","c2perc"))),
                                                        tabPanel("Hourly Sum",
                                                                 conditionalPanel(condition = "input.showTables == true",dataTableOutput("c3table")),br(),
                                                                 conditionalPanel(condition = "input.showGraphs == true",plotlyOutput(width ="100%","c3count")),br(),
                                                                 conditionalPanel(condition = "input.showGraphs == true",plotlyOutput(width ="100%","c3perc"))),
                                                        
                                                        tabPanel("Distance",
                                                                 conditionalPanel(condition = "input.imperial== true & input.metric== false" ,sliderInput("distanceFromChicago_input",label=h3("Distance from Chicago:"), min=0, max=3125, value = c(0,3125,width="100%"))),br(),
                                                                 conditionalPanel(condition = "input.imperial== false & input.metric== true" ,sliderInput("distanceFromChicago_input",label=h3("Distance from Chicago:"), min=0, max=5000, value = c(0,5000,width="100%"))),br(),
                                                                 
                                                                 conditionalPanel(condition = "input.showTables == true",dataTableOutput("c4table")),
                                                                 br(),
                                                                 conditionalPanel(condition = "input.showGraphs == true",plotlyOutput(width ="100%","c4count")),br(),
                                                                 conditionalPanel(condition = "input.showGraphs == true",plotlyOutput(width ="100%","c4perc")))
                                                             ),
                                                      
                                                      navbarMenu("Damage-Analysis",
                                                      
                                                        tabPanel("Yearly Loss",
                                                               
                                                               conditionalPanel(condition = "input.showTables == true",dataTableOutput("c5table")),br(),
                                                               conditionalPanel(condition = "input.showGraphs == true",plotlyOutput(width ="100%","c5"))),
                                                      tabPanel("Monthly Loss",
                                                               
                                                               conditionalPanel(condition = "input.showTables == true",dataTableOutput("c6table")),br(),
                                                               conditionalPanel(condition = "input.showGraphs == true",plotlyOutput(width ="100%","c6"))),
                                                      tabPanel("Hourly Loss",
                                                               
                                                               conditionalPanel(condition = "input.showTables == true",dataTableOutput("c7table")),br(),
                                                               conditionalPanel(condition = "input.showGraphs == true",plotlyOutput(width ="100%","c7")))
#                                                      tabPanel("Counties",
#                                                               
#                                                               conditionalPanel(condition = "input.showTables == true",dataTableOutput("c8table")),br(),
#                                                               conditionalPanel(condition = "input.showGraphs == true",plotlyOutput(width ="100%","c8"))
#                                                               ),
#                                                      tabPanel("Top 10",
                                                               
 #                                                              leafletOutput("map_top10",width="750px",height="700px")
  #                                                             )
                                                      
                                                      )
                                                      
                                                      )
                                     )
                           )
                           
                  ),


              tabPanel(h2("The Worst Tornadoes"), 
                       box(title = "Choose a state for its worst tornadoes", 
                           solidHeader = TRUE, 
                           status = "primary", 
                           width = 9, 
                           height = 6,
                           selectInput("state_map", "", st,selected="WI"),
                         leafletOutput("map_top10",width="750px",height="700px")
                       )  ),
                 
                  
                  tabPanel(h2("Hover Map"), 
                           box(title = "Hover over county for details", 
                               solidHeader = TRUE, 
                               status = "primary", 
                               width = 9, 
                               height = 6, 
                               radioButtons("hover_map_option", 
                                            h3("On Hover Details:"),
                                            choices = list("View amount of Injuries, Fatalities and Losses" = 1,
                                              "View frequency of Magnitudes" = 2),
                                            selected = 2,
                                            inline=T,
                                            width="1000px"),
                               leafletOutput("Heat_Maps",width="1000px",height="700px")
                               )  ),
                  
                  
                  tabPanel(h2("Heat Map"), 
                           box(title = "Heat map by county", 
                               solidHeader = TRUE, 
                               status = "primary", 
                               width = 9, 
                               height = 6, 
                               radioButtons("heat_map_option",
                                            h3("Heat-Map Type:"),
                                            choices = list("Based on Fatalities " = 1, "Based on Injuries" = 2),
                                            selected = 2,
                                            inline=T,
                                            width="1000px"),
                               plotOutput("HeatMaps",width="500px",height="900px")
                           )  )
                  
                  
                  ),
  server = function(input, output) {
    
    tornadoesByMagnitudeByYear =  reactive({
      if (input$state_analysis!="All")
      {
        data %>%
          filter(state == input$state_analysis) %>%
          group_by(year, magnitude = fscale) %>%
          summarize(tornadoCount = n()) %>%
          group_by(year) %>%
          mutate(annualTornadoCount = sum(tornadoCount)) %>%
          mutate(`Percentage Tornadoes` = tornadoCount/annualTornadoCount) %>%
          data.table()
      }
      
      else
      {
        data %>%
          group_by(year, magnitude = fscale) %>%
          summarize(tornadoCount = n()) %>%
          group_by(year) %>%
          mutate(annualTornadoCount = sum(tornadoCount)) %>%
          mutate(`Percentage Tornadoes` = tornadoCount/annualTornadoCount) %>%
          data.table()
      }
    })
    
    output$c1table = renderDataTable(formatStyle(datatable(tornadoesByMagnitudeByYear() %>%
                                                             mutate(`Percentage Tornadoes` = percent(`Percentage Tornadoes`))),
                                                 color = "black",columns = T,fontSize='22px')
    )
    
    # nrow(data)
    # ncol(data)
    # dim(data)[1]
    # dim(data)[2]
    
    output$c1count = renderPlotly({    
      ggplot(tornadoesByMagnitudeByYear(), aes(x = factor(year), y = tornadoCount, fill = magnitude)) +
        geom_bar(stat = "identity", position = 'dodge') + theme_solarized(light = FALSE) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 0))
      ggplotly()
    })
    output$c1perc = renderPlotly({
      ggplot(tornadoesByMagnitudeByYear(), aes(x = factor(year), y = `Percentage Tornadoes`,  fill = magnitude)) + 
        geom_bar(stat = "identity", position = 'stack', color = "black") +theme_solarized(light = FALSE) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 0))+ scale_y_continuous(labels = percent) 
      ggplotly()
    }
    )
    # C2
    
    
    tornadoesByMagnitudeByMonth =  reactive({
      if (input$state_analysis!="All")
      {
        data %>%
          filter(state == input$state_analysis) %>%
          group_by(month, magnitude = fscale) %>%
          summarize(tornadoCount = n()) %>%
          group_by(month) %>%
          mutate(monthlyTornadoCount = sum(tornadoCount)) %>%
          mutate(`Percentage Tornadoes` = tornadoCount/monthlyTornadoCount) %>%
          data.table()
      }
      
      else
      {
        data %>%
          group_by(month, magnitude = fscale) %>%
          summarize(tornadoCount = n()) %>%
          group_by(month) %>%
          mutate(monthlyTornadoCount = sum(tornadoCount)) %>%
          mutate(`Percentage Tornadoes` = tornadoCount/monthlyTornadoCount) %>%
          data.table()
      }
    })
    
    output$c2table = renderDataTable(formatStyle(tornadoesByMagnitudeByMonth() %>%
                                                   mutate(`Percentage Tornadoes` = percent(`Percentage Tornadoes`)) %>%
                                                   datatable(),
                                                 color = "black",columns = T,fontSize='22px')
    )
    
    output$c2count = renderPlotly( {   
      ggplot(tornadoesByMagnitudeByMonth(), aes(x = factor(month), y = tornadoCount, fill = magnitude)) + 
        theme_solarized(light = FALSE) + 
        geom_bar(stat = "identity", position ='dodge', color = "black")
      ggplotly()
    }
    )
    output$c2perc = renderPlotly({
      ggplot(tornadoesByMagnitudeByMonth(), aes(x = factor(month), y = `Percentage Tornadoes`,  fill = magnitude)) + 
        geom_bar(stat = "identity", position = 'stack', color = "black") + 
        theme_solarized(light = FALSE) + 
        scale_y_continuous(labels = percent)
      ggplotly()
    }
    )
    
    
    # C3 
    tornadoesByMagnitudeByHour =   reactive({
      
      if (input$state_analysis!="All")
      {
        if (input$hr==1)
        {
          data %>%
            filter(state == input$state_analysis) %>%
            mutate(hour = substr(time, 1,2)) %>%
            group_by(hour, magnitude = fscale) %>%
            summarize(tornadoCount = n()) %>%
            group_by(hour) %>%
            mutate(hourlyTornadoCount = sum(tornadoCount)) %>%
            mutate(`Percentage Tornadoes` = tornadoCount/hourlyTornadoCount) %>%
            data.table()
        }
        
        else 
        {
          data %>%
            filter(state == input$state_analysis) %>%
            mutate(hour = paste(substr(format(strptime(time, format='%H:%M:%S'), '%r'),1,2) , substr(format(strptime(time, format='%H:%M:%S'), '%r'),10,12))  ) %>%    ######CHANGE
            group_by(hour, magnitude = fscale) %>%
            summarize(tornadoCount = n()) %>%
            group_by(hour) %>%
            mutate(hourlyTornadoCount = sum(tornadoCount)) %>%
            mutate(`Percentage Tornadoes` = tornadoCount/hourlyTornadoCount) %>%
            data.table()
        }
      }
      
      else
      {
        if (input$hr==1)
        {
          data %>%
            mutate(hour = substr(time, 1,2)) %>%
            group_by(hour, magnitude = fscale) %>%
            summarize(tornadoCount = n()) %>%
            group_by(hour) %>%
            mutate(hourlyTornadoCount = sum(tornadoCount)) %>%
            mutate(`Percentage Tornadoes` = tornadoCount/hourlyTornadoCount) %>%
            data.table()
        }
        
        else 
        {
          data %>%
            mutate(hour = paste(substr(format(strptime(time, format='%H:%M:%S'), '%r'),1,2) , substr(format(strptime(time, format='%H:%M:%S'), '%r'),10,12))  ) %>%    ######CHANGE
            group_by(hour, magnitude = fscale) %>%
            summarize(tornadoCount = n()) %>%
            group_by(hour) %>%
            mutate(hourlyTornadoCount = sum(tornadoCount)) %>%
            mutate(`Percentage Tornadoes` = tornadoCount/hourlyTornadoCount) %>%
            data.table()
        }
        
        
        
        
      }
    })
    
    
    output$c3table = renderDataTable(formatStyle(tornadoesByMagnitudeByHour() %>%
                                                   mutate(`Percentage Tornadoes` = percent(`Percentage Tornadoes`))%>%
                                                   datatable(),
                                                 color = "black",columns = T,fontSize='22px')
    )
    output$c3count = renderPlotly({
      ggplot(tornadoesByMagnitudeByHour(), aes(x = factor(hour), y = tornadoCount, fill = magnitude)) + 
        theme_solarized(light = FALSE) + 
        geom_bar(stat = "identity", position ='dodge',color = "black")
      ggplotly()
    }
    )
    output$c3perc = renderPlotly({
      ggplot(tornadoesByMagnitudeByHour(), aes(x = factor(hour), y = `Percentage Tornadoes`,  fill = magnitude)) + 
        geom_bar(stat = "identity", position = 'stack', color = "black")+ 
        theme_solarized(light = FALSE) + 
        scale_y_continuous(labels = percent)
      ggplotly()
    }
    )
    # C4
    stateCoordinates
    data = data %>% 
      merge(stateCoordinates[,.(state = StateCode,
                                stateLat = Latitude,
                                stateLon = Longitude)], by = "state", all.x = T) %>%
      mutate(distChicagoStrt = getGeoDist(startlat = 41.8781, startlon = -87.6298,
                                          endlat = startLat, endlon = startLon),
             distChicagoEnd = getGeoDist(startlat = 41.8781, startlon = -87.6298,
                                         endlat = endLat, endlon = endLon),
             distChicagoState = getGeoDist(startlat = 41.8781, startlon = -87.6298,
                                           endlat = stateLat, endlon = stateLon)) %>% 
      data.table()
    
    data$distChicago = apply(data[,.(distChicagoStrt, distChicagoEnd, distChicagoState)], MARGIN = 1, FUN = min)
    # output$maxDistPossible= renderText(max(data$distChicago))
    #   SlideMax = max(data$distChicago)
    #   SlideMin = min(data$distChicago)
    #   
    #   output$SliderWidget <- renderUI({
    #   sliderInput("Slider1","",min = SlideMin,max = SlideMax,value = c(1000))
    # })
    data = data %>%
      mutate(distChicagoBin = ifelse(distChicago<300,"< 300 km", ifelse(distChicago<1000,"300 - 1000 km", ifelse(distChicago < 2000, "1000 - 2000 km", "> 2000 km")))) %>%
      data.table()
    
    output$c4table = renderDataTable({
      min = input$distanceFromChicago_input[1]
      max = input$distanceFromChicago_input[2]
      formatStyle(data %>%
                    # filter(state == "IL") %>%
                    filter(distChicago >= min & distChicago <=max) %>%
                    # mutate(hour = substr(time, 1,2)) %>%
                    group_by(magnitude = fscale) %>%
                    summarize(tornadoCount = n()) %>%
                    ungroup() %>%
                    mutate(totalTornadoCount = sum(tornadoCount)) %>%
                    mutate(`Percentage Tornadoes` = percent(tornadoCount/totalTornadoCount))%>%
                    datatable(),
                  color = "black",columns = T,fontSize='22px')
      
    })
    
    output$c4count = renderPlotly({
      
      min = input$distanceFromChicago_input[1]
      max = input$distanceFromChicago_input[2]
      
      tornadoesByMagnitudeByDistance = data %>%
        # filter(state == "IL") %>%
        filter(distChicago >= min & distChicago <=max) %>%
        # mutate(hour = substr(time, 1,2)) %>%
        group_by(magnitude = fscale) %>%
        summarize(tornadoCount = n()) %>%
        ungroup() %>%
        mutate(totalTornadoCount = sum(tornadoCount)) %>%
        mutate(`Percentage Tornadoes` = tornadoCount/totalTornadoCount) %>%
        data.table()
      
      ggplot(tornadoesByMagnitudeByDistance, aes(x = magnitude, y = tornadoCount, fill = magnitude)) +
        geom_bar(color = "black",stat = "identity", position ='dodge') +
        theme_solarized(light = FALSE) 
      ggplotly()
    })
    
    output$c4perc = renderPlotly({
      min = input$distanceFromChicago_input[1]
      max = input$distanceFromChicago_input[2]
      tornadoesByMagnitudeByDistance = data %>%
        # filter(state == "IL") %>%
        filter(distChicago >= min & distChicago <=max) %>%
        # mutate(hour = substr(time, 1,2)) %>%
        group_by(magnitude = fscale) %>%
        summarize(tornadoCount = n()) %>%
        ungroup() %>%
        mutate(totalTornadoCount = sum(tornadoCount)) %>%
        mutate(`Percentage Tornadoes` = tornadoCount/totalTornadoCount) %>%
        data.table()
      
      ggplot(tornadoesByMagnitudeByDistance, aes(x = magnitude, y = `Percentage Tornadoes`,  fill = magnitude)) + 
        geom_bar(stat = "identity", color = "black") + 
        scale_y_continuous(labels = percent) +
        theme_solarized(light = FALSE)
      ggplotly()
    })
    
    
    # C5
    
    
    damagesByYear=  reactive({
      if (input$state_analysis!="All")
      {
        data %>%
          filter(state == input$state_analysis) %>%
          group_by(year) %>%
          summarize(damagesInjuries = sum(injuries, na.rm = T),
                    damagesFatalities = sum(fatalities,na.rm = T),
                    damagesLoss = sum(loss, na.rm = T)) %>%
          data.table()
      }
      
      else
      {
        data %>%
          group_by(year) %>%
          summarize(damagesInjuries = sum(injuries, na.rm = T),
                    damagesFatalities = sum(fatalities,na.rm = T),
                    damagesLoss = sum(loss, na.rm = T)) %>%
          data.table()
      }
    })
    
    
    output$c5table = renderDataTable(formatStyle(damagesByYear()%>%
                                                   datatable(),
                                                 color = "black",columns = T,fontSize='22px')
    )
    
    output$c5 = renderPlotly({
      selectedDamage = input$radioDamages
      if(selectedDamage == "Injuries"){
        ggplot(damagesByYear(), aes(x = year, y = damagesInjuries)) + 
          geom_bar(stat = "identity", fill = "steelblue", color = "black") +
          theme_solarized(light = FALSE) + 
          
          theme(axis.text.x = element_text(angle = 90, hjust = 0))
      } else if (selectedDamage == "Fatalities"){
        ggplot(damagesByYear(), aes(x = year, y = damagesFatalities)) + 
          geom_bar(stat = "identity",fill = "seagreen3", color = "black") +
          theme_solarized(light = FALSE) + 
          
          theme(axis.text.x = element_text(angle = 90, hjust = 0))
      } else if (selectedDamage == "Losses"){
        ggplot(damagesByYear(), aes(x = year, y = damagesLoss)) + 
          geom_bar(stat = "identity", fill = "tomato3", color = "black") +
          theme_solarized(light = FALSE) + 
          
          theme(axis.text.x = element_text(angle = 90, hjust = 0))
      }
      ggplotly()
    })
    
    # C6
    
    damagesByMonth =  reactive({
      if (input$state_analysis!="All")
      {
        
        data %>%filter(state == input$state_analysis) %>%
          group_by(month) %>%
          summarize(damagesInjuries = sum(injuries),
                    damagesFatalities = sum(fatalities),
                    damagesLoss = sum(loss)) %>%
          data.table()
      }
      
      else
      {
        data %>%
          group_by(month) %>%
          summarize(damagesInjuries = sum(injuries),
                    damagesFatalities = sum(fatalities),
                    damagesLoss = sum(loss)) %>%
          data.table()
      }
    })
    
    
    
    
    output$c6table = renderDataTable(formatStyle(damagesByMonth()%>%
                                                   datatable(),
                                                 color = "black",columns = T,fontSize='22px')
    )
    
    output$c6= renderPlotly({
      selectedDamage = input$radioDamages
      if(selectedDamage == "Injuries"){
        ggplot(damagesByMonth(), aes(x = month, y = damagesInjuries)) + 
          geom_bar(stat = "identity", fill = "steelblue", color = "black") +
          theme_solarized(light = FALSE) + 
          theme(axis.text.x = element_text(angle = 90, hjust = 0))
      } else if (selectedDamage == "Fatalities"){
        ggplot(damagesByMonth(), aes(x = month, y = damagesFatalities)) + 
          geom_bar(stat = "identity",fill = "seagreen3", color = "black") +
          theme_solarized(light = FALSE) + 
          theme(axis.text.x = element_text(angle = 90, hjust = 0))
      } else if (selectedDamage == "Losses"){
        ggplot(damagesByMonth(), aes(x = month, y = damagesLoss)) + 
          geom_bar(stat = "identity", fill = "tomato3", color = "black") +
          theme_solarized(light = FALSE) + 
          theme(axis.text.x = element_text(angle = 90, hjust = 0))
      }
      ggplotly()
    })
    
    # C7
    
    
    damagesByHour =   reactive({
      
      if (input$state_analysis!="All")
      {
        if (input$hr==1)
        {
          data %>%filter(state == input$state_analysis)%>% 
            mutate(hour = substr(time, 1,2)) %>%
            group_by(hour) %>%
            summarize(damagesInjuries = sum(injuries, na.rm = T),
                      damagesFatalities = sum(fatalities,na.rm = T),
                      damagesLoss = sum(loss, na.rm = T)) %>%
            data.table()
        }
        
        else 
        {
          
          data %>%filter(state == input$state_analysis) %>%
            mutate(hour = paste(substr(format(strptime(time, format='%H:%M:%S'), '%r'),1,2) , substr(format(strptime(time, format='%H:%M:%S'), '%r'),10,12))  ) %>%    ######CHANGE
            group_by(hour) %>%
            summarize(damagesInjuries = sum(injuries, na.rm = T),
                      damagesFatalities = sum(fatalities,na.rm = T),
                      damagesLoss = sum(loss, na.rm = T)) %>%
            data.table()
        }
      }
      
      
      else
      {
        if (input$hr==1)
        {
          data %>%
            mutate(hour = substr(time, 1,2)) %>%
            group_by(hour) %>%
            summarize(damagesInjuries = sum(injuries, na.rm = T),
                      damagesFatalities = sum(fatalities,na.rm = T),
                      damagesLoss = sum(loss, na.rm = T)) %>%
            data.table()
        }
        
        else 
        {
          
          data %>%
            mutate(hour = paste(substr(format(strptime(time, format='%H:%M:%S'), '%r'),1,2) , substr(format(strptime(time, format='%H:%M:%S'), '%r'),10,12))  ) %>%    ######CHANGE
            group_by(hour) %>%
            summarize(damagesInjuries = sum(injuries, na.rm = T),
                      damagesFatalities = sum(fatalities,na.rm = T),
                      damagesLoss = sum(loss, na.rm = T)) %>%
            data.table()
        }
      }
    })
    
    
    output$c7table = renderDataTable(formatStyle(damagesByHour()%>%
                                                   datatable(),
                                                 color = "black",columns = T,fontSize='22px')
    )
    
    output$c7 = renderPlotly({
      selectedDamage = input$radioDamages
      if(selectedDamage == "Injuries"){
        ggplot(damagesByHour(), aes(x = hour, y = damagesInjuries)) + 
          geom_bar(stat = "identity", fill = "steelblue", color = "black") +
          theme_solarized(light = FALSE) + 
          
          theme(axis.text.x = element_text(angle = 90, hjust = 0))
      } else if (selectedDamage == "Fatalities"){
        ggplot(damagesByHour(), aes(x = hour, y = damagesFatalities)) + 
          geom_bar(stat = "identity",fill = "seagreen3", color = "black") +
          theme_solarized(light = FALSE) + 
          
          theme(axis.text.x = element_text(angle = 90, hjust = 0))
      } else if (selectedDamage == "Losses"){
        ggplot(damagesByHour(), aes(x = hour, y = damagesLoss)) + 
          geom_bar(stat = "identity", fill = "tomato3", color = "black") +
          theme_solarized(light = FALSE) + 
          
          theme(axis.text.x = element_text(angle = 90, hjust = 0))
      }
      ggplotly()
    })
    
    # C8
    
    output$c8table <- renderDataTable(formatStyle(tornadoesByCounty%>%
                                                    datatable(),
                                                  color = "black",columns = T,fontSize='22px'))
    
    
    output$c8 = renderPlotly({
      selectedDamage = input$radioDamages
      tornadoesByCounty=read.csv( file = "Tornado_Dataset/tornadospercounty.csv",header=TRUE,colClasses=c("NULL", NA, NA))[0:20,]
      names(tornadoesByCounty)=c("County","#Tornados")
      
      g=ggplot(tornadoesByCounty, aes(x = tornadoesByCounty[[1]], y = tornadoesByCounty[[2]])) + 
        geom_bar(stat = "identity", fill = "steelblue", color = "black") +
        theme_solarized(light = FALSE) +
        theme(axis.text.x=element_text(angle = 90, hjust = 0))+   
        labs(x = "County",y = "Tornado Count")
      
      ggplotly(g)
    })
    
    
    # C9
    #Function maps tornados by year. Excludes missing coordinates
    map_track_state_year_reactive <- reactive({
      subset_data=states_data[State==input$state_select]
      leafletProxy('map_t')
      if(input$units == 1){
        m<-map_track_state_year(input$year_input, state_var1 = "IL", state_var2 = subset_data[,c("Abbreviation")], frange = input$fscale_input, wrange = input$width_input, 
                                lrange = input$length_input, irange = input$injuries_input, fatrange = input$fatalities_input,
                                map_markers = input$radio, lossrange = input$loss_input, units_set = units)
      }
      else{
        m<-map_track_state_year(input$year_input, state_var1 = "IL", state_var2 = subset_data[,c("Abbreviation")], frange = input$fscale_input, wrange = input$width_input, 
                                lrange = input$length_input, irange = input$injuries_input, fatrange = input$fatalities_input,
                                map_markers = input$radio, lossrange = input$loss_input, units_set  = units_metric)
      }
      m
    })
    
    output$map_track = renderLeaflet({
      map_track_state_year_reactive()
    })
    
    observeEvent(input$length_input, {
      leafletProxy("map_track")
    })
    
    #Function for B4
    output$map_top10 = renderLeaflet({
      map_track_top10(input$state_map)
    })
    
    ###Graduate HeatMap
    output$HeatMaps <- renderPlot({
      
      g1=ggplot(fatalities_df, aes(x=long, y=lat, group=group, fill=numbers))+ ggtitle("Fatalities per county") + 
        geom_polygon()+
        scale_fill_gradientn(
          colours=c("lightgreen","yellow","orange","darkorange" ,"red"),
          #  values=rescale(c(-3, -2, -1,0)),
          guide="colorbar"
        )+theme_void()+
        theme(
          plot.title = element_text(color="Yellow", size=20),
          legend.title = element_text(colour = 'white',size=15),
          legend.text=element_text(colour = 'white',size=15),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill="midnightblue"),
          plot.background = element_rect(fill = "midnightblue")
        )
      
      g2=ggplot(injuries_df, aes(x=long, y=lat, group=group, fill=numbers))+ ggtitle("Injuries per county") + 
        geom_polygon()+
        scale_fill_gradientn(
          colours=c("lightgreen","yellow","orange","darkorange" ,"red"),
          #  values=rescale(c(-3, -2, -1,0)),
          guide="colorbar"
        )+theme_void()+
        theme(
          plot.title = element_text(color="Yellow", size=20),
          legend.title = element_text(colour = 'white',size=15),
          legend.text=element_text(colour = 'white',size=15),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill="midnightblue"),
          plot.background = element_rect(fill = "midnightblue")
        )
      
      if(input$heat_map_option==1){g=g1}else{g=g2}
      g
      
      
    })
    
    
    
    output$Heat_Maps<-renderLeaflet(
      
      #  if(input$heat_map_option==1){
      #   ILall@data$hoverText= l1}else{ILall@data$hoverText=l2},
      #l=l1,
      leaflet() %>% 
        addTiles() %>% 
        addPolygons(data=ILall,
                    weight = 1, 
                    opacity = 0.5,
                    color = "blue",
                    fillOpacity = 0.1,
                    highlight = highlightOptions(
                      weight = 1,
                      color = "#666",
                      fillOpacity = 0.1,
                      bringToFront = TRUE),
                    label = if(input$hover_map_option==1){l1}else{l2},
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))
    )
    
    output$width_input<-renderUI(
      if(input$units==2)
        sliderInput("width_input",label=h3("Width (In Km):"), min=0, max=max(data[,c('width')])*1.6, value = c(0, max(data[,c('width')]))*1.6,width="100%")
      else
      {
        sliderInput("width_input",label=h3("Width (In miles):"), min=0, max=max(data[,c('width')]), value = c(0, max(data[,c('width')]))*1.6,width="100%")
      }
    )
    output$length_input<-renderUI(
      if(input$units==2)
        sliderInput("width_input",label=h3("Length(In kms):"), min=0, max=max(data[,c('length')])*1.6, value = c(0, max(data[,c('length')]))*1.6,width="100%")
      else
        sliderInput("width_input",label=h3("Length (In miles):"), min=0, max=max(data[,c('length')]), value = c(0, max(data[,c('width')]))*1.6,width="100%")
    )
    
    
    
    
  }
                           )
