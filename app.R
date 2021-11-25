library(shiny)
library(jsonlite)
library(leaflet)
library(DT)
library(stringr)
library(htmltools)
library(htmlwidgets)


sites <- fromJSON("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Locations?propertytype=LaboratoryMeasurement&usr=TrustedDemo&key=jvdn64df")
#write.csv(sites , 'c:/temp/sites.csv', row.names=F)
#sites <- read.csv('c:/temp/sites.csv', stringsAsFactors = F)


ui <- fluidPage(
  leafletOutput("siteMap", width = 800, height = 600),
  DTOutput('tbl')
)

server <- function(input, output) {
  
  RV <- reactiveValues()
  RV$currentSiteData=NULL
  
  output$tbl = renderDT( RV$currentSiteData, options = list(lengthChange = FALSE)  )
  
  output$siteMap <- renderLeaflet({
    lons <- sites$Longitude
    lats <- sites$Latitude
    sids <- paste0(sites$DataSet, '$', sites$Location_ID)
    
    lf <- leaflet() %>% clearMarkers() %>%
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
      setView(lng = 135, lat = -28, zoom = 4) %>%
      addLayersControl(overlayGroups =  c("Soil Points"),options = layersControlOptions(collapsed = T)) %>% 
      addMarkers(lng = lons, lat = lats, label =  sids, layerId=sids, clusterOptions = markerClusterOptions()) %>% 
      addMiniMap( toggleDisplay = TRUE) 
  })
  
  observe({
    proxy <- leafletProxy("siteMap")
    #proxy %>% leaflet::addRasterImage(raster(rs), group = 'COG')
  })
  
  observe({
    clickm <-input$siteMap_marker_click
    if(is.null(clickm))
      return()
    
    sid <- clickm$id
    print(sid)
  
    bits <- str_split(sid, '[$]')
    print(bits)
    url <- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Site_Data?DataSet=", bits[[1]][1], '&siteid=',bits[[1]][2] ,"&propertytype=LaboratoryMeasurement&tabletype=wide&usr=ross.searle@csiro.au&key=a")
    print(url)
    sdf <- fromJSON(url)
    print(sdf)
    RV$currentSiteData <- sdf
    
  })
   
}


shinyApp(ui = ui, server = server)
