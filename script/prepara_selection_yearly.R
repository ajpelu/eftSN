library(shiny)
library(leaflet)
library(raster)
library(tidyverse)

# Read rasters
evi_mean <- raster::stack(here::here("data/v2/yearly/evi_mean.grd"))
evi_ssd <- raster::stack(here::here("data/v2/yearly/evi_ssd.grd"))
evi_mmax <- raster::stack(here::here("data/v2/yearly/evi_mmax.grd"))
eft <- raster::stack(here::here("data/v2/yearly/eft.grd"))
eft_rarity <- raster::stack(here::here("data/v2/yearly/eft_rarity.grd"))
eft_richness <- raster::stack(here::here("data/v2/yearly/eft_richness.grd"))

  ui <- fluidPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    titlePanel('test raster layers'),
    #leafletOutput("map", width = "100%", height = "100%"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "selectVariable", 
                    label = "Variable:",
                    choices = c("EVI Mean" = "EVI_Mean_", 
                                "EVI sSD" = "EVI_sSD_",
                                "EVI DMAX" = "EVI_DMAX_",
                                "EFTs" = "EFTs_",
                                "EFT Rarity" = "EFT_Rarity_",
                                "EFT Richness" = "EFT_Richness_"), 
                    selected = "EVI Mean"
                    ),
      
      selectInput(inputId = "selectYear", 
                  label = "Year:",
                  choices = 2001:2018, 
                  selected = 2001), 
      
      downloadButton("downloadData", "Download data")),
      
      
      mainPanel(leafletOutput("map", width = "100%"))
    )
  )

#shiny server
server <- function(input, output, session){
  
  rasterInput <- reactive({
    capa <- paste0(input$selectVariable, input$selectYear)

    if (stringr::str_detect(capa, "EVI_Mean_")){
      r = evi_mean[[capa]]
    } else if (stringr::str_detect(capa, "EVI_sSD_")){
      r = evi_ssd[[capa]]
    } else if (stringr::str_detect(capa, "EVI_DMAX_")){
      r = evi_mmax[[capa]]
    } else if (stringr::str_detect(capa, "EFTs_")){
      r = eft[[capa]]
    } else if (stringr::str_detect(capa, "EFT_Rarity_")) {
      r = eft_rarity[[capa]]
    } else if (stringr::str_detect(capa, "EFT_Richness_")) {
      r = eft_richness[[capa]]
    }
    return(r)

  })
  
# # Dinamically create palettes 
#   paleta <- reactive({
#     
#     # if en el nombre está esta variable ---> esta paleta 
#     
#     input$selectVariable
#     
#     c("EVI Mean" = "EVI_Mean_", 
#       "EVI sSD" = "EVI_sSD_",
#       "EVI DMAX" = "EVI_DMAX_",
#       "EFTs" = "EFTs_",
#       "EFT Rarity" = "EFT_Rarity_",
#       "EFT Richness" = "EFT_Richness_"), 
#     
#     pal <- colorNumeric("RdYlBu", values(rasterInput()),
#                         na.color = "transparent")
#   })


  output$map <- renderLeaflet({


    leaflet() %>% 
      # fitBounds(myext@xmin, myext@ymin, myext@xmax, myext@ymax) %>% 
      setView(lat = 37.09, lng =-3.1, 10) %>% 
      addProviderTiles('Esri.WorldImagery') %>% 
      addRasterImage(rasterInput()) 
      # 
      #                , colors = paleta()) %>% 
      # addLegend(position = "bottomright", pal = paleta(), values = values(layerInput()))
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {paste0(input$selectVariable, input$selectYear, ".tif")},
    
    
    content = function(file) {writeRaster(layerInput(), file, format="GTiff", 
                                          overwrite=TRUE)}
  )
}


shinyApp(ui, server)



