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

evi_mmax_levels <- data.frame(level = 1:12,
                              month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
evi_mmax_palette <- read_csv(here::here("data/paleta_mmax.csv"))
evi_mmax_palette <- evi_mmax_palette %>% left_join(evi_mmax_levels, by = "level")




eft_palette <- read_csv(here::here("data/paleta_eft.csv"))
# Asign eft attributes 
# see an example from https://annakrystalli.me/intro-r-gis/raster.html 
eft_levels <- eft_palette %>% 
  dplyr::select(eft_id, eft_code, eft_color) %>% 
  as.data.frame()

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
      evi_mmax_sel <- ratify(evi_mmax[[capa]])
      mmax_rat <-levels(evi_mmax_sel)[[1]]
      mmax_rat <- mmax_rat %>% left_join(evi_mmax_levels, by = c("ID"="level"))
      levels(evi_mmax_sel) <- mmax_rat 
      
      r = evi_mmax_sel
    } else if (stringr::str_detect(capa, "EFTs_")){
      
      eft_sel = eft[[capa]]
      
      eft_sel <- ratify(eft_sel) 
      eft_rat <- levels(eft_sel)[[1]]
      eft_rat <- eft_rat %>% left_join(eft_levels, by=c("ID" = "eft_id"))
      levels(eft_sel) <- eft_rat
      
      eftFactor <- deratify(eft_sel, "eft_code") #?? 
      r = eft_sel 
      
    } else if (stringr::str_detect(capa, "EFT_Rarity_")) {
      r = eft_rarity[[capa]]
    } else if (stringr::str_detect(capa, "EFT_Richness_")) {
      r = eft_richness[[capa]]
    }
    
    return(r)
    
  })
  
  # # Dinamically create palettes 
  paleta <- reactive({
    if (input$selectVariable == "EVI_Mean_"){
      r <- rasterInput()
      pal <- colorNumeric(rainbow(10, rev = TRUE), 
                          domain=values(r), 
                          na.color = "transparent")
    } else if (input$selectVariable == "EVI_sSD_"){
      pal <- colorNumeric(rainbow(8, rev = TRUE), 
                          domain = values(rasterInput()), 
                          na.color = "transparent") 
    } else if (input$selectVariable == "EVI_DMAX_"){
      pal <- evi_mmax_palette$color
      
    } else if (input$selectVariable == "EFTs_"){
      pal <- colorFactor(eft_palette$eft_color, 
                         values(rasterInput()), 
                         na.color = "transparent")
    
    } else if (input$selectVariable == "EFT_Rarity_"){
      pal <- colorNumeric(rainbow(10, rev = TRUE), 
                                domain = NULL, 
                                na.color = "transparent")
    } else if (input$selectVariable == "EFT_Richness_"){
      pal <- colorNumeric(rainbow(10, rev = TRUE), 
                                domain = NULL, 
                                na.color = "transparent")
    }
    
    return(pal)
    
  })

  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(lat = 37.09, lng =-3.1, 10) %>% 
      addProviderTiles('Esri.WorldImagery') %>% 
      addRasterImage(rasterInput(), colors = paleta(), na.color = "transparent") 
    # %>% 
    #   addLegend(position = "bottomright", pal = paleta(), values = values(rasterInput()))
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {paste0(input$selectVariable, input$selectYear, ".tif")},
    
    
    content = function(file) {raster::writeRaster(rasterInput(), file, format="GTiff", 
                                                  overwrite=TRUE)}
  )
}


shinyApp(ui, server)


