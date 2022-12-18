
library("tidyverse")
library("here")
library("raster")
library("rasterVis")
library("tmap")
library("mapview")
library("lubridate")
library("leaflet")
library("leaflet.extras")
library("leaflet.opacity")
library("RColorBrewer")
library("leaflet.esri")
library("shiny")
library("leafem")
library("htmlwidgets")


# Read rasters
evi_mean <- raster::stack(here::here("data/v2/yearly/evi_mean.grd"))
evi_ssd <- raster::stack(here::here("data/v2/yearly/evi_ssd.grd"))
evi_mmax <- raster::stack(here::here("data/v2/yearly/evi_mmax.grd"))
eft <- raster::stack(here::here("data/v2/yearly/eft.grd"))
eft_rarity <- raster::stack(here::here("data/v2/yearly/eft_rarity.grd"))
eft_richness <- raster::stack(here::here("data/v2/yearly/eft_richness.grd"))

# Auxiliar files for palettes
evi_mmax_levels <- data.frame(
  level = 1:12,
  month = c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
)
evi_mmax_palette <- read_csv(here::here("data/paleta_mmax.csv"))
evi_mmax_palette <- evi_mmax_palette %>% left_join(evi_mmax_levels, by = "level")

eft_palette <- read_csv(here::here("data/paleta_eft.csv"))
# Asign eft attributes
# see an example from https://annakrystalli.me/intro-r-gis/raster.html
eft_levels <- eft_palette %>%
  dplyr::select(eft_id, eft_code, eft_color) %>%
  as.data.frame()

shinyApp(
  ui = navbarPage(
    id = "nav",
    div(
      class = "outer",
      
      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      
      tags$head(
        # Include our custom CSS
        # includeCSS(here::here("script/styles.css"))),
        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        leafletOutput("map", width = "100%", height = "100%"),
        
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE,
          top = 30, left = 40, right = "auto", bottom = "auto",
          width = 200, height = "auto",
          
          h4("Select the raster"),
          
          selectInput(
            inputId = "selectVariable",
            label = "Variable:",
            choices = c(
              "EVI Mean" = "EVI_Mean_",
              "EVI sSD" = "EVI_sSD_",
              "EVI DMAX" = "EVI_DMAX_",
              "EFTs" = "EFTs_",
              "EFT Rarity" = "EFT_Rarity_",
              "EFT Richness" = "EFT_Richness_"
            ),
            selected = "EVI Mean"
          ),
          
          selectInput(
            inputId = "selectYear",
            label = "Year:",
            choices = 2001:2018,
            selected = 2001
          ),
          
          downloadButton("downloadData", "Download data")
        )
      )
    )
  ),
  
  server = function(input, output, session) {
    
    rasterSelected <- reactive({
      
      # Generate layer name
      capa <- paste0(input$selectVariable, input$selectYear)
      
      output <- list()
      
      # Select raster, palette and title
      if (stringr::str_detect(capa, "EVI_Mean_")) {
        r <- evi_mean[[capa]]
        paleta <- colorNumeric(rainbow(10, rev = TRUE),
                               domain = getValues(r),
                               na.color = "transparent"
        )
        title <- "EVI mean"
        etiquetas <- NULL
      } else if (stringr::str_detect(capa, "EVI_sSD_")) {
        r <- evi_ssd[[capa]]
        paleta <- colorNumeric(rainbow(8, rev = TRUE),
                               domain = getValues(r),
                               na.color = "transparent"
        )
        title <- "EVI SD"
        etiquetas <- NULL
      } else if (stringr::str_detect(capa, "EVI_DMAX_")) {
        # Prepare raster
        r <- ratify(evi_mmax[[capa]])
        mmax_rat <- levels(r)[[1]]
        mmax_rat <- mmax_rat %>%
          left_join(evi_mmax_levels, by = c("ID" = "level"))
        levels(r) <- mmax_rat
        r[r == 0] <- NA
        
        #  paleta <- evi_mmax_palette$color
        paleta <- colorFactor(
          palette = evi_mmax_palette$color,
          domain = NULL,
          na.color = "#00000000"
        )
        
        etiquetas <- NULL
        title <- "EVI DMAX (month)"
      } else if (stringr::str_detect(capa, "EFTs_")) {
        r_aux <- eft[[capa]]
        r_aux <- ratify(r_aux)
        eft_rat <- levels(r_aux)[[1]]
        eft_rat <- eft_rat %>% left_join(eft_levels, by = c("ID" = "eft_id"))
        levels(r_aux) <- eft_rat
        r_aux[r_aux == 0] <- NA
        
        eftFactor <- deratify(r_aux, "eft_code")
        r <- eftFactor
        
        title <- "EFT"
        
        paleta <- colorFactor(
          palette = eft_palette$eft_color,
          domain = NULL,
          na.color = "#00000000"
        )
        
        etiquetas <- 1
      } else if (stringr::str_detect(capa, "EFT_Rarity_")) {
        r <- eft_rarity[[capa]]
        
        paleta <- colorNumeric(rainbow(10, rev = TRUE),
                               domain = NULL,
                               na.color = "transparent"
        )
        title <- "EFT rarity"
        etiquetas <- NULL
      } else if (stringr::str_detect(capa, "EFT_Richness_")) {
        r <- eft_richness[[capa]]
        
        paleta <- colorNumeric(rainbow(10, rev = TRUE),
                               domain = NULL,
                               na.color = "transparent"
        )
        title <- "EFT richness"
        etiquetas <- NULL
      }
      
      
      output$r <- r
      output$paleta <- paleta
      output$title <- title
      output$etiquetas <- etiquetas
      output$capa_out
      
      output
    })
    
    output$map <- renderLeaflet({
      basemap <- leaflet() %>%
        setView(lat = 37.09, lng = -3.1, 10) %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addRasterImage(rasterSelected()$r, colors = rasterSelected()$paleta) %>%
        addLegend(
          title = rasterSelected()$title,
          pal = rasterSelected()$paleta,
          values = getValues(rasterSelected()$r),
          position = "bottomright",
          labFormat =
            labelFormat(
              if (!is.null(rasterSelected()$etiquetas)) {
                if (rasterSelected()$etiquetas == 1) {
                  eft_palette$eft_nameLegend
                }
              }
            )
        )
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(input$selectVariable, input$selectYear, ".tif")
      },
      
      
      content = function(file) {
        raster::writeRaster(rasterSelected()$r, file,
                            format = "GTiff",
                            overwrite = TRUE
        )
      }
    )
  }
)

