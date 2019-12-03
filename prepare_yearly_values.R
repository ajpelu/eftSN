# Prepare stacks 

library("tidyverse")
library("here")
library("raster")
library("rasterVis")
library("lubridate")

prepareData <- function(path){
  require(raster)
  require(tidyverse)
  require(stringr)
  
  f <- list.files(path, full.names = TRUE)
  # Remove "forGoo..." 
  fs <- f[!grepl("forGoogleEarthVisualization", f)]
  
  s <- stack(fs)
  #Rename 
  new_names <- names(s) %>% modify(~str_sub(.,end = -23)) 
  names(s) <- new_names
  
  return(s)
  
}



# Average Series
## EVI mean  
evi_mean_y <- prepareData(here::here("data/v2//Ecosystem_Functional_Attributes/EVI_Mean/Yearly"))

## EVI sSD
evi_sSD_y <- prepareData(here::here("data/v2//Ecosystem_Functional_Attributes/EVI_sSD/Yearly"))

## EVI MMAX
evi_mmax_y <- prepareData(here::here("data/v2//Ecosystem_Functional_Attributes/EVI_DMAX/Yearly"))

## EFT
eft_y <- prepareData(here::here("data/v2//Ecosystem_Functional_Types/Yearly"))

## EFT Rarity 
eft_rarity_y <- prepareData(here::here("data/v2//Ecosystem_Functional_Diversity/EFT_Rarity/Yearly"))


## EFT Rarity 
eft_richness_y <- prepareData(here::here("data/v2//Ecosystem_Functional_Diversity/EFT_Richness/Yearly"))





PARA VIsualizar

Annual maps
=====================================
  
  ```{r}
#generate raster layers
rst1 = matrix(runif(400, 0, 1),20, 20)# for suitability layer
rst2 = matrix(rnorm(400, 800, 50), 20, 20)# for precipitation
rst3 = matrix(rnorm(400, 27, 2.5), 20, 20)# for mean temperature
rst4 = matrix(rnorm(400, 38, 1.5), 20, 20)# for max. temperature
rst5 = matrix(rnorm(400, 21, 0.5), 20, 20)# for min. temperature

suit1 = raster(rst1)
precip1 = raster(rst2)
mean.T = raster(rst3)
max.T = raster(rst4)
min.T = raster(rst5)

# stack the raster layers
s1 = stack(suit1, precip1, mean.T, max.T, min.T)

# create extent and assign projection
extent(s1) = c(68,82,23,38)
projection(s1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
```


Colum {.sidebar data-width=200} 
---------------------------------------- 
  
  ```{r}
selectInput(inputId = "selectVariable", 
            label = "Variable:",
            choices = c("suitability" = "layer.1", 
                        "l2" = "layer.2",
                        "l3" = "layer.3",
                        "l4" = "layer.4",
                        "l5" = "layer.5"),
            selected = "suitability"
)
```


Column {data-width=800}
-------------------------------------
  
  ```{r}

rasterInput <- reactive({
  switch(input$selectVariable, 
         "suitability" = "layer.1", 
         "l2" = "layer.2",
         "l3" = "layer.3",
         "l4" = "layer.4",
         "l5" = "layer.5")
  
  r <- s1[[input$selectVariable]] 
})

paleta <- reactive({
  pal <- colorNumeric("RdYlBu", values(rasterInput()),
                      na.color = "transparent")  
})


renderLeaflet({
  leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
    addRasterImage(rasterInput(), colors = paleta()) %>% 
    addLegend(position = "bottomright", pal = paleta(), values = values(rasterInput()))
})
```

 