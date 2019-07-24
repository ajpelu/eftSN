# Load Raster 

library("raster")
library("tmap")
library("mapview")
library("here")



# # Get names of rasters
# names_raster <- list.files(path = here::here("data"), pattern = '*tif', 
#                            all.files = TRUE, full.names = TRUE)



disimilitud <- raster(here::here("/data/Disimilud_EFTs_~1840m_2001-2016.tif"))
divEspa <- raster(here::here("/data/DivEspaMediaEFTs_1840m_2001-2016_.tif"))

evi_mean <- raster(here::here("data/EVI_medio_230m_2001-2016.tif"))
evi_mmax<- raster(here::here("data/EVI_MMAX_230m_2001-2016.tif"))
evi_cv <- raster(here::here("data/EVI_sCV_230m_2001-2016.tif"))

eft <- raster(here::here("/data/EFTs_230m_2001-2016.tif"))
eft_rareza <- raster(here::here("data/Rareza_230m_2001-2016.tif"))
eft_var_inter <- raster(here::here("data/Variabilidad_interanual_230m_2001-2016.tif"))




# EVI 
# Sovle small variations of extent
ext <- extent(evi_mean)
evi_cv <- setExtent(evi_cv, ext, keepres = FALSE)
evi_mmax <- setExtent(evi_mmax, ext, keepres = FALSE)


# Create stack 
evi_stack <- stack(evi_mean,  evi_mmax, evi_cv)
pixel <- rasterToPolygons(evi_stack)

names(pixel) <- c("mean_EVI", "mMax_EVI", "sCV_EVI")


head(pixel)


# Ver esto 
# http://shiny.rstudio.com/gallery/superzip-example.html 




data(land)
mapita <- tm_shape(land) + tm_raster(eft, palette=terrain.colors(64)) + 
  tm_format("World") 

+ tm_format("World") + tm_shape(eft)



map_evi <- tm_shape(pixel) + 
  tm_format("World") + 
  tm_style("gray") +
  tm_raster(eft) 

#   tm_polygons(alpha = 0)
tmap_mode("view")
mapita

