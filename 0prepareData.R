# Load Raster 

library("raster")
library("tmap")
library("mapview")
library("here")
library("tidyverse")
library("lubridate")


# Read raster 
# evi_mean <- raster(here::here("data/EVI_medio_230m_2001-2016.tif"))

evi_mean <- raster(here::here("data/EVI_medio_230m_2001-2016_v2.tif"))
evi_mmax<- raster(here::here("data/EVI_MMAX_230m_2001-2016.tif"))
evi_cv <- raster(here::here("data/EVI_sCV_230m_2001-2016.tif"))
eft <- raster(here::here("/data/EFTs_230m_2001-2016.tif"))
rareza <- raster(here::here("data/Rareza_230m_2001-2016.tif"))


# Solve small variations of extent
evi_mmax <- projectRaster(evi_mmax, evi_mean)
evi_cv <- projectRaster(evi_cv, evi_mean)
eft <- projectRaster(eft, evi_mean)

# Units 
evi_mean <-evi_mean / 10000
evi_cv <- evi_cv / 100
eft <- round(eft)
evi_mmax <- round(evi_mmax)



# Add months to evi_mmax
evi_mmax_levels <- data.frame(level = 1:12, 
                              month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
evi_mmax <- ratify(evi_mmax)
mmax_rat <-levels(evi_mmax)[[1]]
mmax_rat <- mmax_rat %>% left_join(evi_mmax_levels, by = c("ID"="level"))
levels(evi_mmax) <- mmax_rat 



# eft attributes 
# https://rdrr.io/cran/raster/man/factor.html
eft_palette <- read_csv(here::here("data/paleta_eft.csv"))

# Asign eft attributes 
# see an example from https://annakrystalli.me/intro-r-gis/raster.html 
eft_levels <- eft_palette %>% dplyr::select(eft_id, eft_code, eft_color) %>% as.data.frame()

eft <- ratify(eft) 
eft_rat <- levels(eft)[[1]]
eft_rat <- eft_rat %>% left_join(eft_levels, by=c("ID" = "eft_id"))
levels(eft) <- eft_rat


eftFactor <- deratify(eft, "eft_code")


# Rename layers 
names(evi_mean) <- "EVI_Mean"
names(evi_mmax) <- "EVI_DateMax"
names(evi_cv) <- "EVI_sCV"
names(eft) <- "EFT"
names(eftFactor) <- "EFT"



# Create stack 
evi_stack <- stack(eft, evi_mean, evi_mmax, evi_cv, RAT = TRUE)

pixel <- rasterToPolygons(evi_stack)
pixel <- merge(pixel, eft_palette[,c("eft_code", "eft_id")], by.x = "EFT", by.y = "eft_id")
pixel@data <- pixel@data[,-1]
names(pixel)[4] <- "EFT"
pixel <- pixel[,c(4,1:3)]
pixel$EVI_DateMaxMonth<- as.character(month(ymd(010101) + months(pixel$EVI_DateMax-1), label=TRUE, abbr = TRUE))
pixel@data <- pixel@data[,-3]
names(pixel)[4] <- "EVI_monthMax"




#option # 2 points
puntos <- as.data.frame(rasterToPoints(evi_stack))


puntos <- puntos %>% 
  mutate(EVI_monthMax = as.character(month(ymd(010101) + months(puntos$EVI_DateMax-1), label=TRUE, abbr = TRUE))) %>% 
  rename(lat = y, long = x) %>% 
  inner_join(eft_palette %>% dplyr::select(eft_id, eft_code, eft_color), c("EFT" = "eft_id"))
  

puntitos <- SpatialPointsDataFrame(puntos[, c("long", "lat")], 
                                   puntos[, c("eft_code", "EVI_Mean", "EVI_monthMax", "EVI_sCV", "eft_color")])










#### Visualization
tmap_mode("view")

mapita <- tm_shape(eftFactor, name = "EFT") + 
  tm_raster(palette = eft_palette$eft_color,
            alpha = 0.5,
            legend.is.portrait = FALSE) + 
  tmap_options(max.categories = 64) +
  tm_shape(pixel) + tm_polygons(labels = NULL, 
                                popup.vars = c("Productivity (EVI mean)" = "EVI_Mean",
                                               "Seasonality (EVI scV)" = "EVI_sCV", 
                                               "Phenology (Month max EVI)" = "EVI_monthMax"), 
                                alpha=.01, border.col = NA, border.alpha = 0.001) 




+ 
  tm_shape(pixel) + tm_polygons(alpha=.01, border.col = NA, border.alpha = 0.01) +
  tm_view(view.legend.position = c("rigth", "bottom"))
  
mapita_p <- tm_shape(pixel) + tm_polygons(col = eft_palette$eft_color) +
  tmap_options(max.categories = 64) 


  
  
  
  
  tm_polygons(alpha=.01, border.col = NA, border.alpha = 0.01) 
tmap_save(mapita, here::here("html/eft_map_v1.html"))








palette = "eft_color") 
mapita



eft2 <- eft 


eft3 <- setValues(raster(eft), as.factor(eft_palette$eft_code))














# +
  tm_shape(pixel) + tm_polygons(alpha = 0)  

mapita


data(land)
mapita <- tm_shape(land) + tm_raster(eft, palette=terrain.colors(64)) + 
  tm_format("World") 

+ tm_format("World") + tm_shape(eft)



map_evi <- tm_shape(pixel) + 
  tm_format("World") + 
  tm_style("gray") +
  tm_raster(eft) 

#   tm_polygons(alpha = 0)







# # Get names of rasters
# names_raster <- list.files(path = here::here("data"), pattern = '*tif', 
#                            all.files = TRUE, full.names = TRUE)



disimilitud <- raster(here::here("/data/Disimilud_EFTs_1840m_2001-2016.tif"))
divEspa <- raster(here::here("/data/DivEspaMediaEFTs_1840m_2001-2016_.tif"))





eft_rareza <- raster(here::here("data/Rareza_230m_2001-2016.tif"))
eft_var_inter <- raster(here::here("data/Variabilidad_interanual_230m_2001-2016.tif"))

disimilitud1 <- raster(here::here("/data/disimilitud.tif"))

















names(pixel) <- c("mean_EVI", "mMax_EVI", "sCV_EVI")


head(pixel)


ra <- evi_mean

variable <- ra@data@names


names_raster <- list.files(path = here::here("data"), pattern = '*tif',
                           all.files = TRUE, full.names = TRUE)

info_rasters <- data.frame() 

for (i in names_raster){ 
  ra <- raster(i)
  
  aux <- data.frame(
    variable = ra@data@names,
    n_cells = ncell(ra), 
    n_col = ncol(ra), n_row = nrow(ra),
    res_x = res(ra)[1], res_y = res(ra)[2], 
    xmin = xmin(ra), xmax= xmax(ra), ymin = ymin(ra), ymax = ymax(ra),
    crs = ra@crs@projargs, 
    min_value = minValue(ra), max_value = maxValue(ra)
  )
  
  info_rasters <- rbind(info_rasters, aux)
  
  }
















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

