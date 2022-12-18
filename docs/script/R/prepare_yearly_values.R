# Prepare stacks 
library("tidyverse")
library("here")
library("raster")
library("terra")
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
evi_mean_y <- evi_mean_y / 10000
raster::writeRaster(evi_mean_y,
                    here::here("data/v2/yearly/evi_mean.grd"), format = "raster", overwrite=TRUE)

## EVI sSD
evi_sSD_y <- prepareData(here::here("data/v2//Ecosystem_Functional_Attributes/EVI_sSD/Yearly"))
evi_sSD_y <- evi_sSD_y / 10000
raster::writeRaster(evi_sSD_y, here::here("data/v2/yearly/evi_ssd.grd"), format = "raster", overwrite=TRUE)

## EVI MMAX
evi_mmax_y <- prepareData(here::here("data/v2//Ecosystem_Functional_Attributes/EVI_DMAX/Yearly")) %>% 
  raster::writeRaster(here::here("data/v2/yearly/evi_mmax.grd"), format = "raster", overwrite=TRUE)

## EFT
eft_y <- prepareData(here::here("data/v2//Ecosystem_Functional_Types/Yearly")) %>% 
  raster::writeRaster(here::here("data/v2/yearly/eft.grd"), format = "raster", overwrite=TRUE)

## EFT Rarity 
eft_rarity_y <- prepareData(here::here("data/v2//Ecosystem_Functional_Diversity/EFT_Rarity/Yearly")) %>% 
  raster::writeRaster(here::here("data/v2/yearly/eft_rarity.grd"), format = "raster", overwrite=TRUE)


## EFT Rarity 
eft_richness_y <- prepareData(here::here("data/v2//Ecosystem_Functional_Diversity/EFT_Richness/Yearly")) %>% 
  raster::writeRaster(here::here("data/v2/yearly/eft_richness.grd"), format = "raster", overwrite=TRUE)


# # Stacking and save 
# evi_yearly_stack <- raster::stack(evi_mean_y, 
#                    evi_mmax_y, 
#                    evi_sSD_y)
# 
# eft_yearly_stack <- raster::stack(eft_y,eft_rarity_y,
#                    eft_richness_y)
# 
# 
# writeRaster(evi_yearly_stack,
#             here::here("data/v2/evi_yearly_stack.grd"), format="raster")
#             
# writeRaster(eft_yearly_stack,
#             here::here("data/v2/eft_yearly_stack.grd"), format="raster")

