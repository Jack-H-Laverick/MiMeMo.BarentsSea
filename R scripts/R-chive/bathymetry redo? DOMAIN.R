
# Create domain polygons without overlapping boundaries, helpful for plotting and estimating currents, 
# Slightly stringent when it comes to filtering data

#### Set up ####

setwd("~/Data/Bathymetry GEBCO")
rm(list=ls())                                                   
library(tidyverse)
library(egg)
library(sp)
library(raster)
library(sf)
library(lwgeom)
library(data.table)
library(rnaturalearth)
library(pbapply)
library(stars)

nc_bath <- readRDS("Polar_Bathymetry_halfres.rds")                          # Get bathymetry

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = 4326)                                                  # Assign polar projection

domains <- readRDS("~/Data/Bathymetry GEBCO/Domains.rds") %>%               # Load SF polygons of the MiMeMo model domains
  st_transform(4326)

#### Final Domain areas ####

close <- readRDS("bathymetry and distance from shore.rds") %>%              # Read in bathymetry points with distances
  st_transform(4326)  

limit <- st_crop(close, st_bbox(domains[c(3,4),])) %>%
  st_join(domains[3,])

#'limit$Region <- `levels<-`(addNA(limit$Region), c(levels(limit$Region), "Out"))  # Replace Region NAs with out of domain marker

limit <- mutate(limit, compartment)

Offshore <- filter(limit, between(Elevation, -400, -60) & Shore_dist > 20000) %>% mutate(Shore = "Off")
Inshore <- filter(limit, Elevation > -60 | Shore_dist < 20000) %>% mutate(Shore = "In")
High_seas <- filter(limit, Elevation < -400 & Shore_dist > 20000) %>% mutate(Shore = "High")

coded <- rbind(Offshore, Inshore, High_seas) %>%
  mutate(compartment = as.factor(paste(Region, Shore, sep = "_"))) %>%
  mutate(Shore = as.factor(Shore))

levels(coded$Shore) <- list("1" = "Off", "2" = "In", "3" = "High")
coded <- mutate(coded, Shore = as.numeric(Shore))

#levels(coded$compartment) <- list("1" = "Out_High", "2" = "Out_Off", "3" = "Out_In", 
#                                  "4" = "Barents Sea_High", "5" = "Barents Sea_Off", "6" = "Barents Sea_In")
#coded <- mutate(coded, compartment = as.numeric(compartment))

#need to convert to numeric

polygon <- st_rasterize(dplyr::select(coded, Shore), nx = 500, ny = 500) %>%
  st_as_sf(as_points = FALSE, merge = TRUE) %>%
  mutate(Shore = as.factor(Shore))

#levels(polygon$compartment) <- list("OOB" = c("1","2","3"), "Barents Sea_High" = "4", 
#                                    "Barents Sea_Off" = "5", "Barents Sea_In" = "6")

plot_crop <- st_bbox(polygon)

ggplot() + 
  geom_sf(data = polygon, aes(fill = Shore), colour = NA) +
  geom_sf(data=world) +
  coord_sf(xlim = c(plot_crop$xmin, plot_crop$xmax), ylim = c(plot_crop$ymin, plot_crop$ymax))
  









## Create full SF object 
#Region <- c("Greenland", "Greenland", "Barents Sea", "Barents Sea")    # Create a data object to label the SFC class geomoetry only objects
#Shore <- c("Offshore", "Inshore", "Offshore", "Inshore")
#classification <- data.frame(Region, Shore)
#domains <- st_sf(classification, c(GO, GI, BO, BI))                    # Create a full SF object by combining data to the geometries
#saveRDS(domains, "Domains.rds")
