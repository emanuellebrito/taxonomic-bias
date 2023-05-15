library(sf)             # spatial data classes
library(rnaturalearth)  # world map data
library(readxl)         # reading excel files
library(dplyr)          # data manipulation
library(tidyr)          # data manipulation
library(purrr)          # data manipulation
library(cartogram)      # cartograms creation
library(tmap)           # maps creation
library(ggplot2)        # maps improvement
library(tidyverse)

#read data
setwd("D:/Documentos/Doutorado Ecologia/Tese - dados/test.map")

#ler coordenadas
coor = read.csv('test-coor2.csv', h = T, sep = ";")
summary(coor)
attach(coor)

#ler países
count = read.csv('countries.csv', h = T, sep = ";")
summary(count)
attach(count)

world <- map_data("world")
world

#map with locations
tiff('cap2_map2.tif', w=14000, h=8000, units="px", res=600, compression = "lzw")
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "#DDDDDD", size = 0.1
  ) +
  geom_point(
    data = coor,
    aes(lon, lat, color = use_frequency),
    alpha = 0.9, size = 3
  ) +
  scale_colour_gradient(low = "skyblue3", high = "black", name = "Network use frequency",
                        guide = guide_colourbar(direction = "horizontal", 
                                                barheight = unit(4, units = "mm"),
                                                barwidth = unit(70, units = "mm"),
                                                draw.ulim = F,
                                                title.hjust = 0.5,
                                                label.hjust = 0.5,
                                                title.position = "top")) +
  labs(x = "Longitude", y = "Latitude")+
  theme_classic()+
  theme(legend.position = "bottom",
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20, face = "bold"))
dev.off()




