##run 01 and 02 first
library(dplyr)
library(sf)
library(ggplot2)
library(patchwork)
library(rnaturalearth)



#load coordinate data
city_coord <- readRDS("data/urban_center_coordinates.rds")

# World boundaries
world <- ne_countries(scale = "medium",returnclass = "sf")

#world map
p_world <- ggplot() +
  geom_sf(data = world,fill = "grey94",colour = "grey70",linewidth = 0.2 ) +
  geom_sf(data = city_coord,colour = "#668B8B",size = 1.1,alpha = 0.8) +
  coord_sf(xlim = c(-180, 180),ylim = c(-60, 85),expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white",colour = NA ) )


p_world

# ggsave("out/FS1_global_city_map.pdf",p_world,width = 11,height = 6,device = cairo_pdf)


#just histogram and continent
p_hist_map<-(p_world|( PI_hist /  PI_continent))+
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 16),
    plot.tag.position = c(0, 1.0))
# ggsave("out/prop_I_2plot_map.pdf", p_hist_map, width = 14, height = 8)