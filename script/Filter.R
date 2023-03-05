library(tidyverse)
library(sf)
library(terra)
library(RColorBrewer)
library(ggtext)
library(ggnewscale)

setwd('~/Documents/ManuSCript/script')

# first we will create the shape of the filter itself. We will turn into an 
# explicitly spatial object using the 'sf' package. We will do this so that we
# are able to color the inside of the Filter shape with a gradient of colors, 
# which is defined within a geom_raster, created by the Terra package. 

filt <- st_polygon( 
    list(
      rbind(
        c(43, 1), c(57, 15), # bottom portion right
        c(57, 60), # stem portion right
        c(85, 100), c(15, 100), # very top of filter
        c(43, 60), # stem portion left
        c(43, 1)  # bottom portion left
      )
    )
  )  %>% 
  st_geometry()  %>% 
  st_as_sf(crs = 26913) %>% 
  mutate(Attribute = 'Filter') %>% 
  rename('geometry' = x) %>% 
  st_as_sf()  
  
# we create an empty raster the size and extent of the filter, and mask it
# to the shape to keep only pixels within the lines in; we are drawing between 
# the lines! FINALLY after all these years!!

filt_v <-   vect(filt)
r <- rast(filt_v, nrow = 1000, ncol = 1000)
r <- setValues(r, 1:1000000)
r <- crop(r, filt_v, mask = T)
r.df <- as.data.frame(r, xy = T)
rm(r, filt_v)

## we create labels and place these at positions to represent how many species 
# exist at each stage of the filter 'head' (...the top part!!!) 

filt_text <- data.frame(
  'Value' = c(
    'Angiosperms 353: 14,000+', 'Floristic Unit: 3226', 'Insect Pollinated: 2721', 
    'Spatial Search: 501', 'SDM: 426', 'Survey: 117' ),
  'Approach' = c(rep('Coarse', times = 3), 
                 rep('Computational', times = 2), 
                 rep('Survey', times = 1)),
  y = c(96, 90, 84, 78, 72, 66),
  x = c(50, 50, 50, 50, 50, 50)
) 

# we create this label seperately so that we can readily rotate it 90 degrees. 
time <- data.frame(
  'Value' = 'temporal gradient',
  'Approach' = 'Computational/Survey',
  x = 50, 
  y = 35
)

# define our colors, they run counter to the background colors... 
myco <- setNames(c("#A50F15", "#CB181D",  "#EF3B2C", "#FB6A4A"),
         c("Computational/Survey", "Survey", "Computational",'Coarse') )

# created a base plot due to the second scale label being weird, probably 
# unnecessary after some code re-factoring, but too scared to investigate. 
base <- ggplot() +
  geom_raster(data = r.df, aes(x = x, y = y, fill = lyr.1)) + 
  scale_fill_distiller(palette = 'YlGn', direction = -1) +
  guides(fill = 'none') +
  theme_void() 

# draw it. 
base +
  ggnewscale::new_scale_fill() +
  geom_label(data = filt_text, lwd = 4, 
             aes(x = x, y = y, label = Value, fill = Approach)) +
  geom_richtext(data = time, angle = 90,  lwd = 4, 
            aes(x = x, y = y, label = Value, fill = Approach)) +
  geom_label(show_guide  = FALSE) +
  lims(x = c(0,100)) +
  scale_fill_manual('Approach', values = myco,
                    breaks = c("Coarse", "Computational",
                               "Survey", "Computational/Survey")) + 
  theme(legend.position = c(0.25, 0.45), 
        plot.title = element_text(hjust = 0.5)) + 
  guides(fill = guide_legend(override.aes = aes(label = "")) ) + 
  labs(title = 
      'Filtering Species by Geography and Ecology\nand stratifying over a temporal gradient') +
  
  
  # add some lines on to the emphasize the 'Approach' groups
  geom_segment(aes(x = 25, y = 81, xend = 75, yend = 81), lty = 2, color = 'white') +
  geom_segment(aes(x = 35, y = 69, xend = 65, yend = 69), lty = 2, color = 'white') +
  geom_segment(aes(x = 40, y = 60, xend = 60, yend = 60), lty = 2, color = 'white')


ggsave('../graphics/plots/filter_diagram.png')

rm(base, filt, filt_text, r.df, time, myco)
