library(tidyverse)
library(ggraph)
library(igraph)

edges <- flare$edges
vertices <- flare$vertices

mygraph <- graph_from_data_frame( edges, vertices=vertices )

ggraph(mygraph, layout = 'circlepack') + 
  geom_node_circle() +
  theme_void()

edges1 <- data.frame(
  from =  c('A353', 'A353.RPTL', 'A353.RPTL.SF', 
            'A353-leaf', 'A353.RPTL-leaf','A353.RPTL.SF-leaf'),
  to = c('A353.RPTL', 'A353.RPTL.SF', 'A353.RPTL.SF.TF', 
         'A353.RPTL-leaf','A353.RPTL.SF-leaf', 'A353.RPTL.SF.TF-leaf') 
)

vertices1 <- data.frame(
  name = 
    c('A353-leaf', 'A353.RPTL-leaf','A353.RPTL.SF-leaf', 'A353.RPTL.SF.TF-leaf', 
      'A353', 'A353.RPTL','A353.RPTL.SF', 'A353.RPTL.SF.TF'),
  size = c(14000, 1295, 425, 346, 0, 0, 0, 0), 
  shortName = rep(c('Angiosperms 353', 'Regional Plant Taxa List',
                'Spatial Filtering', 'Temporal Filtering'), 2)
#  Process = c('Of the 350,000+ known plant species, A353 data exist for 14,000+', 
#              'Occurrence data create a list of candidate taxa likely to occur in the study area', 
#              'Spatial filtering of Occurrence data retains species likely to be present in study sites', 
#              'Temporal filtering of taxa via modelling informs metabarcoding results at multiple time points'
#  )
)

mygraph1 <- graph_from_data_frame( edges1, vertices=vertices1 )
ggraph(mygraph1, layout = 'circlepack') + 
  geom_node_circle() +
  theme_void()
