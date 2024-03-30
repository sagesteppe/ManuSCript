setwd("~/Documents/ManuSCript/script")

library(tidyverse)

commonCrestedButteBees <- c('nevadensis', 'californicus', 'appositus', 'flavifrons', 'rufocinctus', 'mixtus', 'bifarius')

dat <- read.csv('../data/Bombus_queen_observations_2015-2022.csv') %>% 
  filter(resource.coll != 'nectar', species %in% commonCrestedButteBees)

plant_spp_by_bee <- dat %>% 
  select(genus, species, plant.species) %>% 
  distinct() %>% 
  filter(species %in% c('nevadensis', 'californicus', 'appositus', 'flavifrons', 'rufocinctus', 'mixtus', 'bifarius')) %>% 
  group_by(species) %>% 
  count() %>% 
  pull(n) 

plant_spp_by_bee
min(plant_spp_by_bee)
mean(plant_spp_by_bee)
median(plant_spp_by_bee)
max(plant_spp_by_bee)

dat %>% 
  separate(plant.species, c('pl_genus', 'pl_species'), remove = F) %>% 
  select(pl_genus, species) %>% 
  distinct() 

pl_stats <- dat %>% 
  select(plant.species) %>% 
  separate(plant.species, c('genus', 'species'), remove = F) 

pl_stats %>%  # number of plant species. 
  distinct(plant.species) %>% 
  nrow()

pl_stats %>%  # no of genera by Weber
  distinct(genus) %>% 
  nrow()

pl_stats %>%  # the names of the genera
  distinct(genus)

pl1 <- pl_stats %>% 
  mutate(Family = case_when(
    genus %in% c('Heracleum', 'Osmorhiza', 'Taraxacum') ~ 'Apiaceae',
    genus %in% c('Dugaldia', 'Helianthella', 'Heliomeris', 'Erigeron', 'Senecio') ~ 'Asteraceae', 
    genus %in% c('Mertensia') ~ 'Boraginaceae',
    genus %in% c('Valeriana') ~ 'Valerianaceae',
    genus %in% c('Calochortus' ,'Erythronium') ~ 'Liliaceae', 
    genus %in% c('Distegia') ~ 'Caprifoliaceae',
    genus %in% c('Lathyrus', 'Lupinus', 'Vicia' ) ~ 'Fabaceae',
    genus %in% c('Frasera') ~ 'Gentianaceae',
    genus %in% c('Hydrophyllum', 'Phacelia') ~ 'Hydrophyllaceae',
    genus %in% c('Adenolinum') ~ 'Linaceae',
    genus %in% c('Claytonia') ~ 'Montiaceae',
    genus %in% c('Pedicularis') ~ 'Orobanchaceae', 
    genus %in% c('Dodecatheon') ~ 'Primulaceae',
    genus %in% c('Aconitum', 'Aquilegia', 'Delphinium', 'Thalictrum') ~ 'Ranunculaceae',
    genus %in% c('Erythrocoma', 'Rubus', 'Pentaphylloides', 'Rosa', 'Potentilla') ~ 'Rosaceae',
    genus %in% c('Salix') ~ 'Salicaceae'
  )) %>% 
  distinct(Family)
