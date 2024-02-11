setwd('~/Documents/ManuSCript/script')

library(tidyverse)

pmorpho <- read.csv('../data/morpho_pollen.csv') %>% 
  filter(caste == 'q') %>%  # reduce to queens only here, use this as the filtering join later
  select(Sample = sample.id, Bee_spp = species, Site = site, Date = date,
         Plant_spp_morpho = morphotype, Count = sum ) %>% 
  mutate(Observation = 'Microscopy')

site_info <- select(pmorpho,   Sample, Bee_spp, Site, Date) %>% 
  distinct()
  
morpho_lkp <- read.csv('../data/pollen_morphotype_grp_lookup.csv') %>% 
  mutate(morphotype = str_to_upper(morphotype))

blst <- read.csv('../data/Post_Classified_BLAST_sqs.csv') %>% 
  group_by(Sample) %>% 
  slice_max(Prcnt_seqs, n = 10) %>% # reduce to the top 10 records
  ungroup() %>% 
  select(Sample, Plant_spp_morpho = TAXON, Count = Seqs_pr_taxon) %>% 
  mutate(Observation = 'Molecular') %>% 
  inner_join(., site_info, by = 'Sample', relationship = "many-to-many")
# use inner join to drop the unmatched records


queen_obs <- read.csv('../data/Bombus_queen_observations_2015.csv') %>% 
  select(Bee_spp = species, Site = site, Date = date,
         Plant_spp_morpho = plant.species ) %>% 
  mutate(Bee_spp = str_remove(Bee_spp, '[.]orange|[.]dark'), 
         Plant_spp_morpho = str_replace(Plant_spp_morpho, '[.]', ' '),
         Date = as.character(lubridate::dmy(Date)))  %>% 
  count(Bee_spp, Site, Date, Plant_spp_morpho, name = 'Count') %>% 
  mutate(Observation = 'Field', Sample = NA) %>% 
  
  mutate(
    Plant_spp_morpho = str_replace(Plant_spp_morpho, 'leucanthus', 'lanszwertii'),
    Plant_spp_morpho = str_replace(Plant_spp_morpho, 'bakeri', 'sericeus'),
    Plant_spp_morpho = str_replace(Plant_spp_morpho, 'Adenolinum', 'Linum'),
    Plant_spp_morpho = str_replace(Plant_spp_morpho, 'Dugaldia', 'Hymenoxys'),
    Plant_spp_morpho = str_replace(Plant_spp_morpho, 'Ligularia', 'Senecio'),
    Plant_spp_morpho = str_replace(Plant_spp_morpho, 'Distegia', 'Lonciera'),
    Plant_spp_morpho = str_replace(Plant_spp_morpho, 'Erythrocoma triflora', 'Geum trifloum'),
) 
  
all_records <- bind_rows(pmorpho, blst, queen_obs) %>% 
  arrange(Bee_spp, Site, Date, Observation)

rm(queen_obs, pmorpho, blst, site_info)

write.csv(all_records, '../data/all_interactions.csv', row.names = F)
