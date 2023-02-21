library(tidyverse)

setwd('~/Documents/ManuSCript/script')

pmorpho <- read.csv('../data/morpho_pollen.csv')

# this is an export of the variable 'blst' as would be expected on line 717  of the
# manuscript 

blst <- read.csv('../data/blst4experts.csv')

################################################################################
##############       RECLASIFY READS USING EXPERT DATA SET      ################
################################################################################

## Load in the microscopy count datasheets to gather information on sample provenance
pollen_sample_info <- readxl::read_xlsx(file.path('../data', 
                                                  list.files(path = '../data', pattern = 'xlsx') )) %>% 
  select(site, date, sample.id) %>% 
  distinct() %>% 
  filter(!date %in% c('42170', '42169'))


files <- list.files(path = '../data')

## load in the floral observations to provide high resolution spatial and temporal information
flr_date_ranges <- read.csv(
  file.path('../data',  files[grep('flower_ranks', files)] )) %>% 
  select(site, doy, week, plant.species:abun.rank) %>% 
  group_by(plant.species, site) %>% 
  select(site, week, doy, plant.species) %>% 
  mutate(Genus = str_extract(plant.species, '^.+?(?=[.]|$)')) %>% 
  mutate(Genus = case_when(
    Genus == 'Erythrocoma' ~ 'Geum', 
    Genus == 'Amerosedum' ~ 'Sedum', 
    Genus == 'Pentaphylloides' ~ 'Dasiphora',
    TRUE ~ Genus
  )) %>% 
  mutate(plant.species = case_when(
    plant.species == 'Lathyrus.leucanthus' ~ 'Lathyrus.lanszwertii',
    plant.species == 'Lupinus.bakeri' ~ 'Lupinus.sericeus',
    TRUE ~ plant.species
  )) %>% distinct()

rm(files)
## create a look up table to be able to join the known plant locations to 
## sequence data by WEEK
week_tab <- flr_date_ranges %>% 
  ungroup() %>% 
  select(doy, week) %>% 
  distinct()

flr_date_ranges <- select(flr_date_ranges, -doy)

## Create a data frame which contains the sample provenance and sequencing data
## information
blst_expert <- blst %>% 
  select(species, sample.id, TAXON_NEW, Genus, Family, Prcnt_seqs, Prcnt_Seqs_Class) %>% 
  mutate(RecordID = 1:n()) %>% 
  left_join(., pollen_sample_info,  by = 'sample.id') %>% 
  relocate(site:date, .before = TAXON_NEW) %>% 
  mutate(doy = lubridate::yday(date)) %>% 
  left_join(., week_tab, by = 'doy') %>% 
  left_join(., flr_date_ranges, by = c('site', 'Genus', 'week')) %>% 
  relocate(sample.id, .before = plant.species) 

rm(week_tab, flr_date_ranges, pollen_sample_info)

# Some taxa are in a genus with >1 species flowering at the point in time. 
# we identify these here, and reassign them so that each species gets an equivalent
# proportion of the reads

duplicate_reps_in_sample <- blst_expert %>% 
  
  # identify the duplicate samples
  drop_na(plant.species) %>% 
  group_by(RecordID) %>% 
  distinct(plant.species, .keep_all = T) %>% 
  group_by(sample.id, TAXON_NEW) %>% 
  filter(n() >= 2) 

dupe_ids <- duplicate_reps_in_sample %>% pull(RecordID) %>% unique()
dupe_samples <- duplicate_reps_in_sample %>% pull(sample.id) %>% unique()

# now split the reads assigned to them into each constituent taxon ~ 'unwise King Solomon'
duplicate_reps_in_sample <- duplicate_reps_in_sample %>% 
  ungroup() %>% 
  group_by(sample.id, Genus) %>% 
  mutate(Prcnt_seqs_Reclass = 
           ifelse(n() == 2, Prcnt_seqs/2, sum(Prcnt_seqs)/n_distinct(plant.species)/2 ),
         TAXON_EXPERT = plant.species) %>% 
  distinct(sample.id, plant.species, .keep_all = T)
# we will add these back onto the dataframe after our main cleaning process.

## define a list of splash out sequences, note the lsat 2 are not obligates with
## delphinium, and will not always be substituted out!!
ranunc_splash <- c('Caltha', 'Trollius', 'Thalictrum', 'Aquilegia')


# we will now manually reconsider EVERY single sequence classification in 
# light of the field based data. AND floral interactions observations
blst_reclass_expert <- blst_expert %>% 
  filter(!RecordID %in% dupe_ids) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(Genus = str_trim(Genus)) %>% 
  
  mutate(TAXON_EXPERT = case_when(
    
    sample.id == 3 & Family == 'Asteraceae' ~  'Taraxacum.officinale',
    sample.id == 3 & Family %in% c('Rosaceae', 'Onagraceae') ~  'Potentilla.pulcherrima',
    sample.id == 3 & Genus == 'Delphinium' ~ plant.species,       ## COMPLETE ##
    sample.id == 4 & Genus %in% ranunc_splash ~ 'Delphinium.nuttallianum',
    sample.id == 4 ~ plant.species,                               ## COMPLETE ##
    sample.id == 6 ~ plant.species,                               ## COMPLETE ##
    sample.id == 9 & TAXON_NEW == 'Symphyotrichum foliaceum' ~ 'Erigeron speciosus',
    sample.id == 9 ~ plant.species,                               ## COMPLETE ##
    
    sample.id == 10 & Family == 'Asteraceae' ~  'Taraxacum.officinale',
    #  sample.id == 10 & Family == 'Onagraceae' ~ ''
    sample.id == 10 ~ plant.species, # has many late season taxa
    
    sample.id == 14 & Genus == 'Symphyotrichum' ~ 'Senecio.integerrimus',
    sample.id == 14 ~ plant.species,                              ## COMPLETE ##
    sample.id == 15 & Family %in% c('Onagraceae', 'Rosaceae')  ~ 'Potentilla pulcherrima',
    
    sample.id == 15 & Genus %in% c('Cirsium', 'Pseudognaphalium') ~ 'Senecio.integerrimus',
    sample.id == 15 ~ plant.species,                              ## COMPLETE ## 
    sample.id == 18 & Family == 'Ericaceae' ~ 'Ericaceae', # ONLY TO FAMILY
    sample.id == 18 & Genus == 'Cynoglossum' ~ 'Mertensia.fusiformis', 
    sample.id == 18 & Family == 'Asteraceae' ~ 'Senecio.integerrimus',
    sample.id == 18 ~ plant.species,                              ## COMPLETE ##
    
    sample.id == 19 & Family == 'Onagraceae' ~ 'Epilobium',
    sample.id == 19 & Genus == 'Pseudognaphalium' ~ 'Senecio.integerrimus',
    sample.id == 19 & Genus == 'Nemophila' ~ 'Hydrophyllum.fendleri',
    sample.id == 19 ~ plant.species,                              ## COMPLETE ##
    sample.id == 20 & TAXON_NEW == 'Hydrophyllum canadense' ~ 'Hydrophyllum fendleri',
    sample.id == 20 & Family == 'Asteraceae' ~ 'Senecio.integerrimus',
    sample.id == 20 ~ plant.species,                              ## COMPLETE ##
    sample.id == 21 & Family == 'Asteraceae' ~ 'Senecio.integerrimus',
    sample.id == 21 ~ plant.species,                              ## COMPLETE ##
    sample.id == 23 & Genus %in% ranunc_splash ~ 'Delphinium.nuttallianum',
    sample.id == 23 ~ plant.species,                              ## COMPLETE ##
    sample.id == 24 & Genus %in% ranunc_splash ~ 'Delphinium.nuttallianum',
    sample.id == 24 ~ plant.species,                              ## COMPLETE ##
    sample.id == 25 & Genus %in% ranunc_splash ~ 'Delphinium.nuttallianum',
    sample.id == 25 ~ plant.species,                              ## COMPELTE ##
    sample.id == 26 & Genus %in% ranunc_splash ~ 'Delphinium.nuttallianum',
    sample.id == 26 ~ plant.species,                              ## COMPLETE ##
    sample.id == 27 & Genus %in% ranunc_splash ~ 'Delphinium.nuttallianum',
    sample.id == 27 ~ plant.species,                              ## COMPLETE ##
    sample.id == 28 & Genus %in% ranunc_splash ~ 'Delphinium.nuttallianum',
    sample.id == 28 ~ plant.species,                              ## COMPLETE ##
    sample.id == 29 ~ plant.species,                              ## COMPLETE ##
    sample.id == 30 & Genus == 'Cynoglossum' ~ 'Mertensia.fusiformis', 
    sample.id == 30 ~ plant.species,                              ## COMPLETE ##
    sample.id == 30 & Genus == 'Erigeron' ~ 'Erigeron elatior',
    sample.id == 32 & Genus %in% ranunc_splash ~ 'Delphinium.nuttallianum',
    sample.id == 32 ~ plant.species,                              ## COMPLETE ##
    
    sample.id == 33 ~ plant.species, # Symphyotrichum foliaceum check on day 166
    
    sample.id == 35 & Genus %in% ranunc_splash ~ 'Delphinium.nuttallianum',
    sample.id == 35 ~ plant.species,                              ## COMPLETE ##
    sample.id == 36 & Genus == 'Cynoglossum' ~ 'Mertensia.fusiformis',  
    sample.id == 36 ~ plant.species,                              ## COMPLETE ##
    sample.id == 39 & Genus %in% ranunc_splash ~ 'Delphinium.nuttallianum',
    sample.id == 39 ~ plant.species,                              ## COMPLETE ##
    sample.id == 40 ~ plant.species,                              ## COMPLETE ##
    sample.id == 41 & Genus == 'Cynoglossum' ~ 'Mertensia.ciliata', 
    sample.id == 41 & Genus == 'Viola' ~ 'Viola adunca',          ## COMPLETE ##
    sample.id == 41 ~ plant.species,                              ## COMPLETE ##
    sample.id == 42 ~ plant.species,
    sample.id == 43 & Genus == 'Epilobium' ~ 'Geranium richardsonii',
    sample.id == 43 ~ plant.species,                              ## COMPLETE ##
    sample.id == 43 & Genus == 'Epilobium' ~ 'Geranium richardsonii',
    sample.id == 44 ~ plant.species,
    sample.id == 45 & Family == 'Asteraceae' ~ 'Senecio.integerrimus',
    sample.id == 45 ~ plant.species,                              ## COMPLETE ##
    sample.id == 46 & Family == 'Asteraceae' ~ 'Senecio.integerrimus',
    sample.id == 46 & Genus == 'Cynoglossum' ~ 'Mertensia.fusiformis', 
    sample.id == 46 ~ plant.species,                              ## COMPLETE ##
    sample.id == 49 & Genus %in% ranunc_splash ~ 'Delphinium.barbeyi',
    sample.id == 49 & Genus == 'Viola' ~ 'Viola.adunca',
    sample.id == 49 ~ plant.species,                              ## COMPLETE ##
    
    sample.id == 51 & Genus %in% ranunc_splash ~ 'Delphinium.nuttallianum',
    sample.id == 51 ~ plant.species,  ## Helianthella quinquenervis @ DOY 166 ##
    sample.id == 52 ~ plant.species,    ## Erigeron elatior  on DOY 163 @ AVERY WASH ##
    
    sample.id == 54 ~ plant.species,                              ## COMPLETE ##
    sample.id == 55 & Genus == 'Cynoglossum' ~ 'Mertensia.fusiformis', 
    sample.id == 55 ~ plant.species,                              ## COMPLETE ##
    sample.id == 56 & Genus == 'Nemophila' ~ 'Hydrophyllum fendleri',
    sample.id == 56 & Family == 'Asteraceae' ~ 'Senecio.integerrimus',
    sample.id == 56 ~ plant.species,                              ## COMPLETE ##
    sample.id == 60 & Genus == 'Viola' ~ 'Viola.adunca',          ## COMPLETE ##
    sample.id == 60 ~ plant.species,
    sample.id == 65 & Genus == 'Viola' ~ 'Viola.praemorsa',       ## COMPLETE ##
    sample.id == 65 ~ plant.species, 
    
    TRUE ~ as.character(TAXON_NEW)
  ))  %>% 
  mutate( 
    TAXON_EXPERT = str_replace_all(TAXON_EXPERT, '[.]', ' '),
    TAXON_EXPERT = if_else(is.na(TAXON_EXPERT), TAXON_NEW, TAXON_EXPERT),
    
    # all of these are unequivocal remaps. 
    TAXON_EXPERT = str_replace(TAXON_EXPERT, 'Holodiscus argenteus', 'Potentilla pulcherrima'),
    TAXON_EXPERT = str_replace(TAXON_EXPERT, 'Scabrethia scabra', 'Wyethia arizonica'),
    TAXON_EXPERT = str_replace(TAXON_EXPERT, 'Geum ternatum', 'Geum triflorum'),
    TAXON_EXPERT = str_replace(TAXON_EXPERT, 'Erigeron grandiflorus', 'Erigeron elatior'),
    TAXON_EXPERT = str_replace(TAXON_EXPERT, 'Agastache pallidiflora', 'Pedicularis'),
    TAXON_EXPERT = str_replace(TAXON_EXPERT, 'Lithophragma parviflorum', 'Lithophragma glabrum'),
    TAXON_EXPERT = str_replace(
      TAXON_EXPERT, 'Arenaria globiflora|Minuartia recurva|Sagina procumbens', 'Eremogone congesta'),
    TAXON_EXPERT = if_else(doy <= 171 & Genus == 'Cynoglossum', 
                           'Mertensia fusiformis', TAXON_EXPERT), 
    TAXON_EXPERT = if_else(doy < 190 & TAXON_EXPERT == 'Lupinus argenteus', 
                           'Lupinus sericeus', TAXON_EXPERT))  %>% 
  
  # use this to sum up the total sequences by original read record
  distinct(sample.id, TAXON_NEW, .keep_all = T) %>% 
  group_by(sample.id, TAXON_EXPERT) %>% 
  mutate(Prcnt_seqs_Reclass = sum(Prcnt_seqs)) %>% 
  
  # now reduce to the amount of rows of the proper classification
  ungroup() %>% 
  distinct(sample.id, TAXON_EXPERT, .keep_all = T) %>% # what is going on with sample 56 then?
  
  # add back on the multiple groups
  bind_rows(., duplicate_reps_in_sample)  %>% 
  arrange(sample.id, -Prcnt_seqs_Reclass)

rm(ranunc_splash)

# now we split up the reads assigned to conflicted groups  # e.g. Cynoglossum amplifolium in sample 65
b <- blst_reclass_expert %>% 
  filter(sample.id %in% dupe_samples) %>% 
  group_by(sample.id, Family) %>% 
  filter(n() >= 3) %>% 
  mutate(plant.species = na_if(plant.species, "Thalictrum.fendleri")) %>% 
  mutate(Seqs2distribute = sum(Prcnt_seqs_Reclass)) 

records2drop <- b %>% pull(RecordID)

b <- b %>% 
  drop_na(plant.species) %>% 
  mutate(Prcnt_seqs_Reclass = Seqs2distribute/n_distinct(TAXON_EXPERT)) %>% 
  select(-Seqs2distribute)

blst_reclass_expert <- blst_reclass_expert %>% 
  filter(!RecordID %in% records2drop) %>% 
  bind_rows(., b) %>% 
  arrange(sample.id, -Prcnt_seqs_Reclass)

rm(records2drop, b)

## Sample 55 has Delphinium gracile, but is not caught by Solomon the unwise, 
## because the delphiniums DO NOT match the expert info above, they are a week 
## outside. We will split the reads 50/50 between Nuttalls & Barbey's here

s55 <- blst_reclass_expert %>% 
  filter(sample.id == 55) %>% 
  group_by(Genus) %>% 
  mutate(Seqs2distribute = sum(Prcnt_seqs_Reclass)) %>% 
  filter(TAXON_EXPERT != 'Delphinium gracile') %>% 
  mutate(Prcnt_seqs_Reclass = Seqs2distribute/n_distinct(TAXON_EXPERT)) %>% 
  select(-Seqs2distribute)

blst_reclass_expert <- blst_reclass_expert %>% 
  filter(sample.id != 55) %>% 
  bind_rows(., s55)  %>% 
  arrange(sample.id, -Prcnt_seqs_Reclass)

rm(s55)

## Cynoglossum was detected in sample 3, and both Mertensia are present in time 
## and space. However, no seqs were mapped to either ciliata or fusiformis. 
## we will split the reads between them

blst_reclass_expert <- blst_reclass_expert %>% 
  filter(sample.id == 3 & TAXON_EXPERT == 'Cynoglossum pringlei') %>% 
  slice(rep(1:n(), each=2)) %>% 
  mutate(Prcnt_seqs_Reclass = Prcnt_seqs_Reclass/2,
         dummyID = 1:n(),
         TAXON_EXPERT = if_else(dummyID == 1, 'Mertensia fusiformis', 'Mertensia cilita')) %>% 
  select(-dummyID) %>% 
  bind_rows(., filter(
    blst_reclass_expert, sample.id == 3 & TAXON_EXPERT != 'Cynoglossum pringlei'),
    filter(blst_reclass_expert, sample.id != 3)) 

## we will reclassify the original amounts of the classified sequence reads to 
## 100% of all reads. 

blst_reclass_expert <- blst_reclass_expert %>% 
  group_by(sample.id) %>% 
  mutate(multiplier = 100/ sum(Prcnt_seqs_Reclass),
         Prcnt_seqs_Reclass = Prcnt_seqs_Reclass * multiplier, 
         TAXON_EXPERT = str_replace(TAXON_EXPERT, '[.]', " "),
         Genus = str_trim(str_extract(TAXON_EXPERT, '^.* ')),
         Genus = ifelse(is.na(Genus), TAXON_EXPERT, Genus))

## we used 'YARGH' to be able to evaluate each sample one at a time without
## becoming overwhelmed, this worked on occasion.
yargh <- split(blst_reclass_expert, f = blst_reclass_expert$sample.id)

rm(blst, yargh, duplicate_reps_in_sample, blst_expert, 
   dupe_ids, dupe_samples)

################################################################################
##############     COMBINE READS WITH QUANTITATIVE DATA         ################
################################################################################

## remove slides 45 , and 54 which could not be counted

blst_reclass_expert <- blst_reclass_expert %>% 
  filter(!sample.id %in% c(45, 54))

options(scipen = 999)
pmorpho <- pmorpho %>% 
  filter(sample.id %in% unique(blst_reclass_expert$sample.id)) %>% 
  group_by(sample.id) %>% 
  mutate(
    morphotype = str_trim(morphotype),
    morphotype = str_replace(morphotype, 'GROSSULARIACEAE', 'Ribes'),
    morphotype = str_replace(morphotype, 'ROSACEAE', 'C'), 
    morphotype = str_to_sentence(morphotype)) %>% 
  filter(!morphotype %in% c('Conifer', 'Unknown')) %>% 
  group_by(sample.id, morphotype) %>% 
  mutate(sum = sum(sum)) %>% 
  group_by(sample.id) %>% 
  distinct(sample.id, morphotype, sum, .keep_all = T) %>% 
  mutate(Percent = (sum / sum(sum)) * 100) %>% 
  ungroup()


a <- c('Trifolium', 'Lupinus', 'Glyccrrhiza', 'Mitella', 'Geum')
b <- c('Lupinus', 'Lathyrus', 'Potentilla', 'Androsace', 'Bistorta', 'Vicia',
       'Dasiphora') # while not prepared, we include this with Potentilla
c <- c('Jeffersonia', 'Micranthes', 'Prunus', 'Delphinium', 'Androsace', 
       'Penstemon', 'Orthocarpus', 'Scuttelaria', 'Aquilegia',
       'Castilleja', 'Draba', 'Hydrophyllum', # Paldat
       'Lithophragma') # while not prepared, this most likely with Micranthes
d <- c('Salix', 'Boechera')
assorted <- c('Mertensia', 'Ribes', 'Pedicularis', 'Ericaceae', 
              'Epilobium', 'Eremogone', 'Geranium')
Asteraceae_1 <- c('Erigeron', 'Senecio', 'Symphyotrichum', 'Wyethia', 'Helianthella')
Asteraceae_2 <- c('Taraxacum')

morpho_lkp <- data.frame(
  Genus = c(a, b, c, d, assorted, Asteraceae_1, Asteraceae_2),  
  morphotype = c(
    rep('A', length(a)), rep('B', length(b)),  rep('C', length(c)),
    rep('D', length(d)), assorted, rep('Asteraceae_1', length(Asteraceae_1)),
    rep('Asteraceae_2', length(Asteraceae_2)))
  )

rm(a, b, c, d, assorted, Asteraceae_1, Asteraceae_2)

blr_sub <- blst_reclass_expert %>% 
  select(Genus, TAXON_EXPERT, Prcnt_seqs_Reclass) 

pmorpho <- pmorpho %>% 
  select(sample.id, morphotype, Percent_Morph = Percent) %>% 
 # Each Morphotype which is present in trace amounts will be standardized at 0.5 % of reads for Presence
  mutate(Percent_Morph = if_else(Percent_Morph < 0.5, 0.5, Percent_Morph)) # step 1
  
quant_read <- left_join(blr_sub, morpho_lkp, by = 'Genus', multiple = "all") %>% 
  left_join(., pmorpho, by = c('sample.id', 'morphotype'), multiple = "all") %>%  # step 2
  group_by(sample.id, morphotype) %>% 
  mutate(
    Adjusted_Percent = ifelse(is.na(Percent_Morph), 0.5, NA)) %>% # step 3
  ungroup() %>% 
  
  # step 4 & 5 - 
  group_by(sample.id, morphotype) %>% 
  mutate(Adjusted_Percent = (Prcnt_seqs_Reclass / sum(Prcnt_seqs_Reclass)) * Percent_Morph  ) %>% 
  
  # step 6 - part 1, grab the morphotype which has material
  group_by(sample.id, TAXON_EXPERT) %>% 
  slice_max(order_by = Percent_Morph, with_ties = T, n = 1) %>%
  slice_max(order_by = Percent_Morph, with_ties = F, n = 1)  %>% # grab a random one
  ungroup() %>% # this marks the end of step 5. 
  
  # step 7 
  mutate(Adjusted_Percent = replace_na(Adjusted_Percent, 0.5)) %>% 
  
  # Step 8
  group_by(sample.id) %>% 
  mutate(GREATER_SUM = 100 - sum(Adjusted_Percent) ) 


rm(blr_sub, morpho_lkp, pmorpho, blst_reclass_expert)
## SAMPLE 15 AND 26 NEED BE DONE BY HAND. 

## 15 IS WHERE THE MOPRHOTYPE 'A' IS NOT JOINED, THIS IS MOSTLY LIKELY TO BE 
# A POTENTILLA PULCHERRIMA AND IT WILL GAIN THE REMAINING 18.2%

quant_read <- quant_read %>% 
  filter(sample.id == 15) %>% 
  mutate(Percent_Morph = replace_na(Percent_Morph, 18.2), 
         GREATER_SUM = 0) %>% 
  bind_rows(., filter(quant_read, sample.id != 15))

## 26 IS THE WEIRD ONE WHERE THE MOLECULAR DATA DETECTED NO MERTENSIA - 
## IT NEED TO BE THE REMAINING 55.5%

quant_read <- quant_read %>% 
  filter(sample.id == 26) %>% 
  bind_rows(.,   data.frame(
    sample.id = 26, Genus = 'Mertensia', TAXON_EXPERT = 'Mertensia fusiformis', Prcnt_seqs_Reclass = NA, 
    Percent_Morph = 55.4, Adjusted_Percent = 55.4, GREATER_SUM = 0) ) %>% 
  mutate(GREATER_SUM = 0) %>% 
  bind_rows(., filter(quant_read, sample.id != 26)) 


# Now make all values equal to 100, add the 'extra' percentiles to samples  > 0.5 %
# And remove 'extra' percentiles to sample > 0.5 %

quant_read_low <- quant_read %>% 
  filter(Adjusted_Percent <= 1) %>% 
  select(-GREATER_SUM)

quant_read_high <- quant_read %>% 
  filter(Adjusted_Percent > 1) %>% 
  group_by(sample.id) %>% 
  mutate(GREATER_SUM = GREATER_SUM / n(),
         Adjusted_Percent = Adjusted_Percent + GREATER_SUM) %>% 
  select(-GREATER_SUM)

quant_read_final <- bind_rows(quant_read_low, quant_read_high) %>% 
  arrange(sample.id, -Adjusted_Percent) %>% 
  select(sample.id, Genus, Taxon = TAXON_EXPERT, Percent_Sample =  Adjusted_Percent)

rm(quant_read_low, quant_read_high, quant_read)
# PROCESS

# 1) Round up all counted morphotypes from < 0.5 to 0.5 to serve as TRACE quantities
# 2) Join Pollen count data with sequence data
# 3) For all Sequence data which does not have count information, mark it as 0.5% TRACE
# 4 & 5) If number of morphotypes per sample is 1, assign pollen count data to the taxon , 
# for morphotypes with many sequence classes, multiple the total count by the relative
# prop of each sequence class in it (same operation)
# 6) if Taxon_Expert > 1 per sample.id, drop the one which has na morphotype count values
# 7) now all records are suffused with a 0.5% for trace
# 8) Subtract the trace values from the larger values to get all values add up to 100$

packageVersion("dplyr") == '1.1.0' # # note ensure your dplyr is up to date . TRUE IS GOOD
data.frame( #  OR YOU CAN JUST DO THIS !!! this should return 3 rows
  Name  = c(rep('A', times = 2), rep('B', times = 2)),
  Value = c(1, 2, NA, NA) ) %>% 
  group_by(Name) %>% 
  slice_max(order_by = Value, with_ties = T, n = 1)  %>% # see hadlzzz cc8b410 @ gh
  nrow() == 3 
  
# 9) Subsequent to the intitial processing of read counts, value were subtracted
# or added to all sequences composing > 1% of a sample so that all values summed
# to 100%


