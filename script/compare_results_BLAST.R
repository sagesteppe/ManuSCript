library(tidyverse)

setwd('~/Documents/ManuSCript/script')

blast <- read.csv('../data/tally_blast_classifications.csv') %>% 
  group_by(Sample) %>% 
  slice_max(Prcnt_seqs, n = 10) %>% 
  select(Sample, Taxon = taxid, Percent_Sample = Prcnt_seqs) %>% 
  mutate(
    Taxon = str_replace(Taxon, 'Arenaria', 'Eremogone'),
    Genus = str_remove(str_extract(Taxon, '^.* '), ' '),
    Method = 'BLAST')

automated <- read.csv('../data/Post_Classified_BLAST_sqs.csv') %>% 
  group_by(Sample, TAXON_NEW) %>%
  mutate(Prcnt_seqs = sum(Prcnt_seqs)) %>% 
  drop_na() %>% 
  distinct(Sample, TAXON_NEW, .keep_all = T) %>% 
  ungroup(TAXON_NEW) %>% 
  slice_max(Prcnt_seqs, n = 10) %>% 
  select(Sample, Genus = GENUS, Taxon = TAXON_NEW, Percent_Sample = Prcnt_seqs) %>% 
  mutate(across(where(is.character), ~ str_replace(., 'Arenaria', 'Eremogone')),
         Method = 'Automated')

expert <- read.csv('../data/Fully_Integrated_Corbiculae.csv') %>% 
  rename(Sample = sample.id) %>% 
  mutate(across(where(is.character), ~ str_trim(.)), 
         Method = 'Expert') %>% 
  drop_na() %>% 
  relocate()

genera2query <- na.omit(unique(c(blast$Genus, automated$Genus, expert$Genus))) 
genera2query <- genera2query[genera2query != 'Ericaceae'] 
genera2query <- taxizedb::name2taxid(genera2query, out_type = 'summary') 

genera_out <- taxizedb::classification(genera2query$id, db='ncbi', rank = 'family') 
genera_out <- map(genera_out, ~ 
                    data.frame( 
                      'Family' = filter(.x, rank == "family") %>% select(name), 
                      'Genus' = filter(.x, rank == 'genus') %>%  select(name) 
                    )) %>% 
  bind_rows() %>% 
  rename(Family = 1, Genus = 2) 

genera_out <- filter(genera_out, !Family %in% c('Mertensiidae', 'Hesperiidae')) 

all_classifications <- bind_rows(
  automated, blast, expert
) %>% 
  left_join(., genera_out, by = 'Genus') %>% 
  mutate(Rank = case_when(
    is.na(Family) ~ 'Family',
    Taxon == Family ~ 'Family',
    Taxon == Genus ~ 'Genus',
    TRUE ~ 'Species'
  ), 
  Family = if_else(Taxon == 'Ericaceae', 'Eriaceae', Family))

rm(genera2query, genera_out, automated, blast, expert)

expert <- all_classifications %>% filter(Method == 'Expert')
blast <- all_classifications %>% 
  filter(Method == 'BLAST' & Sample %in% expert$Sample)
automated <- all_classifications %>% 
  filter(Method == 'Automated' & Sample %in% expert$Sample)

rm(all_classifications)
## now determine which proportion of records are correct/incorrect

expert_g <- expert %>% 
  select(Sample, Genus) %>% 
  distinct()
expert_f <- expert %>% 
  select(Sample, Family) %>% 
  distinct()

# CORRECT RECORDS FIRST

true_species_blast <- left_join(expert, blast, by = c('Sample', 'Taxon')) %>% 
  group_by(Sample) %>% 
  drop_na(Method.y) %>% 
  summarize(Species_T = n())
true_genus_blast <- left_join(expert_g, blast, by = c('Sample', 'Genus')) %>% 
  group_by(Sample) %>% 
  drop_na(Method) %>% 
  distinct(Sample, Genus) %>% 
  summarize(Genera_T = n())
true_family_blast <- inner_join(expert_f, blast, by = c('Sample', 'Family')) %>% 
  group_by(Sample) %>% 
  drop_na(Method) %>% 
  distinct(Sample, Family) %>% 
  summarize(Family_T = n())

blast_correct <- left_join(true_species_blast, true_genus_blast, by = 'Sample') %>% 
  left_join(., true_family_blast, by = 'Sample')

rm(true_species_blast, true_genus_blast, true_family_blast)


true_species_computer <- left_join(expert, 
                                   filter(automated, Rank == 'Species'),
                                   by = c('Sample', 'Taxon')) %>% 
  group_by(Sample) %>% 
  drop_na(Method.y) %>% 
  summarize(Species_T = n())

true_genus_computer <- left_join(expert_g,  filter(automated, Rank != 'Family'),
                                 by = c('Sample', 'Genus')) %>% 
  group_by(Sample) %>% 
  drop_na(Method) %>% 
  distinct(Sample, Genus) %>% 
  summarize(Genera_T = n())

true_family_computer <- left_join(expert_f, automated, by = c('Sample', 'Family')) %>% 
  group_by(Sample) %>% 
  drop_na(Method) %>% 
  distinct(Sample, Family) %>% 
  summarize(Family_T = n())

computer_correct <- left_join(true_species_computer, true_genus_computer, by = 'Sample') %>% 
  left_join(., true_family_computer, by = 'Sample')

rm(true_species_computer, true_genus_computer, true_family_computer)


# FALSE records second

false_species_blast <- anti_join(blast, expert,  by = c('Sample', 'Taxon')) %>% 
  group_by(Sample) %>% 
  summarize(Species_F = n())

false_genus_blast <- anti_join(blast, expert_g, by = c('Sample', 'Genus')) %>% 
  group_by(Sample) %>% 
  distinct(Sample, Genus) %>% 
  summarize(Genera_F = n())

false_family_blast <-anti_join(blast, expert_f, by = c('Sample', 'Family')) %>% 
  group_by(Sample) %>% 
  distinct(Sample, Family) %>% 
  summarize(Family_F = n())

blast_incorrect <- left_join(false_species_blast, false_genus_blast, by = 'Sample') %>% 
  left_join(., false_family_blast, by = 'Sample')

blast_results <- left_join(blast_correct, blast_incorrect, by = 'Sample') %>% 
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

rm(false_species_blast, false_genus_blast, false_family_blast, blast_correct, blast_incorrect)

false_species_computer <- anti_join(automated, expert,  by = c('Sample', 'Taxon')) %>% 
  group_by(Sample) %>% 
  summarize(Species_F = n())

false_genus_computer <- anti_join(automated, expert_g, by = c('Sample', 'Genus')) %>% 
  group_by(Sample) %>% 
  distinct(Sample, Genus) %>% 
  summarize(Genera_F = n())

false_family_computer <-anti_join(automated, expert_f, by = c('Sample', 'Family')) %>% 
  group_by(Sample) %>% 
  distinct(Sample, Family) %>% 
  summarize(Family_F = n())

computer_incorrect <- left_join(false_species_computer, false_genus_computer, by = 'Sample') %>% 
  left_join(., false_family_computer, by = 'Sample')

computer_results <- left_join(computer_correct, computer_incorrect, by = 'Sample') %>% 
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

rm(false_species_computer, false_genus_computer, false_family_computer, 
   computer_correct, computer_incorrect)

### calculate accuracy

# number of assignments

automated_cnts <- automated %>% 
  group_by(Sample) %>% 
  count(Rank) %>% 
  pivot_wider(names_from = Rank, values_from = n, names_prefix = 'NO_') %>% 
  mutate(Method = 'COMP')
blast_cnts <- blast %>% 
  group_by(Sample) %>% 
  count(Rank) %>% 
  pivot_wider(names_from = Rank, values_from = n, names_prefix = 'NO_') %>% 
  mutate(Method = 'BLAST')
cnts <- bind_rows(automated_cnts, blast_cnts)

spp_count <- expert %>% 
  group_by(Sample) %>% 
  count(name = 'Spp_No')
gen_count <- expert_g %>% 
  group_by(Sample) %>% 
  count(name = 'Gen_No')
fam_count <- expert_f %>% 
  group_by(Sample) %>% 
  count(name = 'Fam_No')

rm(expert, expert_f, expert_g, automated_cnts, blast_cnts, automated, blast)

ACC_sens_Spec <- bind_rows(
  mutate(blast_results, Method = 'BLAST'),
  mutate(computer_results, Method  = 'COMP')
) %>% 
  left_join(., spp_count) %>% 
  left_join(., gen_count) %>% 
  left_join(., fam_count) %>% 
  left_join(., cnts, by = c('Sample', 'Method')) %>% 
  rowwise() %>% 
  mutate(
    Species_false = NO_Species - Species_T,
    
    Sp_Acc = (Species_T / NO_Species) * 100,
    Gen_Acc = (Genera_T / sum(NO_Genus, NO_Species, na.rm = T) ) * 100,
    
    Sp_Sens = (Species_T)  / (Species_T + (Spp_No - Species_T)) * 100
  )

rm(spp_count, gen_count, fam_count, blast_results, computer_results, cnts)
# Spp_No - Species_match is my TRUE NEGATIVE
# species_false is MY FALSE POSITIVE


wilcox.test(
  pull(ACC_sens_Spec[ACC_sens_Spec$Method == 'COMP', 'Sp_Acc']) , 
  pull(ACC_sens_Spec[ACC_sens_Spec$Method == 'BLAST', 'Sp_Acc']),
  alternative = "greater"
)

my_means <- ggpubr::compare_means(Sp_Acc ~ Method,  data = ACC_sens_Spec)
my_comparisons <- my_means %>% 
  nest(groups = c(group1, group2)) %>% 
  pull(groups) %>% 
  map(., as.character)
sample_sizes <- ACC_sens_Spec %>% 
  dplyr::group_by(Method) %>%  
  dplyr::tally() %>% 
  dplyr::mutate(n = paste0('n = ', n)) 
min_v <- dplyr::summarise(ACC_sens_Spec, mean_mpg = floor(min(Sp_Acc))) |>
  pull() |> min()

ggplot(ACC_sens_Spec, aes(y = Sp_Acc,  x= Method, color = Method),  
       alpha = 0.5) + 
  stat_boxplot(notch = T, notchwidth = 0.75, 
               varwidth = T, 
               outlier.shape = F, outlier.alpha = 0.8, outlier.colour = 'black') +
  geom_jitter(width = 0.1, shape = 1) +
  
  geom_text(data = sample_sizes,
            aes(Method, Inf, label = n), color = 'black', 
            vjust = "inward", size = 4, 
            y = min_v * 0.5) +
  ggpubr::stat_compare_means(comparisons = my_comparisons, 
                             aes(label = ..p.signif..),
                             tip.length = 0, vjust = 0.25, size = 4) +
  
  expand_limits(y= min_v - 5) +
  theme_bw() +
  scale_colour_manual(values = c('#F4AFB4', '#94A89A')) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_discrete(labels = c('BLAST', 'Post Classification')) + 
  labs(y = 'Accuracy', title = 'Accuracy of "naive" BLAST & post classification') +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5))

ggsave( '../graphics/plots/Accuracy_BLAST.png')

rm(my_means, sample_sizes, my_comparisons, min_v, ACC_sens_Spec)
