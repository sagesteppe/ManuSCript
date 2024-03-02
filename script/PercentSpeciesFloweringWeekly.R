setwd('~/Documents/ManuSCript/script')

library(tidyverse)

weibs <- bind_rows(
  read.csv('../data/weibull_estimates.csv') %>% 
  mutate(Estimate = 'SubalpineOnly'), 
  read.csv('../data/weibull_estimates_mixed_montane.csv') %>% 
  mutate(Estimate = 'SubalpineMontane')
) %>% 
  arrange(taxon) %>% 
  filter(metric %in% c('tenth', 'ninety')) %>% 
  select(event, taxon, doy = DOY)

n_tax_modelled <- nrow(distinct(weibs, taxon)) # 383

weibs <- split(weibs, f = list( weibs$taxon, weibs$event))
weibs <- lapply(weibs, function(x){data.frame(
  doy = as.matrix(
    seq(x$doy[1], x$doy[2])))})  %>% 
  bind_rows(.id = 'taxon')

bees <- read.csv('../data/Bombus_queen_observations_2015.csv') %>% 
  mutate(species = gsub('[.].*$', '', species)) %>% 
  select(year, week, species) %>% 
  group_by(species, week) %>% 
  tally() %>% 
  filter(! species %in% c('centralis', 'melanopygus', 'kirbiellus', 
                          'unknown', 'insularis', 'sylvicola'))

#ggplot(bees, aes(x = n, y = species)) + 
#  geom_density_ridges()

weeks <- data.frame(
  week = rep(1:19, each = 7), 
  doy = 
    c(130:136, 137:143, 144:150, 151:157, 158:164, 165:171, 172:178, 179:185, 186:192, 
      193:199, 200:206, 207:213, 214:220, 221:227, 228:234, 235:241,
      242:248, 249:255, 256:262)
    )

test <- left_join(weibs, weeks) %>% 
  distinct(taxon, week) %>% 
  filter(week %in% 3:11) %>% 
  separate(taxon,  c('taxon', 'estimate'), '[.]') %>% 
  filter(estimate == 'estimate')

rm(weibs, weeks)

wkkly <- test %>% 
  group_by(week) %>% 
  filter(estimate == 'estimate') %>% 
  tally()
wkkly$prop <- wkkly$n / n_tax_modelled

png(filename = '../graphics/plots/PropFloweringByWeek.png')
barplot(wkkly$prop, horiz = F, names.arg = 3:11, ylim = c(0,1), col = '#d0e2c9',
        yaxt = "n", xlab = 'Week', ylab = 'Proportion', 
        main = 'Proportion of the 383 species with\nmodelled phenology flowering by week')
axis(side = 2, las = 2)
dev.off()

group_by(bees, species) %>% 
  slice_min(week, n = 1)
group_by(bees, species) %>% 
  slice_max(week, n = 1)

plot(x = 3:11, xlim = 3:11)


rm(weeks)
