##02 Relative abundances of groups
load("CDB_div.rda")
load("CDB_data.rda")
### ITS matrix
str(its)
#### Relative abundances
library(vegan)
rela_its = decostand(t(its), method = "total")
rela_its_t = t(rela_its)
rela_its_t = as.data.frame(rela_its_t)
rela_its_t$otu = row.names(rela_its_t)
#### Guilds
library(reshape2)
rela_its_t = melt(rela_its_t)
str(its_taxo)
its_rela_tab = merge(its_taxo, rela_its_t, by.x = "OTU", by.y = "otu")
#### 
library(tidyverse)
rela_site_guild = its_rela_tab %>%
  group_by(Guild, variable) %>%
  summarise(suma = sum(value))
###
ggplot(rela_site_guild, aes( x= variable, y = suma, fill = Guild)) +
  geom_bar(stat = "identity")
### Summarize guilds
its_taxo2 = its_taxo %>% 
  mutate(Guild2 = ifelse(str_detect(Guild, "saprotroph"), "saprotroph",
                         ifelse(str_detect(Guild, "parasite"), "parasite",
                                ifelse(str_detect(Guild, "ecto"), "ectomycorrhizal",
                                       ifelse(str_detect(Guild, "arbu"), 
                                            "arbuscular_mycorrhizal", Guild)))))


its_taxo2$Guild2 = its_taxo2$Guild2 %>% replace_na("unkonwn")

###merge
its_rela_tab2 = merge(its_taxo2, rela_its_t, by.x = "OTU", by.y = "otu")
rela_site_guild2 = its_rela_tab2 %>%
  group_by(Guild2, variable) %>%
  summarise(suma = sum(value))

ggplot(rela_site_guild2, aes( x= variable, y = suma, fill = Guild2)) +
  geom_bar(stat = "identity")
######Pars
###reshape
its_rela_guild = its_rela_tab2[, c("Guild2", "variable", "value")]
its_rela_guild = its_rela_guild %>% 
  group_by(variable, Guild2) %>% 
  summarise(suma = sum(value))
its_rela_guild  = pivot_wider(its_rela_guild, 
            names_from = "Guild2", values_from = "suma")

pairs(log(its_rela_guild[,-1]))
##############
library(sf)
library(viridis)
###Abundance maps
CDB_sf = merge(CDB_grid, its_rela_guild, by.x = "sample", by.y ="variable")
###Grid from centroids
CDB_grid = st_buffer(CDB_sf, dist = 2.5, endCapStyle = "SQUARE")
st_crs(CDB_grid) = st_crs(CDB_plants)
##
CDB_plants = mutate(CDB_plants, guiild = ifelse(species == "E", "Ecto",
                                                ifelse(species == "V", "N-Fixer",
                                                       ifelse(species == "E2", "Ecto",
                                                              "AM"))))
###Maps abudnace 
ggplot(CDB_grid)+
  geom_sf(aes(fill=ectomycorrhizal))+
  scale_fill_viridis()+
  geom_sf(data = CDB_plants, aes(color = guiild), size =2)+
  theme_void()+
  labs(title = "ITS abundance")
  
  ggplot(CDB_grid)+
    geom_sf(aes(fill=arbuscular_mycorrhizal))+
    scale_fill_viridis()+
    geom_sf(data = CDB_plants, aes(color = guiild), size =2)+
    theme_void()+
  labs(title = "ITS Abundance")
##############################################################################
##############################################################################
###bact
bact_rela = as.data.frame(t(decostand(t(bact), method = "total")))
bact_rela$otu = row.names(bact_rela)
bact_rela = melt(bact_rela)
###
bact_taxo = bact_taxo %>% 
  mutate(Guild2 = ifelse( str_detect(FAPROTAX, "nitrogen_fixation"), "n_fix", "other"))

bact_rela = merge(bact_taxo, bact_rela, by.x = "OTU", by.y = "otu")
  
bact_rela_tab = bact_rela %>% 
  group_by(Guild2, variable)%>%
  summarise(suma = sum(value))

bact_rela_guild  = pivot_wider(bact_rela_tab, 
                              names_from = "Guild2", values_from = "suma")

###Map
CDB_grid = merge(CDB_grid, bact_rela_guild, by.x = "sample", by.y ="variable")

ggplot(CDB_grid)+
  geom_sf(aes(fill=n_fix))+
  scale_fill_viridis()+
  geom_sf(data = CDB_plants, aes(color = guiild), size =2)+
  theme_void()+
  labs(title = "n_fixers Abundance")
