########################################
###CDB.02.Diversity maps################
########################################
load("CDB_Div.rda")
########################################
library(sf)
library(tmap)
########################################
CDB_plants = mutate(CDB_plants, guiild = ifelse(species == "E", "Ecto",
                                                ifelse(species == "V", "N-Fixer",
                                                       ifelse(species == "E2", "Ecto",
                                                              "AM"))))
########################################
#General OTU diversity##################
str(Richness)
str(CDB_grid)
Richness$ITS = as.numeric(Richness$ITS)
Richness$SSU = as.numeric(Richness$SSU)
Richness$Bact = as.numeric(Richness$Bact)
Richness$NFix = as.numeric(Richness$NFix)
CDB_sf = merge(CDB_grid, Richness, by.x = "sample", by.y ="Sample")
###Grid from centroids
CDB_grid = st_buffer(CDB_sf, dist = 2.5, endCapStyle = "SQUARE")
st_crs(CDB_grid) = st_crs(CDB_plants)
##ITS###################################
library(ggplot2)
library(viridis)
#######
ggplot(CDB_grid)+
  geom_sf(aes(fill=ITS))+
  scale_fill_viridis()+
  geom_sf(data = CDB_plants, aes(color = guiild), size =2)+
  theme_void()+
  labs(title = "ITS Extrapolated Richness")
##SSU###################################
ggplot(CDB_grid)+
  geom_sf(aes(fill=SSU))+
  scale_fill_viridis()+
  geom_sf(data = CDB_plants, aes(color = guiild), size =2)+
  theme_void()+
  labs(title = "SSU Extrapolated Richness")
##Bact##################################
ggplot(CDB_grid)+
  geom_sf(aes(fill=Bact))+
  geom_sf(data = CDB_plants, aes(color = guiild), size =2)+
  scale_fill_viridis()+
  theme_void()+
  labs(title = "Bact Extrapolated Richness")
##NFix###################################
ggplot(CDB_grid)+
  geom_sf(aes(fill=NFix))+
  scale_fill_viridis()+
  geom_sf(data = CDB_plants, aes(color = guiild), size =2)+
  theme_void()+
  labs(title = "N-Fixers Extrapolated Richness")
##AMF-ITS###############################
ggplot(CDB_grid)+
  geom_sf(aes(fill=AMF_div))+
  scale_fill_viridis()+
  geom_sf(data = CDB_plants, aes(color = guiild), size =2)+
  theme_void()+
  labs(title = "AMF ITS")
##ECM-ITS###############################
ggplot(CDB_grid)+
  geom_sf(aes(fill=EcM_div))+
  scale_fill_viridis()+
  geom_sf(data = CDB_plants, aes(color = guiild), size =2)+
  theme_void()+
  labs(title = "EcM ITS")
########################################
########################################
#Abundance ITS##########################
abu_EcM = colSums(EcM)
abu_AMF = colSums(AMF)
abun = data.frame(abu_AMF,abu_EcM, colnames(EcM))
CDB_grid = merge(CDB_grid, abun, by.x = "sample", by.y ="colnames.EcM.")
##AMF-ITS abundance#########################
ggplot(CDB_grid)+
  geom_sf(aes(fill= log1p(abu_AMF)))+
  scale_fill_viridis()+
  geom_sf(data = CDB_plants, aes(color = guiild), size =2)+
  theme_void()+
  labs(title = "AMF-ITS abundance")
##ECM-ITS###############################
ggplot(CDB_grid)+
  geom_sf(aes(fill=log1p(abu_EcM)))+
  scale_fill_viridis()+
  geom_sf(data = CDB_plants, aes(color = guiild), size =2)+
  theme_void()+
  labs(title = "EcM-ITS abundance")

##ECM-AMF###############################
CDB_grid$ecm_amf = abu_AMF/abu_EcM
ggplot(CDB_grid)+
  geom_sf(aes(fill=log1p(ecm_amf)))+
  scale_fill_viridis()+
  geom_sf(data = CDB_plants, aes(color = guiild), size =2)+
  theme_void()+
  labs(title = "AMF/EcM")

##########################################
###########################################
library(vegan)
t(its)
r_its = decostand(t(its), method = "total")
####filter those that are ecto
EcM
rela_its = rowSums(r_its)
r_amf = decostand(AMF, method = "total")
rela_amf = colSums(r_amf)
data.frame(rela_amf, rela_its)





summary(CDB_grid$ITS)
summary(CDB_grid$SSU)
par(mfrow=c(3,2))
hist(CDB_grid$ITS)
hist(CDB_grid$SSU)
hist(CDB_grid$NFix)
hist(CDB_grid$Bact)
hist(log1p(CDB_grid$abu_AMF))
hist(log1p(CDB_grid$abu_EcM))

par(mfrow=c(1,1))
###Calcular abundancias relativas, correlogramas de abundacias entre grupos funcionales diferentes




