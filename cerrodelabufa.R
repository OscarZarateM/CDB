#CerrodelaBufa
setwd("C:/Users/oscar94/Documents/PhD/Microislands/working")
library(readxl)
##Meta
meta = read_excel("mexico_2023_samples.xlsx")
str(meta)
##ITS
its = read_excel("kadri.mexico.2023.its.xlsx")
str(its)
##SSU
ssu = read_excel("kadri.mexico.wanda29.2023.ssu.xlsx")
str(ssu)
## bact
bact = read_excel("kadri.maxico.2023.bac.c99.faprotax.xlsx")
str(bact)
##########################################################
##########################################################
########## EcM ###########################################
unique(its$primary_lifestyle)
ecto = subset(its, primary_lifestyle == "ectomycorrhizal")
otu = ecto$Samples
nits = its[1 ,grepl("CDB",names(ecto))]
ecto = ecto[ ,grepl("CDB",names(ecto))]
names(ecto) = nits
ecto = cbind(otu, ecto)
ecto = sapply(ecto[,-1 ], as.numeric)
EcM = colSums(ecto > 0)
#########################################################
##########AMF  ##########################################
ssu_2 = ssu[ ,grepl("CDB",names(ssu))]
nssu = unlist(ssu_2[1,])
names(ssu_2) = nssu
ssu_2 = sapply(ssu_2[-1, ], as.numeric)
id = ssu[-1, "ORIGINAL_SAMPLE"]
ssu = cbind(id ,ssu_2)

AMF = colSums(ssu[,-1] > 0)

########################################################
###########N-fix########################################
unique(bact$category1)
nbact = bact[1, -1]
n_fix = bact [grepl("n-cycle" ,bact$category1),]
otu = n_fix$ORIGINAL_SAMPLE
n_fix = n_fix[, grepl("CDB", names(n_fix))]
names(n_fix) = nbact
nfix = cbind(otu, n_fix)
nfix = sapply(nfix[-1, ], as.numeric)
NFix = colSums(nfix[,-1] > 0)

########################################################
Rich = cbind(NFix, AMF, EcM)
id = row.names(Rich)
Rich = cbind(id, Rich)
########################################################
library(sf)
meta = meta[ grepl("CDB", meta$sample_id),]
points_sf = st_as_sf(meta, coords = c("lon", "lat"))
trees = meta[grepl("euca",meta$sample_id),]
trees_sf = st_as_sf(trees, coords = c("lon", "lat"))
points_sf = merge(points_sf, Rich, by.x = "sample_id", by.y = "id")
st_crs(points_sf) = 4326
points_sf$NFix = as.numeric(points_sf$NFix)
points_sf$AMF = as.numeric(points_sf$AMF)
points_sf$EcM = as.numeric(points_sf$EcM)
library(tmap)
tm_shape(points_sf)+
  tm_bubbles(size = "AMF")


st_write(points_sf, "CDB.gpkg")
st_write(trees_sf, "trees_CDB.gpkg", append=FALSE)

