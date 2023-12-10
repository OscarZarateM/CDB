####CDB################################
####0.0.Data Preparation###############
##ITS##################################
its = read_excel("kadri.mexico.2023.its.xlsx")
str(its)
##ITS taxo and guild
its_taxo = its[-1,c("Samples", "Taxonomy", "primary_lifestyle")]
##OTU codes
its_otu = its$Samples[-1]
##Converted sample ids
nits = its[1 ,grepl("CDB",names(its))]
##Filter ID from CDB
its = its[-1 ,grepl("CDB",names(its))]
##Change names
names(its) = nits
str(its)
its = sapply(its, as.numeric)
rownames(its) = its_otu
##Remove OTU with 0 reads (from tulum and Pinus)
dim(its)
its = its[which(rowSums(its)>1),]
dim(its)
##Harmonize its_taxo
rownames(its)
its_taxo = subset(its_taxo, Samples %in% rownames(its))
##SSU##################################
ssu = read_excel("kadri.mexico.wanda29.2023.ssu.xlsx")
str(ssu)
##ssu taxo 
ssu_taxo = ssu[-1,c("ORIGINAL_SAMPLE", "taxonomy")]
##OTU codes
ssu_otu = ssu$ORIGINAL_SAMPLE[-1]
##Converted sample ids
nssu = ssu[1 ,grepl("CDB",names(ssu))]
##Filter ID from CDB
ssu = ssu[-1 ,grepl("CDB",names(ssu))]
##Change names
names(ssu) = nssu
str(ssu)
ssu = sapply(ssu, as.numeric)
rownames(ssu) = ssu_otu
##Remove OTU with 0 reads (from tulum and Pinus)
dim(ssu)
ssu = ssu[which(rowSums(ssu)>1),]
dim(ssu)
##Harmonize ssu_taxo
rownames(ssu)
ssu_taxo = subset(ssu_taxo, ORIGINAL_SAMPLE %in% rownames(ssu))
##Bact#################################
bact = read_excel("kadri.maxico.2023.bac.c99.faprotax.xlsx")
str(bact)
##bact taxo and guild
bact_taxo = bact[-1,c("ORIGINAL_SAMPLE", "taxonomy", "FAPROTAX","category1")]
##OTU codes
bact_otu = bact$ORIGINAL_SAMPLE[-1]
##Converted sample ids
nbact = bact[1 ,grepl("CDB",names(bact))]
##Filter ID from CDB
bact = bact[-1 ,grepl("CDB",names(bact))]
##Change names
names(bact) = nbact
str(bact)
bact = sapply(bact, as.numeric)
rownames(bact) = bact_otu
##Remove OTU with 0 reads (from tulum and Pinus)
dim(bact)
bact = bact[which(rowSums(bact)>1),]
dim(bact)
##Harmonize bact taxo
rownames(bact)
bact_taxo = subset(bact_taxo, ORIGINAL_SAMPLE %in% rownames(bact))
######################################
##Harmonize col names#################
names(its_taxo) = c("OTU", "Taxonomy", "Guild")
names(ssu_taxo) = c("OTU", "Taxonomy")
names(bact_taxo) = c("OTU", "Taxonomy", "FAPROTAX", "Guild")
######################################
##Meta data###########################
library(sf)
CDB_grid = st_read("CDB_grid.gpkg")
CDB_plants = st_read("CDB_plants.gpkg")
#######################################
##Save#################################
save(its, its_taxo, ssu, ssu_taxo, bact, bact_taxo, CDB_grid, CDB_plants, file = "CDB_data.rda")

unique(its_taxo$Guild)





