####CDB################################
setwd("C:/Users/oscar94/Documents/PhD/Microislands/working")
####0.1.Singletons and iNEXT###########
#######################################
load("CDB_data.rda")
#######################################
##Singletons###########################
##ITS##################################
dim(its)
its = its[which(rowSums(its)>1),]
dim(its)
#Samples with less than 100 reads######
which(colSums(its)<20)
##SSU##################################
dim(ssu)
ssu = ssu[which(rowSums(ssu)>1),]
dim(ssu)
#Samples with less than 100 reads######
ssu = ssu[,which(colSums(ssu)>20)]
dim(ssu)
###Two sites with less than 100 reads
###Bacteria############################
dim(bact)
bact = bact[which(rowSums(bact)>1),]
dim(bact)
#Samples with less than 100 read#######
which(colSums(bact)<20)
##All with more than 100 reads
#######################################
#######################################
#iNEXT#################################
library(iNEXT)
Sys.time()
its_e = iNEXT(its, datatype = "abundance")
Sys.time()
ssu_e = iNEXT(ssu, datatype = "abundance")
Sys.time()
bact_e = iNEXT(bact, datatype = "abundance")
Sys.time()
########################################
########################################
########################################
###N Fixers ############################
nfix_taxo = bact_taxo [grepl("nitrogen_fixation" ,bact_taxo$FAPROTAX),]
n_fix = subset(bact, rownames(bact) %in% nfix_taxo$OTU)
n_fix_e = iNEXT(n_fix, datatype = "abundance")
########################################
########################################
###EcM##################################
EcM_taxo = its_taxo[grepl("ectomycorrhizal", its_taxo$Guild),]
EcM = subset(its, rownames(its) %in% EcM_taxo$OTU)
colSums(EcM)
#EcM_e = iNEXT(EcM, datatype = "abundance")
########################################
########################################
##AMF###################################
AMF_taxo = its_taxo[grepl("arbuscular_mycorrhizal", its_taxo$Guild),]
AMF = subset(its, rownames(its) %in% AMF_taxo$OTU)
colSums(AMF)
#AMF_e = iNEXT(AMF, datatype = "abundance")
###Melt 
###Diversity estimates ITS
its_div = reshape(its_e$AsyEst, idvar = "Assemblage", timevar = "Diversity", direction = "wide")
its_div = its_div[, c("Assemblage", "Estimator.Species richness", "Estimator.Shannon diversity",  "Estimator.Simpson diversity")]
names(its_div) = c ("Sample", "Richness", "Shannon", "Simpson")
###Diversity estimates SSU
ssu_div = reshape(ssu_e$AsyEst, idvar = "Assemblage", timevar = "Diversity", direction = "wide")
ssu_div = ssu_div[, c("Assemblage", "Estimator.Species richness", "Estimator.Shannon diversity",  "Estimator.Simpson diversity")]
names(ssu_div) = c ("Sample", "Richness", "Shannon", "Simpson")
###Diversity estimates bact
bact_div = reshape(bact_e$AsyEst, idvar = "Assemblage", timevar = "Diversity", direction = "wide")
bact_div = bact_div[, c("Assemblage", "Estimator.Species richness", "Estimator.Shannon diversity",  "Estimator.Simpson diversity")]
names(bact_div) = c ("Sample", "Richness", "Shannon", "Simpson")
###Diversity estimates n_fixers
Nfix_div = reshape(n_fix_e$AsyEst, idvar = "Assemblage", timevar = "Diversity", direction = "wide")
Nfix_div = Nfix_div[, c("Assemblage", "Estimator.Species richness", "Estimator.Shannon diversity",  "Estimator.Simpson diversity")]
names(Nfix_div) = c ("Sample", "Richness", "Shannon", "Simpson")
########################################
########################################
save(its_e,ssu_e,bact_e, n_fix_e, file ="CDB_inext.rda")
########################################
its_div = its_div[order(its_div$Sample),]
ssu_div = rbind(ssu_div,c("CDB-29",0,0,0))
ssu_div  = ssu_div = ssu_div[order(ssu_div$Sample),]
bact_div = bact_div[order(bact_div$Sample),]
Nfix_div = Nfix_div[order(Nfix_div$Sample),]
Richness = as.data.frame(cbind(its_div$Sample,its_div$Richness, ssu_div$Richness, bact_div$Richness, Nfix_div$Richness))
names(Richness) = c("Sample", "ITS", "SSU","Bact", "NFix")
########################################
EcM_div = colSums(EcM>0)
AMF_div = colSums(AMF>0)
Richness = cbind(Richness, AMF_div, EcM_div)
########################################
########################################
save(Richness, AMF, EcM, n_fix, its, bact, ssu, CDB_grid, CDB_plants,file = "CDB_Div.rda")

