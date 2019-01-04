#Krakenuniq report to comp - call Rscript script inputfile outputfile - carry out filtering first as read counts wont survive!
library(tidyverse)
library(ggpubr)

input_files <- commandArgs(trailingOnly = TRUE)
#input_files <- c('~/Desktop/tool_comparison/kuniq/contig_classification/002.report', '~/Desktop/002.comp')
inputfile <- read_tsv(file = input_files[1], col_names=TRUE, skip=2)
outputfile <- input_files[2]

inphylum <- inputfile %>% filter(rank == 'phylum')
inclass <- inputfile %>% filter(rank == 'class')
inorder <- inputfile %>% filter(rank == 'order')
infamily <- inputfile %>% filter(rank == 'family')
ingenus <- inputfile %>% filter(rank == 'genus')
inspecies <- inputfile %>% filter(rank == 'species')


#do some length checks
if(nrow(inphylum) == 0) {
  inphylum <- data.frame("name"=NA, "taxID"=NA, "taxRank"=NA, "genomeSize"=NA, "numReads"=NA, "numUniqueReads"=NA, "abundance"=NA)
}
if(nrow(inclass) == 0) {
  inclass <- data.frame("name"=NA, "taxID"=NA, "taxRank"=NA, "genomeSize"=NA, "numReads"=NA, "numUniqueReads"=NA, "abundance"=NA)
}
if(nrow(inorder) == 0) {
  inorder <- data.frame("name"=NA, "taxID"=NA, "taxRank"=NA, "genomeSize"=NA, "numReads"=NA, "numUniqueReads"=NA, "abundance"=NA)
}
if(nrow(infamily) == 0) {
  infamily <- data.frame("name"=NA, "taxID"=NA, "taxRank"=NA, "genomeSize"=NA, "numReads"=NA, "numUniqueReads"=NA, "abundance"=NA)
}
if(nrow(ingenus) == 0) {
  ingenus <- data.frame("name"=NA, "taxID"=NA, "taxRank"=NA, "genomeSize"=NA, "numReads"=NA, "numUniqueReads"=NA, "abundance"=NA)
}
if(nrow(inspecies) == 0) {
  inspecies <- data.frame("name"=NA, "taxID"=NA, "taxRank"=NA, "genomeSize"=NA, "numReads"=NA, "numUniqueReads"=NA, "abundance"=NA)
}


ncbi <- data.frame("NCBI-TaxID" = 0, "Kingdom" = NA)
reads_abund <- data.frame("Number_of_Reads" = 0, "Relative_Abundance" = 0)

merged_precomp <- cbind(ncbi$NCBI.TaxID, ncbi$Kingdom, inphylum$taxName, inclass$taxName, inorder$taxName, infamily$taxName, ingenus$taxName, inspecies$taxName, reads_abund$Number_of_Reads, reads_abund$Relative_Abundance)
merged_precomp <- as.data.frame(merged_precomp)

colnames(merged_precomp) <- c("NCBI-TaxID", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Number_of_Reads", "Relative_Abundance")

write.table(merged_precomp, file = outputfile, sep = '\t', col.names=TRUE, row.names=FALSE, quote=FALSE)
