library(tidyverse)

#script to convert centrifuge output to comp file format. need to filter for number of reads prior to running this script
#names wont match up in rows, ie genus != species, but just used for inferring performance overall

input_files <- commandArgs(trailingOnly = TRUE)

#input_files <- c('/Users/agihawi/Desktop/tool_comparison/centrifuge/001_paired.cent', '001.comp')

inputfile <- read_tsv(file = input_files[1], col_names=TRUE)
outputfile <- input_files[2]

inphylum <- inputfile %>% filter(taxRank == 'phylum')
inclass <- inputfile %>% filter(taxRank == 'class')
inorder <- inputfile %>% filter(taxRank == 'order')
infamily <- inputfile %>% filter(taxRank == 'family')
ingenus <- inputfile %>% filter(taxRank == 'genus')
inspecies <- inputfile %>% filter(taxRank == 'species')


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

merged_precomp <- cbind(ncbi$NCBI.TaxID, ncbi$Kingdom, inphylum$name, inclass$name, inorder$name, infamily$name, ingenus$name, inspecies$name, reads_abund$Number_of_Reads, reads_abund$Relative_Abundance)
merged_precomp <- as.data.frame(merged_precomp)

colnames(merged_precomp) <- c("NCBI-TaxID", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Number_of_Reads", "Relative_Abundance")



write.table(merged_precomp, file = outputfile, sep = '\t', col.names=TRUE, row.names=FALSE, quote=FALSE)

write.table(merged_precomp, file = outputfile, sep = '\t', col.names=TRUE, row.names=FALSE, quote=FALSE)