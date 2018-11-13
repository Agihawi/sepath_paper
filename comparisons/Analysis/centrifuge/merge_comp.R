#library(tidyverse)
library(readr)
library(dplyr)

#This script will merge 2 comp files - it was designed for PE report being merged with SE report

# from cmd, call Rscript, script INPUTCOMP TRUECOMP OUTPUT
cmdargs = commandArgs(trailingOnly=TRUE)
final_filename = cmdargs[3]

#debugging purposes
#cmdargs = c("01PEkraken_mpa_report.comp", "01SEkraken_mpa_report.comp")
#final_filename = "merged"

#check files exist
if (file.exists(cmdargs[1])){
  print(paste0("File one : ", cmdargs[1]))
} else {
  stop(paste0(cmdargs[1], "<-- File not Found."))
}

if (file.exists(cmdargs[2])){
  print(paste0("File two: ", cmdargs[2]))
} else { 
  stop(paste0(cmdargs[2], "<-- File not Found."))
}

print(paste0("Merging ", cmdargs[1], ' with ', cmdargs[2]))

# load two files
comp1 <- read_tsv(file = cmdargs[1], col_names = TRUE)
comp2 <- read_tsv(file = cmdargs[2], col_names = TRUE)

#merge dataframes by NCBI ID and Taxonomy
merged_comp <- merge(comp1, comp2, by=c('NCBI-TaxID',"Kingdom","Phylum","Class","Order","Family","Genus","Species"), all.x = TRUE, all.y = TRUE)

#set NA values to zero
merged_comp$Number_of_Reads.x[is.na(merged_comp$Number_of_Reads.x)] <- 0
merged_comp$Number_of_Reads.y[is.na(merged_comp$Number_of_Reads.y)] <- 0


#Add together number of reads if the merge was successful
merged_comp <- merged_comp %>% mutate(Number_of_Reads = Number_of_Reads.x + Number_of_Reads.y)

#Set relative abundance to 0, no use for it at present
merged_comp <- merged_comp %>% mutate(Relative_Abundance = 0)

merged_comp <- merged_comp %>% select('NCBI-TaxID',"Kingdom","Phylum","Class","Order","Family","Genus","Species","Number_of_Reads", "Relative_Abundance")

#now write to file
write.table(merged_comp, file=final_filename, col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")



