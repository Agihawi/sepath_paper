#library(tidyverse)
library(readr)
library(dplyr)


# from command line, call Rscript, script INPUTCOMP TRUECOMP
cmdargs = commandArgs(trailingOnly=TRUE)
#cmdargs <- c('/Users/agihawi/Desktop/Kaiju_WGS/greedy/output.comp', '~/Desktop/tool_comparison/true_comp/001.comp')

#check if parsed files exist
if (file.exists(cmdargs[1])){
  cat(paste0("Analysing File: ", cmdargs[1]))
} else {
  stop(paste0(cmdargs[1], "<-- File not Found."))
}

if (file.exists(cmdargs[2])){
  cat(paste0("True COMP File: ", cmdargs[2]))
} else { 
  stop(paste0(cmdargs[2], "<-- File not Found."))
}

#load true comp file
true_comp <- read_tsv(file = cmdargs[2], col_names = TRUE)

#load input compfile
input_comp <- read_tsv(file = cmdargs[1], col_names=FALSE)

#input headers may or may not be there, so add your own, then filter out other ones
colnames(input_comp) <- c("NCBI-TaxID",
                          "Kingdom",
                          "Phylum",
                          "Class",
                          "Order",
                          "Family",
                          "Genus",
                          "Species",
                          "Number_of_Reads",
                          "Relative_Abundance")
input_comp <- input_comp %>% filter(`NCBI-TaxID` !="NCBI-TaxID")


#Form unique lists of the true comp file

# distinct_vector = distinct_df$field1  df %>% distinct(field1)
true_kingdom <- true_comp %>% filter(!is.na(Kingdom)) %>% distinct(Kingdom)  #as.data.frame(unique(true_comp$Kingdom))
true_phylum <- true_comp %>% filter(!is.na(Phylum)) %>% distinct(Phylum)
true_class <- true_comp %>% filter(!is.na(Class)) %>% distinct(Class)
true_order <- true_comp %>% filter(!is.na(Order)) %>% distinct(Order)
true_family <- true_comp %>% filter(!is.na(Family)) %>% distinct(Family)
true_genus <- true_comp %>% filter(!is.na(Genus)) %>% distinct(Genus)
true_species <- true_comp %>% filter(!is.na(Species)) %>% distinct(Species)


#Form unique lists of the input comp file
input_kingdom <- input_comp %>% filter(!is.na(Kingdom)) %>% distinct(Kingdom)
input_phylum <- input_comp %>% filter(!is.na(Phylum)) %>% distinct(Phylum)
input_class <- input_comp %>% filter(!is.na(Class)) %>% distinct(Class)
input_order <- input_comp %>% filter(!is.na(Order)) %>% distinct(Order)
input_family <- input_comp %>% filter(!is.na(Family)) %>% distinct(Family)
input_genus <- input_comp %>% filter(!is.na(Genus)) %>% distinct(Genus)
input_species <- input_comp %>% filter(!is.na(Species)) %>% distinct(Species)
input_species <- as.data.frame(gsub("_", " ", input_species$Species)) #remove pesky _'s from input_species
colnames(input_species) <- "Species"

#declare values
kingdom_PPV = 0
kingdom_SSV = 0
phylum_PPV = 0
phylum_SSV = 0
class_PPV = 0
class_SSV = 0
order_PPV = 0
order_SSV = 0
family_PPV = 0
family_SSV = 0
genus_PPV = 0
genus_SSV = 0
species_PPV = 0
species_SSV = 0


#make them all a list so they work with for statements
true_kingdom_list <- as.list(true_kingdom[1])
true_phylum_list <- as.list(true_phylum[1])
true_class_list <- as.list(true_class[1])
true_order_list <- as.list(true_order[1])
true_family_list <- as.list(true_family[1])
true_genus_list <- as.list(true_genus[1])
true_species_list <- as.list(true_species[1])
input_kingdom_list <- as.list(input_kingdom[1])
input_phylum_list <- as.list(input_phylum[1])
input_class_list <- as.list(input_class[1])
input_order_list <- as.list(input_order[1])
input_family_list <- as.list(input_family[1])
input_genus_list <-as.list(input_genus[1]) 
input_species_list <- as.list(input_species[1])


#begin calculations -- MAKE SURE dividing is above 0 before calculating
# sensitivity = tp/(tp + fn)
# ppv = tp/ (tp + fp)

#Kingdom calculations
Kingdom_TP = 0
Kingdom_TN = 0
Kingdom_FN = 0
Kingdom_FP = 0
i = 1

if((nrow(input_kingdom))>0) {
  while(i<(nrow(input_kingdom)+1)){
    if((any((as.character((input_kingdom[i,1])))==true_kingdom))==TRUE){
      Kingdom_TP = Kingdom_TP + 1
      cat(paste('Kingdom', (input_kingdom[i,1]), 'found in both lists - True Positive\n', sep='\t'))
    } else {
      Kingdom_FP = Kingdom_FP + 1
      cat(paste('Kingdom',(input_kingdom[i,1]), 'in input, but not found - False Positive\n', sep='\t'))
    }
    i = i + 1
  }
  
  i = 1 
  while(i<(nrow(true_kingdom)+1)){
    if((any((as.character((true_kingdom[i,1])))==input_kingdom))==FALSE){
      Kingdom_FN = Kingdom_FN + 1
      cat(paste('Kingdom', (true_kingdom[i,1]), 'in true list but not found - False Negative\n', sep='\t'))
    }
    i = i + 1
  }
}
#sensitivity
if((Kingdom_TP + Kingdom_FN)>0){
  kingdom_SSV <- (Kingdom_TP/(Kingdom_TP+Kingdom_FN))
} else {
  kingdom_SSV <- 0
}
#PPV
if((Kingdom_TP + Kingdom_FP)>0){
  kingdom_PPV <- (Kingdom_TP/(Kingdom_TP+Kingdom_FP))
} else {
  kingdom_PPV <- 0
}

#calculate distance to perfection from PPV/SSV roc curve #smaller the better
kingdom_distance <- sqrt((((1-kingdom_SSV)^2)+((1-kingdom_PPV)^2)))







#phylum calculations
Phylum_TP = 0
Phylum_TN = 0
Phylum_FN = 0
Phylum_FP = 0
i = 1

if((nrow(input_phylum))>0) {
  while(i<(nrow(input_phylum)+1)){
    if((any((as.character((input_phylum[i,1])))==true_phylum))==TRUE){
      Phylum_TP = Phylum_TP + 1
      cat(paste('Phylum',(input_phylum[i,1]), 'found in both lists - True Positive\n', sep='\t'))
    } else {
      Phylum_FP = Phylum_FP + 1
      cat(paste('Phylum',(input_phylum[i,1]), 'in input, but not found - False Positive\n', sep='\t'))
    }
    i = i + 1
  }
  
  i = 1 
  while(i<(nrow(true_phylum)+1)){
    if((any((as.character((true_phylum[i,1])))==input_phylum))==FALSE){
      Phylum_FN = Phylum_FN + 1
      cat(paste('Phylum',(true_phylum[i,1]), 'in true list but not found - False Negative\n', sep='\t'))
    }
    i = i + 1
  }
}
#sensitivity
if((Phylum_TP + Phylum_FN)>0){
  phylum_SSV <- (Phylum_TP/(Phylum_TP+Phylum_FN))
} else {
  phylum_SSV <- 0
}
#PPV
if((Phylum_TP + Phylum_FP)>0){
  phylum_PPV <- (Phylum_TP/(Phylum_TP+Phylum_FP))
} else {
  phylum_PPV <- 0
}

#calculate distance to perfection from PPV/SSV roc curve #smaller the better
phylum_distance <- sqrt((((1-phylum_SSV)^2)+((1-phylum_PPV)^2)))







#class calculations
Class_TP = 0
Class_TN = 0
Class_FN = 0
Class_FP = 0
i = 1

if((nrow(input_class))>0) {
  while(i<(nrow(input_class)+1)){
    if((any((as.character((input_class[i,1])))==true_class))==TRUE){
      Class_TP = Class_TP + 1
      cat(paste('Class', (input_class[i,1]), 'found in both lists - True Positive\n', sep='\t'))
    } else {
      Class_FP = Class_FP + 1
      cat(paste('Class',(input_class[i,1]), 'in input, but not found - False Positive\n', sep='\t'))
    }
    i = i + 1
  }
  
  i = 1 
  while(i<(nrow(true_class)+1)){
    if((any((as.character((true_class[i,1])))==input_class))==FALSE){
      Class_FN = Class_FN + 1
      cat(paste('Class',(true_class[i,1]), 'in true list but not found - False Negative\n', sep='\t'))
    }
    i = i + 1
  }
}
#sensitivity
if((Class_TP + Class_FN)>0){
  class_SSV <- (Class_TP/(Class_TP+Class_FN))
} else {
  class_SSV <- 0
}
#PPV
if((Class_TP + Class_FP)>0){
  class_PPV <- (Class_TP/(Class_TP+Class_FP))
} else {
  class_PPV <- 0
}

#calculate distance to perfection from PPV/SSV roc curve #smaller the better
class_distance <- sqrt((((1-class_SSV)^2)+((1-class_PPV)^2)))







#order calculations
Order_TP = 0
Order_TN = 0
Order_FN = 0
Order_FP = 0
i = 1

if((nrow(input_order))>0) {
  while(i<(nrow(input_order)+1)){
    if((any((as.character((input_order[i,1])))==true_order))==TRUE){
      Order_TP = Order_TP + 1
      cat(paste('Order',(input_order[i,1]), 'found in both lists - True Positive\n', sep='\t'))
    } else {
      Order_FP = Order_FP + 1
      cat(paste('Order',(input_order[i,1]), 'in input, but not found - False Positive\n', sep='\t'))
    }
    i = i + 1
  }
  
  i = 1 
  while(i<(nrow(true_order)+1)){
    if((any((as.character((true_order[i,1])))==input_order))==FALSE){
      Order_FN = Order_FN + 1
      cat(paste('Order',(true_order[i,1]), 'in true list but not found - False Negative\n', sep='\t'))
    }
    i = i + 1
  }
}
#sensitivity
if((Order_TP + Order_FN)>0){
  order_SSV <- (Order_TP/(Order_TP+Order_FN))
} else {
  order_SSV <- 0
}
#PPV
if((Order_TP + Order_FP)>0){
  order_PPV <- (Order_TP/(Order_TP+Order_FP))
} else {
  order_PPV <- 0
}

#calculate distance to perfection from PPV/SSV roc curve #smaller the better
order_distance <- sqrt((((1-order_SSV)^2)+((1-order_PPV)^2)))







#family calculations
Family_TP = 0
Family_TN = 0
Family_FN = 0
Family_FP = 0
i = 1

if((nrow(input_family))>0) {
  while(i<(nrow(input_family)+1)){
    if((any((as.character((input_family[i,1])))==true_family))==TRUE){
      Family_TP = Family_TP + 1
      cat(paste('Family', (input_family[i,1]), 'found in both lists - True Positive\n', sep='\t'))
    } else {
      Family_FP = Family_FP + 1
      cat(paste('Family',(input_family[i,1]), 'in input, but not found - False Positive\n', sep='\t'))
    }
    i = i + 1
  }
  
  i = 1 
  while(i<(nrow(true_family)+1)){
    if((any((as.character((true_family[i,1])))==input_family))==FALSE){
      Family_FN = Family_FN + 1
      cat(paste('Family', (true_family[i,1]), 'in true list but not found - False Negative\n', sep='\t'))
    }
    i = i + 1
  }
}
#sensitivity
if((Family_TP + Family_FN)>0){
  family_SSV <- (Family_TP/(Family_TP+Family_FN))
} else {
  family_SSV <- 0
}
#PPV
if((Family_TP + Family_FP)>0){
  family_PPV <- (Family_TP/(Family_TP+Family_FP))
} else {
  family_PPV <- 0
}

#calculate distance to perfection from PPV/SSV roc curve #smaller the better
family_distance <- sqrt((((1-family_SSV)^2)+((1-family_PPV)^2)))






#genus calculations
Genus_TP = 0
Genus_TN = 0
Genus_FN = 0
Genus_FP = 0
i = 1

if((nrow(input_genus))>0) {
  while(i<(nrow(input_genus)+1)){
    if((any((as.character((input_genus[i,1])))==true_genus))==TRUE){
      Genus_TP = Genus_TP + 1
      cat(paste('Genus',(input_genus[i,1]), 'found in both lists - True Positive\n', sep='\t'))
    } else {
      Genus_FP = Genus_FP + 1
      cat(paste('Genus',(input_genus[i,1]), 'in input, but not found - False Positive\n', sep='\t'))
    }
    i = i + 1
  }
  
  i = 1 
  while(i<(nrow(true_genus)+1)){
    if((any((as.character((true_genus[i,1])))==input_genus))==FALSE){
      Genus_FN = Genus_FN + 1
      cat(paste('Genus',(true_genus[i,1]), 'in true list but not found - False Negative\n', sep='\t'))
    }
    i = i + 1
  }
}
#sensitivity
if((Genus_TP + Genus_FN)>0){
  genus_SSV <- (Genus_TP/(Genus_TP+Genus_FN))
} else {
  genus_SSV <- 0
}
#PPV
if((Genus_TP + Genus_FP)>0){
  genus_PPV <- (Genus_TP/(Genus_TP+Genus_FP))
} else {
  genus_PPV <- 0
}

#calculate distance to perfection from PPV/SSV roc curve #smaller the better
genus_distance <- sqrt((((1-genus_SSV)^2)+((1-genus_PPV)^2)))




#species calculations
Species_TP = 0
Species_TN = 0
Species_FN = 0
Species_FP = 0
i = 1

#Declare a data frame for species that aren't found to go into - NCBI id matching will be tried later
failed_species <- data.frame(species=NA)


#Find true positives that are an easy string match at species level
if((nrow(input_species))>0) { #make sure the input length is actually greater than 0, or will cause a processing error
  while(i<(nrow(input_species)+1)){
    #cat(paste('looking up: ', (input_species[i,1])))
   if((any((as.character((input_species[i,1])))==true_species))==TRUE){
    Species_TP = Species_TP + 1
      cat(paste('Species',(input_species[i,1]), 'found in both lists - True Positive\n', sep='\t'))
    } else {
      failed_species <- rbind(failed_species, as.character(input_species[i,1])) #if the term didn't match append it to a list for further investigation later
    }
    i = i + 1
  }
}
     

# NCBI ID SEARCH - Last chance for some true positives 
#remove NA from failed_species if anything has been added to the failed_species dataframe
if((nrow(failed_species)>1)) {
    failed_species <- failed_species %>% filter(!is.na(species))
    #retrieve ncbi ID's from failed species
    i = 1
    while(i<(nrow(failed_species)+1)){ # Iterate through each failed species trying to find an NCBI ID match
      if(exists('temp') && is.data.frame(temp) ==TRUE){
        rm(temp) # remove any previous temp dataframe and reset if it already exists
      }
      spp <- as.character(failed_species[i,1]) # set species string
      temp <- input_comp %>% filter(Species==(spp)) %>% select('NCBI-TaxID') #search for NCBI ID
      if(nrow(temp) == 0){ #if no NCBI ID found - try some simple string manipulation and redo search
        if(exists('temp') && is.data.frame(temp) ==TRUE){
          rm(temp) # remove any previous temp dataframe and reset if it already exists
        }
        temp <- input_comp %>% filter(Species==(gsub(" ", "_", spp))) %>% select("NCBI-TaxID") #replace _ in string and find taxID, then search each found taxID in the TRUE comp file
      }
      
      #Perform NCBI search
      l = 1 #iterate through the matches (just in case there's more than one)
      while((l<(nrow(temp)+1)) && nrow(temp) != 0 ){
        tp_found = FALSE
        if(((any((as.character((temp[l,1])))==true_comp$`NCBI-TaxID`)))==TRUE){
          tp_found = TRUE
          cat(paste('Species',(input_species[i,1]), 'found in both lists -- based on NCBI-ID lookup - True Positive\n', sep='\t'))
        }
        l = l + 1
      }
      if(tp_found == TRUE) {
        Species_TP = Species_TP + 1
      } else {
          Species_FP = Species_FP + 1
          cat(paste('Species',(input_species[i,1]), 'in input, but not found from name or NCBI lookup - False Positive\n', sep='\t'))
        }
      i = i + 1
    }
}


i=1 # Reset counter for false Negative Searching
#Now do some searching for False Negatives -- no need to do NCBI match as it has already been done - performing the reverse would likely result in inaccurate false negative classifications
while(i<(nrow(true_species)+1)){
   if((any((as.character((true_species[i,1])))==input_species))==FALSE){
    Species_FN = Species_FN + 1
    cat(paste('Species', (true_species[i,1]), 'in true list but not found - False Negative\n', sep='\t'))
  }
  i = i + 1
}



#sensitivity
if((Species_TP + Species_FN)>0){
  species_SSV <- (Species_TP/(Species_TP+Species_FN))
} else {
  species_SSV <- 0
}
#PPV
if((Species_TP + Species_FP)>0){
  species_PPV <- (Species_TP/(Species_TP+Species_FP))
} else {
  species_PPV <- 0
}


#calculate distance to perfection from PPV/SSV roc curve #smaller the better
species_distance <- sqrt((((1-species_SSV)^2)+((1-species_PPV)^2)))

#calculate F1 scores -- F1 score = 2/((1/Recall)+(1/precision))where recall = sensitivity and precision = PPV
if((kingdom_SSV + kingdom_PPV)>0){
  kingdom_F1 <- (2/((1/kingdom_SSV)+(1/kingdom_PPV)))
} else {
  kingdom_F1 <- 0
}
if((phylum_SSV + phylum_PPV)>0){
  phylum_F1 <- (2/((1/phylum_SSV)+(1/phylum_PPV)))
} else {
  phylum_F1 <- 0
}
if((class_SSV + class_PPV)>0){
  class_F1 <- (2/((1/class_SSV)+(1/class_PPV)))
} else {
  class_F1 <- 0
}
if((order_SSV + order_PPV)>0){
  order_F1 <- (2/((1/order_SSV)+(1/order_PPV)))
} else {
  order_F1 <- 0
}
if((family_SSV + family_PPV)>0){
  family_F1 <- (2/((1/family_SSV)+(1/family_PPV)))
} else {
  family_F1 <- 0
}
if((genus_SSV + genus_PPV)>0){
  genus_F1 <- (2/((1/genus_SSV)+(1/genus_PPV)))
} else {
  genus_F1 <- 0
}
if((species_SSV + species_PPV)>0){
  species_F1 <- (2/((1/species_SSV)+(1/species_PPV)))
} else {
  species_F1 <- 0
}

#Write values to comp_performance_log

#check if file is created

#if not, make one

#append results to file






#declare results to dataframe
writetofile <- data.frame("Filename" = cmdargs[1], "Kingdom_SSV" = kingdom_SSV, "Kingdom_PPV" = kingdom_PPV,
                          "Phylum_SSV" = phylum_SSV, "Phylum_PPV" = phylum_PPV,
                          "Class_SSV" = class_SSV, "Class_PPV" = class_PPV,
                          "Order_SSV" = order_SSV, "Order_PPV" = order_PPV,
                          "Family_SSV" = family_SSV, "Family_PPV" = family_PPV,
                          "Genus_SSV" = genus_SSV, "Genus_PPV" = genus_PPV,
                          "Species_SSV" = species_SSV, "Species_PPV" = species_PPV,
                          "Kingdom_Dist" = kingdom_distance, "Phylum_Dist" = phylum_distance, "Class_Dist" = class_distance, "Order_Dist" = order_distance, "Family_Dist" = family_distance, "Genus_Dist" = genus_distance, "Species_Dist" = species_distance,
                          "Kingdom_TP" = Kingdom_TP, "Phylum_TP" = Phylum_TP, "Class_TP" = Class_TP, "Order_TP" = Order_TP, "Family_TP" = Family_TP, "Genus_TP" = Genus_TP, "Species_TP" = Species_TP,
                          "Kingdom_FP" = Kingdom_FP, "Phylum_FP" = Phylum_FP, "Class_FP" = Class_FP, "Order_FP" = Order_FP, "Family_FP" = Family_FP, "Genus_FP" = Genus_FP, "Species_FP" = Species_FP,
                          "Kingdom_FN" = Kingdom_FN, "Phylum_FN" = Phylum_FN, "Class_FN" = Class_FN, "Order_FN" = Order_FN, "Family_FN" = Family_FN, "Genus_FN" = Genus_FN, "Species_FN" = Species_FN,
                          "Kingdom_TN" = Kingdom_TN, "Phylum_TN" = Phylum_TN, "Class_TN" = Class_TN, "Order_TN" = Order_TN, "Family_TN" = Family_TN, "Genus_TN" = Genus_TN, "Species_TN" = Species_TN,
                          "Kingdom_F1" = kingdom_F1, "Phylum_F1" = phylum_F1, "Class_F1" = class_F1, "Order_F1" = order_F1, "Family_F1" = family_F1, "Genus_F1" = genus_F1, "Species_F1" = species_F1)


#Write to file if it exists, if not then make one
if (file.exists("performancelog")){
  oldlog <- read_tsv(file = "performancelog", col_names = TRUE) #if present read old log
  merge_old_new <- rbind(oldlog, writetofile) #add new rows to old performance file
  write.table(merge_old_new, file="performancelog", col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")#write merged to file
} else { 
  write.table(writetofile, file="performancelog", col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t") #write to file
  }



