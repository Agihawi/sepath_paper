#script to correlate motu relative abundance with true relative abundance from input list of true positive genus classifications
library(tidyverse)
library(plyr)


#load input list of true positives

list_of_nums <- c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020", "021", "022", "023", "024", "025", "026", "027", "028", "029", "030", "031", "032", "033", "034", "035", "036", "037", "038", "039", "040", "041", "042", "043", "044", "045", "046", "047", "048", "049", "050", "051", "052", "053", "054", "055", "056", "057", "058", "059", "060", "061", "062", "063", "064", "065", "066", "067", "068", "069", "070", "071", "072", "073", "074", "075", "076", "077", "078", "079", "080", "081", "082", "083", "084", "085", "086", "087", "088", "089", "090", "091", "092", "093", "094", "095", "096", "097", "098", "099", "100")


for(filename in list_of_nums) {
  input_file <- paste0('~/Desktop/tool_comparison/motu/genus_output/true_positives/', filename, '_tp')
  input_tp <- read_tsv(file=input_file, col_names = FALSE)
  input_tp <- as.list(input_tp$X1)

  #load true comp, select only relative abundance and genus
  input_comp_file <- paste0('~/Desktop/tool_comparison/true_comp/', filename, '.comp')
  input_comp <- read_tsv(file=input_comp_file, col_names=TRUE)
  input_comp <- input_comp %>% select(Genus, Relative_Abundance)

  #load up motu results and filter for only true positive's
  motu_file <- paste0('~/Desktop/tool_comparison/motu/genus_output/', filename, '_genus.motus')
  motu <- read_tsv(file=motu_file, col_names=FALSE)
  motu <- motu %>% filter(X1 %in% input_tp)

  #filter out input comp by genus that are present in true_positives
  input_comp <- input_comp %>% filter(Genus %in% input_tp)
  #search for duplicate genera and add relative abundances together
  input_comp <- ddply(input_comp, 'Genus', numcolwise(sum))

  #marry up input_comp and motu so output can be correlated
  colnames(input_comp) <- c('Genus', 'True_Abundance')
  colnames(motu) <- c('Genus', 'Estimated_Abundance')

  #make all integers
  input_comp <- input_comp %>% mutate(True_Abundance = as.numeric(True_Abundance))
  motu <- motu %>% mutate(Estimated_Abundance = as.numeric(Estimated_Abundance))

  merged <- merge(input_comp, motu, by='Genus', all=TRUE)

  write_file <- paste0('~/Desktop/tool_comparison/motu/genus_output/merged_TP_abundances/', filename, '_abundances')
  write.table(merged, file=write_file, row.names=FALSE, col.names=FALSE, quote=FALSE, sep='\t')
}



#merge all results via command line
system('cat ~/Desktop/tool_comparison/motu/genus_output/merged_TP_abundances/* > ~/Desktop/tool_comparison/motu/genus_output/merged_TP_abundances/all_merged_abundances')

#load in merged abundances for all 100 motus results
abundances <- read_tsv(file='~/Desktop/tool_comparison/motu/genus_output/merged_TP_abundances/all_merged_abundances', col_names=FALSE)
colnames(abundances) <- colnames(merged)


setwd('/Users/agihawi/Desktop/SEPATH_Figures/')
pdf('motu_abundance_correlation.pdf', width=6, height=6)
#do some plots
ggplot(abundances, aes(x=True_Abundance, y=Estimated_Abundance)) +
  geom_point(alpha=0.9, col='darkgoldenrod2') +
  geom_smooth(method='lm', col='navy') +
  theme_pubclean() +
  labs(title='mOTUs2 Estimated vs True Abundance',
       subtitle='2,084 True Positive Results for 100 simulated metagenomes',
       x='True Relative Abundance',
       y='Estimated Relative Abundance') +
  theme(plot.title=element_text(size=12, hjust=0.5, face='bold'),
        plot.subtitle=element_text(hjust=0.5), 
        panel.grid.major = element_line(colour='grey', size=0.2),
        axis.text = element_text(size=12),
        axis.title=element_text(size=12, face='bold')
        ) +
  scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
  stat_cor(method='spearman', label.x=0.6, label.y=0.03, col='navy', size=5)

dev.off()
