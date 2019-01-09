library(tidyverse)


#create dataframe of one dataset - append to in loop below
prefix_list <- c("002")
for (prefix in prefix_list) {
  #load kraken report, true report and find out if the taxa was found or not
  filename <- paste0('~/Desktop/tool_comparison/kuniq/contig_classification/', prefix, '.report')
  two <- read_tsv(file=filename, col_names=TRUE, skip=2)
  two <- two %>% filter(rank=='genus')
  true_filename <- paste0('~/Desktop/tool_comparison/true_comp/', prefix, '.comp')
  too_true <- read_tsv(file=true_filename, col_names=TRUE)
  true_genus <- unique(too_true$Genus)
  
  two <- two %>% mutate(found = case_when(
    taxName %in% true_genus ~ 'True_Positive',
    !(taxName %in% true_genus) ~ 'False_Positive'
  ))
  results <- two
  #merge this dataframe with one already created before moving on to the next file
}

# prefix_list <- c("002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020", "021", "022", "023", "024", "025", "026", "027", "028", "029", "030", "031", "032", "033", "034", "035", "036", "037", "038", "039", "040", "041", "043", "044", "045", "046", "047", "048", "049", "050", "051", "052", "053", "054", "055", "056", "057", "058", "059", "060", "061", "062", "063", "064", "065", "066", "067", "068", "069", "070", "071", "072", "073", "074", "075", "076", "077", "078", "079", "080", "081", "082", "083", "084", "085", "086", "087", "088", "089", "090", "091", "092", "093", "094", "095", "096", "097", "098", "099", "100")
# for (prefix in prefix_list) {
# #load kraken report, true report and find out if the taxa was found or not
#   filename <- paste0('~/Desktop/tool_comparison/kuniq/contig_classification/', prefix, '.report')
#   two <- read_tsv(file=filename, col_names=TRUE, skip=2)
#   two <- two %>% filter(rank=='genus')
#   true_filename <- paste0('~/Desktop/tool_comparison/true_comp/', prefix, '.comp')
#   too_true <- read_tsv(file=true_filename, col_names=TRUE)
#   true_genus <- unique(too_true$Genus)
# 
#   two <- two %>% mutate(found = case_when(
#     taxName %in% true_genus ~ 'True_Positive',
#     !(taxName %in% true_genus) ~ 'False_Positive'
#   ))
#   results <- rbind(results, two)
#   #merge this dataframe with one already created before moving on to the next file
# }

pdf('~/Desktop/SEPATH_Figures/kuniq_filtering_parameters_002.pdf', width=10, height=4)

ggplot(results, aes(x=log10(reads), y=log10(kmers), col=log10(cov), shape=found, size=found)) +
  geom_point(alpha=0.9) +
  theme_pubclean() +
  scale_shape_manual(values=c(17,16),
                     breaks=c('True_Positive', 'False_Positive'),
                     labels=c('True Positive', 'False Positive'),
                     name='Detection Status')+
  scale_size_manual(values=c(2,8),
                    breaks=c('True_Positive', 'False_Positive'),
                    labels=c('True Positive', 'False Positive'),
                    name='Detection Status') +
  scale_colour_gradientn(colours = terrain.colors(10),
                         name='log Coverage of Clade\nin Database') +
  labs(x='log Number of Taxon Reads',
       y='log number of Unique K-mers',
       title='Classification Status of Kraken Output Genera by Filtering Parameters',
       subtitle='Simulated Dataset 002 - 38/43 Genera Detected') +
  theme(legend.position='right',
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))

dev.off()
