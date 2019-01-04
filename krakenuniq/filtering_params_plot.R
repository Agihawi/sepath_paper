library(tidyverse)
library(ggpubr)
library(cowplot)

kraken002 <- read_tsv(file='~/Desktop/tool_comparison/kuniq/contig_classification/002.report', col_names=TRUE, skip=2)
kraken002 <- kraken001 %>% filter(rank=='genus')
true_genus <- c('Abiotrophia', 'Actinobaculum', 'Actinomyces', 'Actinotignum', 'Anaerococcus', 'Bacteroides', 'Blautia', 'Bulleidia', 'Campylobacter', 'Catonella', 'Corynebacterium', 'Cutibacterium', 'Dialister', 'Enterococcus', 'Fusobacterium', 'Gemella', 'Genus', 'Haemophilus', 'Helicobacter', 'Holdemania', 'Kingella', 'Leptotrichia', 'Listeria', 'Mannheimia', 'Mobiluncus', 'Moraxella', 'Mycoplasma', 'Neisseria', 'Pasteurella', 'Peptoniphilus', 'Porphyromonas', 'Prevotella', 'Propionibacteriaceae', 'Propionimicrobium', 'Rothia', 'Salmonella', 'Sneathia', 'Staphylococcus', 'Streptobacillus', 'Streptococcus', 'Treponema', 'Ureaplasma', 'Veillonella')

                
kraken002$found <- ifelse(kraken002$taxName %in% true_genus, 'Found', 'Not_Found')
                
                ggplot(kraken002, aes(log(reads), log(kmers), col=(log(cov)), shape=found, size=found)) +
                geom_point(alpha=0.8) +
                theme_pubclean() +
                scale_shape_manual(values=c(16,17),
                breaks=c('Found', 'Not_Found'),
                labels=c('True Positive', 'False Positive'),
                name='Detection Status')+
                scale_size_manual(values=c(8,2),
                breaks=c('Found', 'Not_Found'),
                labels=c('True Positive', 'False Positive'),
                name='Detection Status') +
                scale_colour_gradientn(colours = terrain.colors(10),
                name='log Coverage of Clade\nin Database') +
                labs(x='log Number of Taxon Reads',
                y='log number of Unique K-mers',
                title='Classification Status of Kraken Output Genera by Filtering Parameters',
                subtitle='Dataset 002 - 38/43 Genera Detected') +
                theme(legend.position='right',
                plot.title=element_text(hjust=0.5),
                plot.subtitle=element_text(hjust=0.5))
                
pdf('~/Desktop/sepath_paper/krakenuniq/kuniq_filtering_parameters_002.pdf', width=10, height=5)
ggdraw(kraken_plot)
dev.off()
