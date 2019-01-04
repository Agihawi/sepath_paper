library(tidyverse)
library(ggpubr)
library(cowplot)


setwd('~/Desktop/tool_comparison/performancelogs')

raw <- read_tsv(file='spades_cov_raw', col_names=TRUE)
one <- read_tsv(file='spades_cov_1', col_names=TRUE)
two <- read_tsv(file='spades_cov_1_25', col_names=TRUE)
five <- read_tsv(file='spades_cov_1_5', col_names=TRUE)

raw <- raw %>% select(Genus_F1, Genus_SSV, Genus_PPV)
one <- one %>% select(Genus_F1, Genus_SSV, Genus_PPV)
two <- two %>% select(Genus_F1, Genus_SSV, Genus_PPV)
five <- five %>% select(Genus_F1, Genus_SSV, Genus_PPV)

raw_melt <- melt(raw)
one_melt <- melt(one)
two_melt <- melt(two)
five_melt <- melt(five)

raw_melt$Coverage_Filter <- '0'
one_melt$Coverage_Filter <- '1'
two_melt$Coverage_Filter <- '1.25'
five_melt$Coverage_Filter <- '1.5'

combined <- rbind(raw_melt, one_melt, two_melt, five_melt)

ggplot(combined, aes(x=variable, y=value, fill=Coverage_Filter)) +
  geom_violin(alpha=0.8) +
  theme_pubclean() +
  labs(title='Spades Contig Coverage Threshold vs Genus Performance',
       subtitle='n=99 simulated metagenomes') +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=12, face='bold'),
        legend.position='right',
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))
