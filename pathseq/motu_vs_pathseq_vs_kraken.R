#script to plot top performers
library(cowplot)
library(tidyverse)
library(ggpubr)
library(reshape2)

setwd('~/Desktop/tool_comparison/performancelogs/top_performers/Pathseq')

motu <- read_tsv(file='motus_unfilt_performance_pathseq.txt', col_names=TRUE)
pathseq <- read_tsv(file='pathseq_1f_performance', col_names=TRUE)
kraken <- read_tsv(file='spades_krak_unfilt_performance', col_names=TRUE)

motu <- motu %>% select(Genus_F1, Genus_PPV, Genus_SSV)
pathseq <- pathseq %>% select(Genus_F1, Genus_PPV, Genus_SSV)
kraken <- kraken %>% select(Genus_F1, Genus_PPV, Genus_SSV)


#melt variables
pathmelt <- melt(pathseq)
pathmelt$tool <- 'PathSeq'
motumelt <- melt(motu)
motumelt$tool <- 'mOTUs2'
kraken_melt <- melt(kraken)
kraken_melt$tool <- 'Kraken'

combi <- rbind(motumelt, pathmelt, kraken_melt)

#plot
ggplot(combi, aes(x=variable, y=value, fill=tool)) +
  geom_violin(alpha=0.8) +
  theme_pubclean() +
  scale_y_continuous(breaks=c(seq(0,1,0.1)),
                     limits=c(0,1)) +
  scale_fill_manual(name='Tool',
                    limits=c('mOTUs2', 'PathSeq', 'Kraken'),
                    values=c('darkgoldenrod2', 'tomato', 'chartreuse4'),
                    labels=c('mOTUs2', 'PathSeq', 'MetaSPAdes & Kraken')) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text(size=14, face='bold'),
          legend.position='right')


