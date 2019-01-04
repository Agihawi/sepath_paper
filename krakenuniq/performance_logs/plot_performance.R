library(tidyverse)
library(ggpubr)
library(reshape2)

spades_krak <- read_tsv(file='~/Desktop/tool_comparison/performancelogs/kuniq_contig_unfiltered', col_names=TRUE)
spades_g_filt <- spades_krak %>% select(Genus_F1, Genus_PPV, Genus_SSV)
spades_melt <- melt(spades_g_filt)

ggplot(data=spades_melt, aes(x=variable, y=value)) +
  geom_violin(alpha=0.7, fill='darkolivegreen') +
  theme_pubclean() +
  labs(title='Unfiltered KrakenUniq on MetaSPAdes Contigs') +
  theme(axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=15),
        plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,1,0.1))
