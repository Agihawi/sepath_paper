library(tidyverse)
library(plyr)
library(cowplot)
library(ggExtra)

setwd('~/Desktop/simulation_summary')
#read in bacterial reads, human reads, species_counts

bacteria <- read_tsv(file='bacterial_reads.txt', col_names = FALSE)
colnames(bacteria) <- c('bacterial_reads')
human <- read_tsv(file='human_reads.txt', col_names = FALSE)
colnames(human) <- c('human_reads')
species <- read_tsv(file='species_counts.txt', col_names = FALSE)
colnames(species) <- c('number_of_species')

simulations <- data.frame(bacterial_reads = as.numeric(bacteria$bacterial_reads),
                     human_reads = as.numeric(human$human_reads),
                     number_of_species = as.numeric(species$number_of_species))


simulation_plot <- ggplot(data=simulations, aes(x=human_reads, y=bacterial_reads, color=number_of_species)) +
                          geom_point(alpha=0.8) +
  labs(title='Simulated Metagenomes Summary',
       x='Number of Human Reads',
       y='Number of Bacterial Reads',
       color='Number of Species') +
  theme(plot.title=element_text(size=16))



xdensity <- axis_canvas(simulation_plot, axis='x') +
  geom_density(data=simulations, aes(x=human_reads), alpha=0.7, fill='firebrick4')

ydensity <- axis_canvas(simulation_plot, axis='y', coord_flip=TRUE) +
  geom_density(data=simulations, aes(x=bacterial_reads), alpha=0.7, fill='firebrick4') +
  coord_flip()


plot1 <- insert_xaxis_grob(simulation_plot, xdensity, grid::unit(.2, 'null'), position='top')
plot2 <- insert_yaxis_grob(plot1, ydensity, grid::unit(.2, 'null'), position='right')

ggdraw(plot2)


pdf('simulated_dataset_summary.pdf', width=8, height=5)
ggdraw(plot2)
dev.off()
