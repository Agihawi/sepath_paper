#!/usr/bin/env python
# This script will filter contigs by a selected minimum level of coverage --- Contigs must be fasta format and produced by metaSPAdes --- # call script.py inputfasta

from Bio import SeqIO
import sys

input_file = sys.argv[-1]

#set minimum coverage here
min_cov = 1.5

#generate output filename
output_file = (input_file.split('.fa')[0])
out_file = ('%s_filt.fasta' %output_file)

#Generate sequence records only if the coverage is greater than or equal to the coverage of the contig - then write to output file
filtered = (rec for rec in SeqIO.parse(input_file, "fasta") if ( (float(((rec.id).split('_'))[-1])) >= min_cov) )
SeqIO.write(filtered, out_file, "fasta")



